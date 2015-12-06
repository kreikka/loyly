{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
#if !DEVELOPMENT
{-# OPTIONS_GHC -fno-warn-unused-matches #-} -- RecordWildCards in widgets
#endif
module Handler.Home where

import           Import
import           RemindHelpers
import qualified Data.ByteString.Builder as B
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Char (toLower, isSpace, isPunctuation)
import qualified Data.List as L
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Time

---------------------------------------------------------------
import           System.FilePath ((</>), (<.>))
import qualified System.FilePath as F
import qualified System.Directory
import qualified System.Process as P
import           System.Exit (ExitCode(..))
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.UTF8 as Pandoc (toStringLazy) -- strips BOM
import           Text.Printf
import           Database.Persist.Sql (fromSqlKey)
import           Yesod.Markdown
import           Yesod.Auth.Account (setPasswordR, newPasswordForm, resetPasswordR)
import qualified Yesod.Auth.Message as Msg
import           Yesod.Auth (getAuthEntity)

import Permissions

homeTitle :: MonadWidget m => m ()
homeTitle = setTitle "Akateeminen saunakerho Löyly ry"

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        homeTitle
        $(widgetFile "homepage")

getMembersR, postMembersR :: Handler Html
getMembersR  = postMembersR
postMembersR = do
    form@((result, _), _) <- runFormPost . memberForm =<< lookupGetParam "email"

    case result of
        FormSuccess res -> do
            _ <- runDB $ insert res
            setMessage . isSuccess . toHtml $ "Tervetuloa saunomaan, " <> memberName res <> "!"
            redirect MembersR
        _ -> return ()

    maid <- maybeAuthId
    users <- runDB $ selectList [] [Asc UserUsername]

    defaultLayout $ do
        setTitle "Jäsenet ja liittyminen"
        $(widgetFile "members")

getAssociationR :: Handler Html
getAssociationR = do
    defaultLayout $ do
        setTitle "Yhdistys"
        $(widgetFile "association")

getHelpR :: Handler Html
getHelpR = defaultLayout $(widgetFile "help")

getActivitiesR :: Handler Html
getActivitiesR = do
    defaultLayout $ do
        setTitle "Toiminta"
        $(widgetFile "activities")

getPrivacyPolicyR :: Handler Html
getPrivacyPolicyR =
    defaultLayout $ do
        setTitle "Tietosuojaseloste"
        $(widgetFile "privacy-policy")

memberForm :: Maybe Text -> Form Member
memberForm memail = renderBootstrap2 $ Member False
    <$> areq textField "Koko nimi" Nothing
    <*> areq textField "Kotikunta" Nothing
    <*> areq (checkM mailNotTaken emailField) "Sähköposti" memail
    <*> areq checkBoxField "Olen HYY:n jäsen" Nothing
    <*> aopt dayField "Syntymäaika (ei pakollinen)" Nothing
    <*> areq (radioFieldList genderSelections) "Sukupuoli" Nothing
    <*> areq textField "Miten sait tietää meistä?" Nothing
    <*> (fmap (fmap unTextarea)) (aopt textareaField "Löylyprofiilisi" Nothing)
    <*> lift (liftIO getCurrentTime)
  where
      genderSelections :: [(Text, Maybe Bool)]
      genderSelections = [("Mies", Just True), ("Nainen", Just False), ("Muu", Nothing)]

      mailNotTaken :: Text -> Handler (Either Text Text)
      mailNotTaken mail =
          maybe (Right mail) (\_ -> Left ("Sähköpostillasi on jo liitytty" :: Text))
          <$> (runDB $ getBy $ UniqueMember mail)

memberStatistics :: Widget
memberStatistics = do
    membs <- handlerToWidget $ runDB $ selectList [] []

    let membs'            = map entityVal membs
        membsAppr         = filter memberApproved membs'
        membsNotAppr      = filter (not . memberApproved) membs'
        total             = length membsAppr
        hyy               = length $ filter memberHyyMember membsAppr
        hyyP | total == 0 = 0
             | otherwise  = floor (100 * fromIntegral hyy / fromIntegral total :: Double) :: Int

    [whamlet|
Jäseniä #{total}, joista HYYläisiä #{hyy} (#{hyyP}%).
$if not $ null membsNotAppr
    \ Lisäksi jäseneksi hyväksymistä odottaa #{length membsNotAppr} hakemusta.
|]

-- * Blog

getBlogR, postBlogR :: Handler Html
getBlogR = postBlogR
postBlogR = do
    maid <- maybeAuthId
    form@((res, _), _) <- runFormPost $ blogPostForm maid

    case res of
        FormSuccess r -> handleBlogPost r
        _             -> return ()

    defaultLayout $ do
        setTitle "Saunablogi"
        $(widgetFile "blog-home")

blogPostForm :: Maybe Text -> Form (FileInfo, Bool, Text)
blogPostForm muser = renderBootstrap2 $ (,,)
    <$> fileAFormReq "Markdown-tiedosto"
    <*> areq checkBoxField "Päivitän postausta" Nothing
    <*> maybe (areq textField "Kuka olet" Nothing
            <* areq (checkM blogPass passwordField) "Salasana" Nothing)
            (\u -> areq (checkM (\_ -> confirmedUser u) hiddenField) "" (Just "a")) muser
    where
        confirmedUser, blogPass :: Text -> Handler (Either Text Text)
        blogPass got = do correct <- (== got) . extraBlogPass <$> getExtra
                          return $ if correct then Right got else Left "Väärä salasana"

        confirmedUser name = do
            m <- runDB $ getBy404 (UniqueUsername name) >>= getBy404 . UniqueMember . userEmail . entityVal
            return $ if not $ memberApproved $ entityVal m
                then Left "Hakemustasi ei ole vielä hyväksytty. Vain varsinaiset jäsenet voivat päivittää blogia."
                else Right name


getBlogPostR :: Text -> Handler Html
getBlogPostR ident = do
    Entity _ BlogPost{..} <- runDB $ getBy404 $ UniqueBlogPost ident
    defaultLayout $ do
        setTitle $ toHtml blogPostTitle
        $(widgetFile "blog-post")

handleBlogPost :: (FileInfo, Bool, Text) -> Handler ()
handleBlogPost (fi, overwrite, editor) = do
    time <- liftIO getCurrentTime
    bs <- fmap B.toLazyByteString $ fileSource fi $$ CL.foldMap B.byteString

    let md_str = Pandoc.toStringLazy bs -- strips BOM if present
        md     = Markdown (T.pack md_str)

    let readerOptions = yesodDefaultReaderOptions
            { Pandoc.readerExtensions = Pandoc.pandocExtensions }
        writerOptions = yesodDefaultWriterOptions
            { Pandoc.writerExtensions = Pandoc.pandocExtensions }

        Right doc@(Pandoc.Pandoc meta _) = Pandoc.readMarkdown readerOptions md_str -- TODO handle left case, it indicates a parse error
        title = T.pack $ concatMap inlineToString (Pandoc.docTitle meta)

        post = BlogPost
            { blogPostIdent    = toUrlIdent title
            , blogPostTitle    = title
            , blogPostAuthors  = map (T.pack . concatMap inlineToString) (Pandoc.docAuthors meta)
            , blogPostTags     = getTags meta
            , blogPostLog      = [(time, editor)]
            , blogPostMarkdown = md
            , blogPostRendered = writePandocTrusted writerOptions doc
            }

    if T.null (blogPostIdent post)
        then setMessage $ isError "Virhe: tiedoston title-metakentän on oltava epätyhjä."
        else saveBlogPost overwrite post

saveBlogPost :: Bool -> BlogPost -> Handler ()
saveBlogPost overwrite p = do
    mp <- runDB $ getByValue p
    case mp of
        Nothing                       -> void . runDB $ insert p
        Just (Entity k v) | overwrite -> void . runDB $ replace k p { blogPostLog = blogPostLog p ++ blogPostLog v }
                          | otherwise -> return ()
    setMessage $ isSuccess $ maybe "Postaus lisätty."
        (\p -> toHtml $ "Postaus `" <> blogPostTitle (entityVal p) <> "' päivitetty.") mp
    redirect $ BlogPostR (blogPostIdent p)

-- * Profile

type AckResult = [(PersonInImageId, Maybe Bool)]

getProfileR :: Handler Html
getProfileR = do
    u <- requireAuthId
    Entity uid user <- runDB $ getBy404 $ UniqueUsername u
    mmemb <- runDB $ getBy $ UniqueMember $ userEmail user
    passForm <- runFormPost $ renderBootstrap2 $ newPasswordForm u (userResetPasswordKey user)
    form@(_, ackImages) <- runImagePublishAckForm uid
    defaultLayout $ do
        setTitle "Löylyprofiili"
        $(widgetFile "profile")

getPublicProfileR :: Text -> Handler Html
getPublicProfileR u = do
    Entity _ user <- runDB $ getBy404 $ UniqueUsername u
    Entity _ memb <- runDB $ getBy404 $ UniqueMember $ userEmail user
    defaultLayout $ do
        setTitle $ toHtml $ "Löylyprofiili: " <> userUsername user
        $(widgetFile "profile-public")


-- * Gallery

-- ** Handlers

-- | Some default view; recent imgs or smth
getGalleryR, postGalleryR :: Handler Html
getGalleryR  = postGalleryR
postGalleryR = do
    albumFilters <- albumAccessFilters
    imageFilters <- imageAccessFilters
    albums       <- runDB $ selectList albumFilters [Desc AlbumDate]
    images       <- runDB $ selectList imageFilters [Desc ImageDate, LimitTo 8] -- a few most resent images
    numImages    <- runDB $ count ([] :: [Filter Image])
    numPrivate   <- runDB $ count [ImageAccess ==. readableMembers]

    numNotAcked  <- runDB $ do
        notAcked <- selectList [PersonInImageAcked ==. Nothing] [Asc PersonInImageImg]
        return $ length $ L.nub $ map (personInImageImg . entityVal) notAcked

    defaultLayout $ do
        setTitle "Galleria"
        $(widgetFile "gallery-home")

-- | Previews of all images in the album.
getAlbumR, postAlbumR :: Text -> Text -> Handler Html
getAlbumR               = postAlbumR
postAlbumR author ident = do
    album@(Entity aid Album{..}) <- runDB $ getBy404 $ UniqueAlbum author ident

    authorized <- permissionsOrAuthored albumAccess albumAuthor
    unless authorized $ permissionDenied "Not authorized"

    filters <- imageAccessFilters
    images <- runDB $ selectList ((ImageAlbum ==. aid) : filters) [Asc ImageNth]

    defaultLayout $ do
        setTitle $ toHtml albumTitle
        $(widgetFile "gallery-album")

-- | HTML view of an album image.
getImageViewR, postImageViewR :: Text -> Text -> Int -> Handler Html
postImageViewR                 = getImageViewR
getImageViewR author ident nth = do
    Just route <- getCurrentRoute
    maid       <- maybeAuthId

    img@(Entity aid Album{..}, Entity iid Image{..}, people) <- getImage author ident nth
    imageCount <- runDB $ count [ImageAlbum ==. aid]

    authorized <- permissionsOrAuthoredOrAckPending img
    unless authorized $ permissionDenied "Not authorized"

    -- List of everyone, used in the ack select list
    allPeople <- map (liftA2 (,) (userUsername . entityVal) entityKey) <$> runDB (selectList [] [Asc UserUsername])

    -- handle image edit form
    form@((res,_),_) <- runFormPost $ renderBootstrap2 $ imageEditForm allPeople img
    case res of
        FormSuccess (newPeople, newDesc) -> do
            runDB $ do
                update iid [ImageDesc =. fromMaybe "" newDesc]
                -- delete untagged people
                deleteWhere [PersonInImageImg ==. iid, PersonInImageUser /<-. newPeople]
                -- add newly tagged people
                mapM_ (\u -> insertUnique $ PersonInImage iid u Nothing) newPeople
                updateAckInfo iid
            redirect route
        _ -> return ()

    defaultLayout $ do
        setTitle $ "Kuva " <> toHtml imageNth <> " [" <> toHtml albumAuthor <> ", " <> toHtml albumTitle <> "]"
        $(widgetFile "gallery-image")

-- *** Sending images

-- | Send raw image (or thumbnail) based on a @author/album/nth@ URL.
getImageR, getThumbR :: Text -> Text -> Int -> Handler ()
getImageR x y z = sendImage False =<< getImage x y z
getThumbR x y z = sendImage True  =<< getImage x y z

-- | Send raw image (or thumbnail) based on an ImageId.
getImageByIdR, getThumbByIdR :: ImageId -> Handler ()
getImageByIdR = getImageById >=> sendImage False
getThumbByIdR = getImageById >=> sendImage True

-- | Send an image (or thumbnail) based on "ImageInfo".
sendImage :: Bool -> ImageInfo -> Handler ()
{-# INLINE sendImage #-}
sendImage thumb img@(Entity aid Album{..}, Entity _ Image{..}, _) = do

    authored <- permissionsOrAuthoredOrAckPending img
    unless authored $ permissionDenied "Not authorized"

    root <- extraGalleryRoot <$> getExtra

    let fileName | thumb     = thumbDir </> imageFile <.> ".jpg"
                 | otherwise = imageFile <.> imageFileExt
        imagePath            = root </> show (fromSqlKey aid) </> fileName
        imageType            = if thumb then typeJpeg else imageContentType

    sendFile imageType imagePath

-- ** Forms etc.

-- | This form allows editing tagged people and the description field.
imageEditForm :: RenderMessage App msg
              => [(msg, UserId)] -- ^ Everyone registered
              -> ImageInfo
              -> AForm Handler ([UserId], Maybe Text)
imageEditForm allPeople (_,Entity _ img, people) = (,)
    <$> (fmap (fromMaybe []) $ aopt (multiSelectFieldList allPeople) "Ihmiset kuvassa" (Just . Just $ nowPeople))
    <*> (fmap (fmap unTextarea) $ aopt textareaField "Meta" (Just . Just . Textarea $ imageDesc img))
    where nowPeople = entityKey . snd <$> people

-- | Upload a new album.
uploadWidget :: Maybe (Entity Album) -> Widget
uploadWidget malbum = do
    maid <- handlerToWidget maybeAuthId

    ((result, widget), _) <- handlerToWidget $ runFormPost $ renderBootstrap2 $ (,,,)
        <$> areq textField "Albumin otsikko" (albumTitle . entityVal <$> malbum)
        <*> maybe (areq textField "Kuka olet" Nothing) pure maid
        <*> areq permissionsField "Albumi on julkinen" (albumAccess . entityVal <$> malbum)
        <*> maybe (areq passwordField "Salasana" Nothing)
                  (const $ lift $ extraBlogPass <$> getExtra) maid

    case result of
        FormSuccess (title, author, public, pass) -> do

            -- check pass and album uniqueness
            handlerToWidget $ do
                pass' <- extraBlogPass <$> getExtra
                when (pass /= pass') $ do setMessage $ isError "Virhe: Väärä salasana."
                                          redirect GalleryR
                when (isNothing malbum) $ do
                    indb <- runDB $ getBy $ UniqueAlbum author title
                    when (isJust indb) $ invalidArgs ["Sinulla on jo tämänniminen albumi."]

            -- insert or update album and files
            Entity aid Album{..} <- handlerToWidget $ createOrUpdateAlbum author malbum title public
            nfiles <- lookupFiles "files" >>= handlerToWidget . saveFiles author aid

            setMessage . isSuccess . toHtml $ show nfiles ++ " kuvaa lisätty."
            redirect $ AlbumR albumAuthor albumIdent

        _ -> renderForm msg
                ((result, widget >> filesWidget), Multipart)
                (maybe GalleryR (AlbumR <$> albumAuthor . entityVal <*> albumIdent . entityVal) malbum)
                (submitI msg)
  where
    msg = case malbum of
              Nothing -> MsgCreateNewAlbum
              Just _  -> MsgUpdateAlbum
    filesWidget = [whamlet|
<div.control-group.clearfix required>
    <label.control-label for=files>Kuvatiedostot
    <div.controls input>
        <input#files name=files type=file multiple>
|]

-- NOTE don't use @albumTitle@ returned here, it is not updated
createOrUpdateAlbum :: Text -> Maybe (Entity Album) -> Text -> Permissions -> Handler (Entity Album)
createOrUpdateAlbum _author (Just (Entity aid album)) title access = do
    runDB $ update aid [AlbumTitle =. title, AlbumAccess =. access]
    return $ Entity aid album

createOrUpdateAlbum author Nothing title access = do
    time      <- liftIO getCurrentTime
    let album = Album access time (toUrlIdent title) title author
    aid       <- runDB $ insert album
    return $ Entity aid album

-- ** Files and thumbs (in fs)

-- | Save files. Return number of new images.
saveFiles :: Text -> AlbumId -> [FileInfo] -> Handler Int
saveFiles author aid fs = do
    time  <- liftIO getCurrentTime
    album <- runDB $ selectFirst [ImageAlbum ==. aid] [Desc ImageNth]
    root  <- extraGalleryRoot <$> getExtra

    let albumRoot = root </> show (fromSqlKey aid)
        imageN    = maybe 0 (fromIntegral . fromSqlKey . entityKey) album

    liftIO $ System.Directory.createDirectoryIfMissing True (albumRoot </> thumbDir)

    names <- forM (zip fs [imageN + 1 ..]) $ \(fi, nth) -> do

        let (base, ext) = (,) <$> F.takeBaseName <*> F.takeExtension $ (T.unpack $ fileName fi)
            file        = printf "%03d" nth
            contentType = simpleContentType $ encodeUtf8 $ fileContentType fi

        liftIO $ fileMove fi (albumRoot </> file <.> ext)
        _ <- runDB $ insert $ Image readableNone "" time aid nth (T.pack base) author contentType file ext
        return (file <.> ext)

    code <- liftIO $ generateThumbnails albumRoot thumbDir names
    when (code /= ExitSuccess) $ do
        setMessage $ isError "Virhe: kuvat lisättiin, mutta peukalonkynsien generointi epäonnistui!"
        redirect HomeR

    return $ length fs

-- | @generateThumbnails albumRoot thumbs_relative names@
generateThumbnails :: FilePath -> FilePath -> [FilePath] -> IO ExitCode
generateThumbnails albumRoot thumbs names = do
    (_,_,_,ph) <- P.createProcess
        (P.proc "mogrify" $ ["-format", "jpg", "-thumbnail", "270x270", "-path", thumbs] ++ names)
        { P.cwd = Just albumRoot }
    P.waitForProcess ph

-- ** Render image view

flexImagesWidget :: Text -> Text -> [Entity Image] -> Widget
flexImagesWidget author ident images = [whamlet|
<div .images>
  $forall Entity _ Image{..} <- images
    <a href=@{ImageViewR author ident imageNth}>
      <img src=@{ThumbR author ident imageNth} alt=#{imageTitle}>
|]

-- TODO: this does naive @mapM (get404 ...)@; should use a join instead
-- (esqueleto).
flexImagesWidget' :: [Entity Image] -> Widget
flexImagesWidget' images = do
    albums <- handlerToWidget $ runDB $ mapM (get404 . imageAlbum . entityVal) images
    [whamlet|
<div .images>
  $forall (Entity _ Image{..}, Album{..}) <- zip images albums
    <a href=@{ImageViewR albumAuthor albumIdent imageNth}>
      <img src=@{ThumbR albumAuthor albumIdent imageNth} alt=#{imageTitle}>
|]

-- ** ImageInfo

type ImageInfo = (Entity Album, Entity Image, [(PersonInImage, Entity User)])

-- | Get an image's info by its unique path: "album_author/album_name/image_number"
getImage :: Text -> Text -> Int -> Handler ImageInfo
getImage author ident nth = runDB $ do
    -- XXX: Could use a join here too
    album  <- getBy404 $ UniqueAlbum author ident
    image  <- getBy404 $ UniqueImage (entityKey album) nth
    people <- getImagePeople $ entityKey image
    return (album, image, people)

getImageById :: ImageId -> Handler ImageInfo
getImageById iid = runDB $ do
    image  <- get404 iid
    album  <- get404 (imageAlbum image)
    people <- getImagePeople iid
    return (Entity (imageAlbum image) album, Entity iid image, people)

getImagePeople :: ImageId -> Query [(PersonInImage, Entity User)]
getImagePeople iid = do
    piis <- selectList [PersonInImageImg ==. iid] []
    let people = map entityVal piis
    zip people <$> selectList [UserId <-. map personInImageUser people] [Asc UserUsername]

-- ** Access control

permissionsField = radioFieldList getOpts 
    where getOpts = [("Näkyy kaikille" :: Text, readableAll)
                    ,("Näkyy vain jäsenille", readableMembers)]

-- | Also unpublishes if personInImages was updated.
updateAckInfo :: ImageId -> Query ()
updateAckInfo iid = do
    piis <- selectList [PersonInImageImg ==. iid] []

    let acks      = map (personInImageAcked . entityVal) piis
        allPublic = all (== Just 2) acks
        noPublish = any (== Just 0) acks
        state | noPublish = readableMembers
              | allPublic = readableAll
              | otherwise = readableNone
    update iid [ImageAccess =. state]

    -- update album

    img <- get404 iid
    albumImages <- selectList [ImageAlbum ==. imageAlbum img] []

    let perms      = map (imageAccess . entityVal) albumImages
        hasPublic  = any isReadableAll perms
        hasPrivate = any isReadableMembers perms
        albumState | hasPublic  = readableAll
                   | hasPrivate = readableMembers
                   | otherwise  = readableNone
    update (imageAlbum img) [AlbumAccess =. albumState]

type AckForm = (((FormResult [(PersonInImageId, Maybe Int)], Widget), Enctype), [Entity PersonInImage])

-- | The result type looks like a monster, but actually it's just
-- @(form_result, personInImages)@.
runImagePublishAckForm :: UserId -> Handler AckForm
runImagePublishAckForm uid = do
    acks <- runDB $ selectList [ PersonInImageUser ==. uid, PersonInImageAcked ==. Nothing]
                               [ Asc PersonInImageImg ]
    form <- runFormPost $ renderBootstrap2 $ sequenceA $ map ackForm acks
    return (form, acks)
  where
    ackForm (Entity k v) = (k, ) <$> aopt (ackField v) "" Nothing
    updatePublishable k status = update k [PersonInImageAcked =. status]

postImagePublishAckR :: Handler Html
postImagePublishAckR = do
    form@(((res,_),_),piis) <- runImagePublishAckForm . entityKey =<< runDB . getBy404 . UniqueUsername =<< requireAuthId
    case res of
        FormSuccess xs -> do runDB $ mapM_ (uncurry updatePublishable) xs
                             runDB $ mapM_ (updateAckInfo . personInImageImg . entityVal) piis
                             setMessage $ isSuccess "Julkaisuoikeudet asetettu."
                             redirect ProfileR
        _ -> do
            setMessage $ isError "Virhe: päivitys epäonnistui, tarkista lomake."
            defaultLayout $ do
                setTitle "Julkaisuoikeuksien päivitys."
                ackImageForm form
  where
    updatePublishable k status | isJust status = update k [PersonInImageAcked =. status]
                               | otherwise     = return ()

ackImageForm :: AckForm -> Widget
ackImageForm (((_, ackWidget), enctype), _) = $(widgetFile "ack-image-form")

ackField :: PersonInImage -> Field Handler Int
ackField v = ackOptions { fieldView = \theId name attrs val isReq -> [whamlet|
<div.clearfix.ack>
  <img style="float:left" src=@{ThumbByIdR $ personInImageImg v}>
  <div .controls .clearfix>^{fieldView ackOptions theId name attrs val isReq}
|] }
    where
        ackOptions :: Field Handler Int
        ackOptions = selectFieldList [("Ei julkaisuoikeutta" :: Text, 0)
                                     ,("Vain jäsenille", 1)
                                     ,("Kaikille", 2)]

imageAccessFilters :: Handler [Filter Image]
imageAccessFilters = do
    maid <- maybeAuthId
    return $ case maid of
          Just aid -> [ImageAccess >. readableNone] ||. [ImageAuthor ==. aid]
          Nothing  -> [ImageAccess >. readableNone]

albumAccessFilters :: Handler [Filter Album]
albumAccessFilters = do
    maid <- maybeAuthId
    return $ case maid of
          Just aid -> [AlbumAccess >. readableNone] ||. [AlbumAuthor ==. aid]
          Nothing  -> [AlbumAccess >. readableNone]

permissionsOrAuthored :: Permissions -> Text -> Handler Bool
permissionsOrAuthored p t | isReadableAll p   = return True
                          | p /= readableNone = isJust <$> maybeAuthId
                          | otherwise         = (t ==) <$> requireAuthId

-- | image public enough, author of it or on the ack list
permissionsOrAuthoredOrAckPending :: ImageInfo -> Handler Bool
permissionsOrAuthoredOrAckPending (_, i, _) = do
    authored <- permissionsOrAuthored (imageAccess $ entityVal i) (imageAuthor $ entityVal i)
    if authored then return True else do
        aid <- requireAuthId
        uid <- entityKey . fromJust <$> getAuthEntity aid
        ack <- runDB $ getBy $ UniquePersonImage (entityKey i) uid
        return (isJust ack)

-- * Misc.

-- | Relative to *album root*.
thumbDir :: FilePath
thumbDir = "thumbs"

toImageFile :: Int -> String
toImageFile = printf "%03d"

-- | space -> '-', punctuation removed
toUrlIdent :: Text -> Text
toUrlIdent = T.map f . T.unwords . T.words . T.filter (not . isPunctuation)
  where
    f c | isSpace c = '-'
        | otherwise = toLower c

prettyPublicity :: Permissions -> Text
prettyPublicity p | isReadableAll p     = "julkinen"
                  | isReadableMembers p = "vain jäsenille"
                  | otherwise           = "piilotettu"

-- ** Pandoc-specific

inlineToString :: Pandoc.Inline -> String
inlineToString x = case x of
    Pandoc.Str s    -> s
    Pandoc.Space    -> " "
    Pandoc.Code _ s -> s
    _               -> ""

getTags :: Pandoc.Meta -> [Text]
getTags = maybe (error "tags-kenttää ei löytynyt") go . Pandoc.lookupMeta "tags"
  where
    go (Pandoc.MetaInlines xs) = filter (not . T.null) . T.split (==',') . T.pack $ concatMap inlineToString xs
    go x                       = error ("tags-kentäksi odotettiin MetaInlines, mutta tuli: " ++ show x)

unMetaText :: Pandoc.MetaValue -> Maybe Text
unMetaText (Pandoc.MetaString s)   = Just $ T.pack s
unMetaText (Pandoc.MetaInlines xs) = Just $ T.pack $ concatMap inlineToString xs
unMetaText _                       = Nothing

-- | Like requireAuthId but to UserId
requireUserId :: Handler UserId
requireUserId = requireAuthId >>= fmap entityKey . runDB . getBy404 . UniqueUsername

-- | Like requireAuthId but to UserId
maybeUserId :: Handler (Maybe UserId)
maybeUserId = maybeAuthId >>= maybe (return Nothing) (fmap (fmap entityKey) . runDB . getBy . UniqueUsername)

-- * Calendar

-- ** Views

-- | Combined calendar view
getCalendarR :: Handler Html
getCalendarR = do
    muid       <- maybeUserId
    allCurrent <- remindAsciiCalendar -- TODO allCurrent should be cached
    cals       <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Tapahtumakalenteri"
        $(widgetFile "calendar-home")

getCalendarCreateR, postCalendarCreateR :: Handler Html
getCalendarCreateR  = postCalendarCreateR
postCalendarCreateR = do
    uid        <- requireUserId
    form       <- runFormPost (calendarForm $ Left uid)
    handleNewCalendar form
    defaultLayout $ do
        setTitle "Uusi kalenteri"
        $(widgetFile "calendar-create")

-- | Single calendar view and edit page.
-- Editing is enabled iff viewing as calendar owner.
getCalendarEditR, postCalendarEditR :: CalendarId -> Handler Html
getCalendarEditR        = postCalendarEditR
postCalendarEditR calId = do
    muid <- maybeUserId
    cal  <- runDB (get404 calId)

    mform <- case muid of
        Just uid | uid == calendarOwner cal -> do
            form <- runFormPost $ calendarForm (Right cal)
            handleEditCalendar calId form
            return (Just form)
        _ -> return Nothing

    defaultLayout $ do
        setTitle $ toHtml $ calendarTitle cal
        $(widgetFile "calendar-edit")

-- ** Form handlers

-- | On FormSuccess update the db entry and redirect to CalendarR.
handleNewCalendar :: ((FormResult Calendar, b), c) -> Handler ()
handleNewCalendar    ((FormSuccess c,_),_) = runDB (insert c) >> setMessage (isSuccess "Kalenteri luotu.") >> redirect CalendarR
handleNewCalendar    _                     = return ()

-- | On FormSuccess update the db entry and redirect to CalendarR.
handleEditCalendar :: CalendarId -> ((FormResult Calendar, b), c) -> Handler ()
handleEditCalendar k ((FormSuccess c,_),_) = runDB (replace k c) >> setMessage (isSuccess "Kalenteri päivitetty.") >> redirect CalendarR
handleEditCalendar _ _                     = return ()

-- ** Widgets and forms

calendarFormWidget :: Maybe CalendarId -> ((t, Widget), Enctype) -> Widget
calendarFormWidget Nothing  form = renderForm MsgNewCalendarTitle form CalendarR (submitI MsgNewCalendarDo)
calendarFormWidget (Just k) form = renderForm MsgEditCalendarTitle form (CalendarEditR k) (submitI MsgEditCalendarDo)

calendarForm :: Either UserId Calendar -> Form Calendar
calendarForm x = renderBootstrap2 $ Calendar (either id calendarOwner x)
    <$> areq textField "Otsikko" (either (const Nothing) (Just . calendarTitle) x)
    <*> fmap unTextarea (areq textareaField "Merkinnät"
                         (either (const Nothing) (Just . Textarea . calendarRemind) x))

remindRender :: RemindRes -> Widget
remindRender x = [whamlet|$newline always
$case x
    $of Right res
        <pre style="font-size:65%">#{res}
    $of Left err
        <div.alert.alert-error>#{err}
|]
