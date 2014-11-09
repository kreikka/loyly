{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
#if !DEVELOPMENT
{-# OPTIONS_GHC -fno-warn-unused-matches #-} -- RecordWildCards in widgets
#endif
module Handler.Home where

import           Import
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Char (toLower, isSpace, isPunctuation)
import qualified Data.Text  as T
import           Data.Text.Encoding
import           Data.Time

---------------------------------------------------------------
import           System.FilePath ((</>), (<.>))
import qualified System.FilePath as F
import qualified System.Directory
import qualified System.Process as P
import           System.Exit (ExitCode(..))
import           Text.Pandoc hiding (Image)
import           Text.Printf
import           Database.Persist.Sql (fromSqlKey)
import           Yesod.Markdown
import           Yesod.Auth.Account (setPasswordR, newPasswordForm, resetPasswordR)
import qualified Yesod.Auth.Message as Msg


homeTitle :: MonadWidget m => m ()
homeTitle = setTitle "Akateeminen saunakerho Löyly ry"

getHomeR :: Handler Html
getHomeR =
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
            setMessage $ toHtml $ "Tervetuloa saunomaan, " <> memberName res
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
    form@((res, _), _) <- runFormPost $ renderBootstrap2 $ (,,,)
        <$> fileAFormReq "Markdown-tiedosto"
        <*> areq checkBoxField "Päivitän postausta" Nothing
        <*> areq textField "Kuka olet" Nothing
        <*> areq textField "Salasana" Nothing

    case res of
        FormSuccess r@(_,_,_,pass) -> do
            authorized <- (== pass) . extraBlogPass <$> getExtra
            if authorized
                then handleBlogPost r
                else setMessage "Wäärä salasana"
        _ -> return ()

    defaultLayout $ do
        setTitle "Saunablogi"
        $(widgetFile "blog-home")

handleBlogPost :: (FileInfo, Bool, Text, Text) -> Handler ()
handleBlogPost (fi, overwrite, editor, _) = do
    markdown <- fmap (Markdown . decodeUtf8 . B.toStrict . B.toLazyByteString) $ fileSource fi $$ CL.foldMap B.byteString

    let doc@(Pandoc meta _) = parseMarkdown yesodDefaultReaderOptions markdown
        rendered   = writePandocTrusted yesodDefaultWriterOptions doc
        tags       = maybe [] getTags $ lookupMeta "tags" meta
        title      = T.pack $ concatMap inlineToString (docTitle meta)
        authors    = map (T.pack . concatMap inlineToString) (docAuthors meta)
        ident      = toUrlIdent title

    if T.null ident then setMessage "title-metakenttä ei voi olla tyhjä"
                    else do

        mp   <- runDB $ getBy $ UniqueBlogPost ident
        time <- liftIO getCurrentTime

        case mp of
            Nothing         -> do _ <- runDB $ insert $ BlogPost ident title authors tags [(time, editor)] markdown rendered
                                  setMessage "Postaus lisätty."
                                  redirect $ BlogPostR ident
            Just (Entity k v)
                | overwrite -> do runDB (replace k v { blogPostTitle    = title
                                                     , blogPostAuthors  = authors
                                                     , blogPostTags     = tags
                                                     , blogPostLog      = blogPostLog v ++ [(time, editor)]
                                                     , blogPostMarkdown = markdown
                                                     , blogPostRendered = rendered
                                                     })
                                  setMessage "Postaus päivitetty."
                                  redirect $ BlogPostR ident
                | otherwise -> return ()

getBlogPostR :: Text -> Handler Html
getBlogPostR ident = do
    Entity _ BlogPost{..} <- runDB $ getBy404 $ UniqueBlogPost ident
    defaultLayout $ do
        setTitle $ toHtml blogPostTitle
        $(widgetFile "blog-post")

recentBlogPosts :: Maybe Int -> Widget
recentBlogPosts mn = do
    posts <- handlerToWidget $ runDB $
        selectList [] ((Desc BlogPostLog) : maybe [] (return . LimitTo) mn)
    [whamlet|
<ul>
  $forall Entity _ post <- posts
    <li>
      <a href=@{BlogPostR (blogPostIdent post)}>#{blogPostTitle post}
|]

-- * Profile

type AckResult = [(PersonInImageId, Maybe Bool)]

getProfileR :: Handler Html
getProfileR = do
    u <- requireAuthId
    Entity uid user <- runDB $ getBy404 $ UniqueUsername u
    mmemb <- runDB $ getBy $ UniqueMember $ userEmail user
    passForm <- runFormPost $ renderBootstrap2 $ newPasswordForm u (userResetPasswordKey user)
    ((_, ackWidget), enctype) <- runImagePublishAckForm uid
    defaultLayout $ do
        setTitle "Löylyprofiili"
        $(widgetFile "profile")

postImagePublishAckR :: Handler Html
postImagePublishAckR = getProfileR -- XXX: is this always sensible?

-- runImagePublishAckForm :: UserId -> Handler (Form AckResult)
runImagePublishAckForm uid = do
    acks <- runDB $ selectList [ PersonInImageUser ==. uid, PersonInImagePublishable ==. Nothing]
                               [ Asc PersonInImageImg ]
    let runForm = runFormPost $ renderBootstrap2 $ sequenceA $ map ackForm acks
    form@((res,_),_) <- runForm
    case res of
        FormSuccess xs -> do runDB $ mapM_ (uncurry updatePublishable) xs
                             runForm
        _ -> return form
  where
    ackForm (Entity k v) = (k, ) <$> aopt (ackField v) "" Nothing

    updatePublishable k status = update k [PersonInImagePublishable =. status]

ackField :: PersonInImage -> Field Handler Bool
ackField v = boolField' { fieldView = \theId name attrs val isReq -> [whamlet|
<div.clearfix.ack>
  <img style="float:left" src=@{ThumbByIdR $ personInImageImg v}>
  <div .controls .clearfix>^{fieldView boolField' theId name attrs val isReq}
|] }
    where
        boolField' :: Field Handler Bool
        boolField' = boolField

getPublicProfileR :: Text -> Handler Html
getPublicProfileR u = do
    Entity _ user <- runDB $ getBy404 $ UniqueUsername u
    Entity _ memb <- runDB $ getBy404 $ UniqueMember $ userEmail user
    defaultLayout $ do
        setTitle $ toHtml $ "Löylyprofiili: " <> userUsername user
        $(widgetFile "profile-public")

-- * Gallery

type ImageInfo = (Entity Album, Entity Image, [Entity User])

-- | Some default view; recent imgs or smth
getGalleryR, postGalleryR :: Handler Html
getGalleryR  = postGalleryR
postGalleryR = do
    albums <- runDB $ selectList [] [Desc AlbumDate]
    images <- runDB $ selectList [] [Desc ImageDate, LimitTo 5]
    defaultLayout $ do
        setTitle "Galleria"
        $(widgetFile "gallery-home")

-- | View all imgs in a specific album.
getAlbumR, postAlbumR :: Text -> Text -> Handler Html
getAlbumR               = postAlbumR
postAlbumR author ident = do
    (album@(Entity _ Album{..}), images) <- runDB $ do
        album@(Entity aid _) <- getBy404 $ UniqueAlbum author ident
        imgs <- selectList [ImageAlbum ==. aid] [Asc ImageNth]
        return (album, imgs)

    defaultLayout $ do
        setTitle $ toHtml albumTitle
        $(widgetFile "gallery-album")

getImageViewR, postImageViewR :: Text -> Text -> Int -> Handler Html
postImageViewR = getImageViewR
getImageViewR author ident nth = do
    Just route <- getCurrentRoute
    maid <- maybeAuthId

    img@(Entity _ Album{..}, Entity iid Image{..}, people) <- getImage author ident nth

    if isNothing maid && not imagePublic then notFound else do

        allPeople <- fmap (map $ (,) <$> userUsername . entityVal <*> entityKey) $ runDB $ selectList [] [Asc UserUsername]

        form@((res,_),_) <- runFormPost $ renderBootstrap2 $ imageEditForm allPeople img

        case res of
            FormSuccess (newPeople, newDesc) -> do
                runDB $ do
                    update iid [ImageDesc =. fromMaybe "" newDesc]
                    deleteWhere [PersonInImageImg ==. iid, PersonInImageUser /<-. newPeople]
                    mapM_ (\u -> insertUnique $ PersonInImage iid u Nothing) newPeople
                redirect route
            _ -> return ()

        defaultLayout $ do
            setTitle "An image"
            $(widgetFile "gallery-image")

getImageR, getThumbR :: Text -> Text -> Int -> Handler ()
getImageR x y z = sendImage False =<< getImage x y z
getThumbR x y z = sendImage True  =<< getImage x y z

getImageByIdR, getThumbByIdR :: ImageId -> Handler ()
getImageByIdR = getImageById >=> sendImage False
getThumbByIdR = getImageById >=> sendImage True

sendImage :: Bool -> ImageInfo -> Handler ()
{-# INLINE sendImage #-}
sendImage thumb (Entity aid _, Entity _ Image{..}, _) = do
    root <- extraGalleryRoot <$> getExtra
    let path = root </> show (fromSqlKey aid) </>
                (if thumb then thumbDir </> imageFile <.> ".jpg"
                          else imageFile <.> imageFileExt)
    sendFile (if thumb then typeJpeg else imageContentType) path

imageEditForm allPeople (_,Entity _ img, people) = (,)
    <$> areq (multiSelectFieldList allPeople) "Ihmiset kuvassa" (Just $ entityKey <$> people)
    <*> (fmap unTextarea <$> aopt textareaField "Meta" (Just . Just . Textarea $ imageDesc img))

uploadWidget :: Maybe (Entity Album) -> Widget
uploadWidget malbum = do
    ((result, widget), _) <- handlerToWidget $ runFormPost $ renderBootstrap2 $ (,,,)
        <$> areq textField "Albumin otsikko" (albumTitle . entityVal <$> malbum)
        <*> areq textField "Kuka olet" Nothing
        <*> areq checkBoxField "Albumi on julkinen" Nothing
        <*> areq passwordField "Salasana" Nothing

    case result of
        FormSuccess (title, author, public, pass) -> do

            handlerToWidget $ do
                pass' <- extraBlogPass <$> getExtra
                when (pass /= pass') $ do setMessage "Väärä salasana"
                                          redirect GalleryR

            Entity aid Album{..} <- handlerToWidget $ createOrUpdateAlbum author malbum title public
            nfiles <- lookupFiles "files" >>= handlerToWidget . saveFiles author aid
            setMessage $ toHtml $ show nfiles ++ " kuvaa lisätty."
            redirect $ AlbumR albumAuthor albumIdent

        _ -> renderForm msg ((result, widget >> filesWidget), Multipart) GalleryR (submitI msg)
  where
    msg = case malbum of
              Nothing -> MsgCreateNewAlbum
              Just a  -> MsgUpdateAlbum
    filesWidget = [whamlet|
<div.control-group.clearfix required>
    <label.control-label for=files>Kuvatiedostot
    <div.controls input>
        <input#files name=files type=file multiple>
|]

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
        _ <- runDB $ insert $ Image False "" time aid nth (T.pack base) author contentType file ext
        return (file <.> ext)

    code <- liftIO $ generateThumbnails albumRoot thumbDir names
    when (code /= ExitSuccess) $ do
        setMessage "Virhe: kuvat lisättiin, mutta peukalonkynsien generointi epäonnistui!"
        redirect HomeR

    return $ length fs

-- | @generateThumbnails albumRoot thumbs_relative names@
generateThumbnails :: FilePath -> FilePath -> [FilePath] -> IO ExitCode
generateThumbnails albumRoot thumbs names = do
    (_,_,_,ph) <- P.createProcess
        (P.proc "mogrify" $ ["-format", "jpg", "-thumbnail", "270x270", "-path", thumbs] ++ names)
        { P.cwd = Just albumRoot }
    P.waitForProcess ph

-- Or update album title.
--
-- NOTE don't use @albumTitle@ returned here, it is not updated
createOrUpdateAlbum :: Text -> Maybe (Entity Album) -> Text -> Bool -> Handler (Entity Album)
createOrUpdateAlbum _author (Just (Entity aid album)) title public = do
    runDB (update aid [ AlbumTitle =. title
                      , AlbumPublic =. public ])
    return $ Entity aid album

createOrUpdateAlbum author Nothing                    title public = do
    time      <- liftIO getCurrentTime
    let album = Album public time (toUrlIdent title) title author
    aid       <- runDB $ insert album
    return $ Entity aid album

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

-- XXX: Could use a join here too
getImage :: Text -> Text -> Int -> Handler (Entity Album, Entity Image, [Entity User])
getImage author ident nth = runDB $ do
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

getImagePeople iid = do
    peopleIds <- map (personInImageUser . entityVal) <$> selectList [PersonInImageImg ==. iid] []
    selectList [UserId <-. peopleIds] [Asc UserUsername]

-- * Misc.

-- | Relative to *album root*.
thumbDir :: FilePath
thumbDir = "thumbs"

toImageFile :: Int -> String
toImageFile = printf "%03d"

inlineToString :: Inline -> String
inlineToString x = case x of
    Str s    -> s
    Space    -> " "
    Code _ s -> s
    _        -> ""

getTags :: MetaValue -> [Text]
getTags (MetaString s)    = [T.pack s]
getTags (MetaList xs)     = mapMaybe unMetaText xs
getTags _                 = []

unMetaText :: MetaValue -> Maybe Text
unMetaText (MetaString s)   = Just $ T.pack s
unMetaText (MetaInlines xs) = Just $ T.pack $ concatMap (inlineToString) xs
unMetaText _                = Nothing

-- | space -> '-', punctuation removed
toUrlIdent :: Text -> Text
toUrlIdent = T.map f . T.unwords . T.words . T.filter (not . isPunctuation)
  where
    f c | isSpace c = '-'
        | otherwise = toLower c
