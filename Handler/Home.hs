{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
#if !DEVELOPMENT
{-# OPTIONS_GHC -fno-warn-unused-matches #-} -- RecordWildCards in widgets
#endif
module Handler.Home where

import           Import
import           Control.Monad
import qualified Data.Text  as T
import           Data.Text.Encoding
import           Data.Time
import           Data.Char (toLower, isSpace, isPunctuation)
import           Data.Maybe
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import           Data.Conduit
import qualified Data.Conduit.List as CL

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
    ((result, formWidget), formEnctype) <- runFormPost memberForm

    case result of
        FormSuccess res -> do
            _ <- runDB $ insert res
            setMessage $ toHtml $ "Tervetuloa saunomaan, " <> memberName res
            redirect MembersR
        _ -> return ()

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

memberForm :: Form Member
memberForm = renderBootstrap2 $ Member False
    <$> areq textField "Koko nimi" Nothing
    <*> areq textField "Kotikunta" Nothing
    <*> areq (checkM mailNotTaken emailField) "Sähköposti" Nothing
    <*> areq checkBoxField "Olen HYY:n jäsen" Nothing
    <*> areq dayField "Syntymäaika" Nothing
    <*> areq (radioFieldList genderSelections) "Sukupuoli" Nothing
    <*> areq textField "Miten sait tietää meistä?" Nothing
    <*> (fmap (fmap unTextarea)) (aopt textareaField "Löylyprofiilisi" Nothing)
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
    ((res, widget), enctype) <- runFormPost $ renderBootstrap2 $ (,,,)
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

-- * Gallery

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

getImageViewR :: Text -> Text -> Int -> Handler Html
getImageViewR author ident nth = do
    (Entity _ Album{..}, Entity _ Image{..}) <- getImage author ident nth
    defaultLayout $ do
        setTitle "An image"
        $(widgetFile "gallery-image")

getImageR, getThumbR :: Text -> Text -> Int -> Handler ()
getImageR = sendImageWith False
getThumbR = sendImageWith True

-- | Unoverloaded version of ThumbR/ImageR
sendImageWith :: Bool -> Text -> Text -> Int -> Handler ()
{-# INLINE sendImageWith #-}
sendImageWith thumb author ident nth = do
    root                               <- extraGalleryRoot <$> getExtra
    (Entity aid _, Entity _ Image{..}) <- getImage author ident nth

    let path = root </> show (fromSqlKey aid) </>
                (if thumb then thumbDir </> imageFile <.> ".jpg"
                          else imageFile <.> imageFileExt)

    sendFile (if thumb then typeJpeg else imageContentType) path

uploadWidget :: Maybe (Entity Album) -> Widget
uploadWidget malbum = do
    ((result, widget), _) <- handlerToWidget $ runFormPost $ renderBootstrap2 $ (,,)
        <$> areq textField "Albumin otsikko" (albumTitle . entityVal <$> malbum)
        <*> areq textField "Kuka olet" Nothing
        <*> areq passwordField "Salasana" Nothing

    case result of
        FormSuccess (title, author, pass) -> do

            handlerToWidget $ do
                pass' <- extraBlogPass <$> getExtra
                when (pass /= pass') $ do setMessage "Väärä salasana"
                                          redirect GalleryR

            Entity aid Album{..} <- handlerToWidget $ createOrUpdateAlbum author malbum title
            nfiles <- lookupFiles "files" >>= handlerToWidget . saveFiles author aid
            setMessage $ toHtml $ show nfiles ++ " kuvaa lisätty."
            redirect $ AlbumR albumAuthor albumIdent

        _ -> $(widgetFile "gallery-upload-form")

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
        _ <- runDB $ insert $ Image time aid nth (T.pack base) author contentType file ext
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
createOrUpdateAlbum :: Text -> Maybe (Entity Album) -> Text -> Handler (Entity Album)
createOrUpdateAlbum author (Just (Entity aid album)) title = do
    runDB (update aid [AlbumTitle =. title])
    return $ Entity aid album
createOrUpdateAlbum author Nothing                   title = do
    time      <- liftIO getCurrentTime
    let album = Album time (toUrlIdent title) title author
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
getImage :: Text -> Text -> Int -> Handler (Entity Album, Entity Image)
getImage author ident nth = runDB $ do
    album@(Entity aid _) <- getBy404 $ UniqueAlbum author ident
    image                <- getBy404 $ UniqueImage aid nth
    return (album, image)


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
toUrlIdent = T.map f . T.filter (not . isPunctuation)
  where
    f c | isSpace c = '-'
        | otherwise = toLower c
