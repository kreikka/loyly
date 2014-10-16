{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
module Handler.Home where

import           Import
import qualified Data.Text  as T
import           Data.Text.Encoding
import           Data.Time
import           Data.Char (toLower, isSpace, isPunctuation)
import           Data.Maybe
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Text.Pandoc
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

getGalleryR :: Handler Html
getGalleryR = do
    defaultLayout $ do
        setTitle "Galleria"
        $(widgetFile "gallery")

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
    Lisäksi jäseneksi hyväksymistä odottaa #{length membsNotAppr}.
|]


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

    posts <- runDB $ selectList [] [Asc BlogPostIdent]
    defaultLayout $ do
        setTitle "Saunablogi"
        $(widgetFile "blog-home")

handleBlogPost :: (FileInfo, Bool, Text, Text) -> Handler ()
handleBlogPost (fi, overwrite, editor, _) = do
    markdown <- fmap (Markdown . decodeUtf8 . B.toStrict . B.toLazyByteString) $ fileSource fi $$ CL.foldMap B.byteString

    let doc@(Pandoc meta _) = parseMarkdown yesodDefaultReaderOptions markdown
        rendered   = writePandoc yesodDefaultWriterOptions doc
        tags       = maybe [] getTags $ lookupMeta "tags" meta
        title      = T.pack $ concatMap inlineToString (docTitle meta)
        authors    = map (T.pack . concatMap inlineToString) (docAuthors meta)
        ident      = filterTitle title

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
                                                     , blogPostTags     = tags
                                                     , blogPostAuthors  = authors
                                                     , blogPostLog      = blogPostLog v ++ [(time, editor)]
                                                     , blogPostRendered = rendered
                                                     })
                                  setMessage "Postaus päivitetty; merkitse ruksi jos haluat päivittää postausta."
                                  redirect $ BlogPostR ident
                | otherwise -> return ()

getBlogPostR :: Text -> Handler Html
getBlogPostR ident = do
    Entity _ BlogPost{..} <- runDB $ getBy404 $ UniqueBlogPost ident
    let doc = parseMarkdown yesodDefaultReaderOptions blogPostMarkdown
        content             = writePandocTrusted yesodDefaultWriterOptions doc
    defaultLayout $ do
        setTitle $ toHtml blogPostTitle
        $(widgetFile "blog-post")

-- * Misc.

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
filterTitle :: Text -> Text
filterTitle = T.map f . T.filter (not . isPunctuation)
  where
    f c | isSpace c = '-'
        | otherwise = toLower c
