{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foundation where

import Prelude
import Data.Text (Text)
import Data.Time
import qualified Data.Text as T
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.Account
import qualified Yesod.Auth.Message as Msg
import qualified Yesod.Auth.Account.Message as Msg
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlBackend)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import RemindHelpers
import Mixins
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Yesod.Core.Types (Logger)
import Data.Time.Format.Human
#if DEVELOPMENT
#else
import Network.Mail.Mime (Address(..), renderSendMail, simpleMail')
#endif
import System.Locale

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    }

instance HasHttpManager App where
    getHttpManager = httpManager

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)
type Query x = YesodDB App x

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        route <- getCurrentRoute
        maid <- maybeAuthId

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                , css_typebase_css
                ])
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authenitcation.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

    maximumContentLength _ (Just GalleryR)     = Just (32 * 1024 * 1024)
    maximumContentLength _ (Just (AlbumR _ _)) = Just (32 * 1024 * 1024)
    maximumContentLength _ _                   = Just (2 * 1024 * 1024)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

-- Auth

instance YesodAuth App where
    type AuthId App = Username
    getAuthId = return . Just . credsIdent
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authHttpManager _ = error "No manager needed"
    onLogin = return ()
    maybeAuthId = lookupSession credsKey
    renderAuthMessage _ _ = Msg.finnishMessage -- TODO i18n
    authPlugins _ = [ accountPlugin { apLogin = \tm -> do
        form <- liftHandlerT $ runFormPostNoToken $ renderBootstrap2 loginForm
        [whamlet|
<div.toppush>
^{renderForm Msg.LoginTitle form (tm loginFormPostTargetR) (submitI Msg.LoginTitle)}
<p>
  <a href=@{tm newAccountR}>_{Msg.RegisterLong}
  <br>
  <a href=@{tm resetPasswordR}>_{Msg.MsgForgotPassword}
|] }]

instance PersistUserCredentials User where
    userUsernameF = UserUsername
    userPasswordHashF = UserPassword
    userEmailF = UserEmail
    userEmailVerifiedF = UserVerified
    userEmailVerifyKeyF = UserVerifyKey
    userResetPwdKeyF = UserResetPasswordKey
    uniqueUsername = UniqueUsername

    userCreate uname email verkey pass = User uname pass email False verkey "" False

instance YesodAuthAccount (AccountPersistDB App User) App where
    runAccountDB = runAccountPersistDB
    renderAccountMessage _ _ = finnishAccountMsg -- TODO i18n

    getNewAccountR = do
        tm <- getRouteToParent
        lift $ defaultLayout $ do
            setTitleI Msg.RegisterLong
            form <- liftHandlerT $ runFormPost $ renderBootstrap2 newAccountForm
            [whamlet|
<div.toppush>
^{renderForm Msg.RegisterLong form (tm newAccountR) (submitI Msg.Register)}
<p>
  <a href=@{AuthR LoginR}>_{Msg.LoginTitle}
|]

    getResetPasswordR = do
        tm <- getRouteToParent
        lift $ defaultLayout $ do
            setTitleI Msg.PasswordResetTitle
            [whamlet|<div.toppush>|]
            runAndRenderFormPost Msg.PasswordResetTitle resetPasswordForm (tm resetPasswordR) (submitI Msg.SendPasswordResetEmail)

    unregisteredLogin u = do
        tm <- getRouteToParent
        lift $ defaultLayout $ do
            setTitleI Msg.MsgEmailUnverified

            [whamlet|
<div .toppush>
<div .alert .alert-error>Kirjautuminen epäonnistui
|] -- TODO i18n

            runAndRenderFormPost Msg.MsgEmailUnverified (resendVerifyEmailForm (username u)) (tm resendVerifyR) (submitI Msg.MsgResendVerifyEmail)

    setPasswordHandler u = do
        tm <- getRouteToParent
        lift $ defaultLayout $ do
            setTitleI Msg.SetPassTitle
            [whamlet|<div.toppush>|]
            runAndRenderFormPost Msg.SetPass (newPasswordForm (username u) (userResetPwdKey u)) (tm setPasswordR) (submitI Msg.SetPassTitle)

-- XXX: pull request to yesod-auth-account
finnishAccountMsg :: Msg.AccountMsg -> T.Text
finnishAccountMsg Msg.MsgUsername = "Käyttäjänimi"
finnishAccountMsg Msg.MsgForgotPassword = "Unohditko salasanasi?"
finnishAccountMsg Msg.MsgInvalidUsername = "Väärä käyttäjänimi"
finnishAccountMsg (Msg.MsgUsernameExists u) = T.concat ["Käyttäjänimi ", u, " on jo käytössä.  Valitse jokin muu käyttäjänimi."]
finnishAccountMsg Msg.MsgResendVerifyEmail = "Lähetä sähköpostivarmennus uudestaan."
finnishAccountMsg Msg.MsgResetPwdEmailSent = "Salasanan palautusviesti on lähetetty sähköpostiisi."
finnishAccountMsg Msg.MsgEmailVerified = "Sahköpostiosoitteesi on nyt varmistettu."
finnishAccountMsg Msg.MsgEmailUnverified = "Sähköpostiosoitettasi ei ole vielä varmistettu."

instance AccountSendEmail App where
#if DEVELOPMENT
#else
    sendVerifyEmail user to url = do
        extra <- getExtra
        let fromAddr= Address Nothing $ extraEmail extra
            toAddr  = Address Nothing to
            subject = "Saunahali: Vahvista sähköpostisi!"

        liftIO $ renderSendMail $ simpleMail' toAddr fromAddr subject body
      where
          body = TL.fromStrict $ T.unlines 
              [ "Tervetuloa Helsingin Akateemisten löylyttelijiöiden sivulle!"
              , ""
              , "Olet luonut käyttäjän " `T.append` user `T.append` " tälle sähköpostiosoitteelle ja se vaatii"
              , "vahvistuksen osoitteessa: "
              , url
              ]

    sendNewPasswordEmail user to url = do
        extra <- getExtra
        let fromAddr= Address Nothing $ extraEmail extra
            toAddr  = Address Nothing to
            subject = "Saunahali: Salasanan palautus!"

        liftIO $ renderSendMail $ simpleMail' toAddr fromAddr subject body
      where
          body = TL.fromStrict $ T.unlines 
              [ "Tervetuloa takaisin!"
              , ""
              , "Pyysit unohtuneen salasanan käyttäjälle `" `T.append` user `T.append` "`tälle sähköpostiosoitteelle."
              , "Voit vaihtaa osoitteen täällä:"
              , url
              ]
#endif

-- Form, extra

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

submitI :: RenderMessage App a => a -> Widget
submitI msg = [whamlet|<input .btn type=submit value=_{msg}>|]

renderForm :: RenderMessage App msg => msg -> ((t, Widget), Enctype) -> Route App -> Widget -> Widget
renderForm legend ((_, widget), enctype) action formActions = [whamlet|
<form .form method=post enctype=#{enctype} action=@{action}>
  <fieldset>
    <legend>_{legend}
    ^{widget}
  <div .form-actions>^{formActions}
|]

runAndRenderFormPost :: RenderMessage App msg => msg -> AForm Handler a -> Route App -> Widget -> Widget
runAndRenderFormPost legend form action formActions = do
    rendered <- liftHandlerT $ runFormPost $ renderBootstrap2 form
    renderForm legend rendered action formActions

prettyDate :: UTCTime -> Widget
prettyDate t = do
    s <- liftIO $ humanReadableTimeI18N finnishLocale t
    [whamlet|#{s}|]

finnishLocale :: HumanTimeLocale
finnishLocale = defaultHumanTimeLocale
    { justNow       = "juuri äsken"
    , secondsAgo    = (++ " sekuntia sitten")
    , oneMinuteAgo  = "minuutti sitten"
    , minutesAgo    = (++ " minuuttia sitten")
    , oneHourAgo    = "tunti sitten"
    , aboutHoursAgo = \x -> "noin " ++ x ++ " tuntia sitten"
    , at            = \n _ -> "viime " ++ (case n of
                               0 -> "sunnuntaina"
                               1 -> "maanantaina"
                               2 -> "tiistaina"
                               3 -> "keskiviikkona"
                               4 -> "torstaina"
                               5 -> "perjantaina"
                               6 -> "lauantaina"
                               7 -> "sunnuntaina"
                               _ -> "EI OLE PÄIVÄ: " ++ show n)
    , daysAgo      = (++ " päivää sitten")
    , weekAgo      = (++ " viikko sitten")
    , weeksAgo     = (++ " viikkoa sitten")
    , onYear       = id
    , locale       = defaultTimeLocale
        { wDays = [("Sunnuntai", "Su"), ("Maanantai", "Ma")
                  ,("Tiistai", "Ti"), ("Keskiviikko", "Ke")
                  ,("Torstai", "To"), ("Perjantai", "Pe")
                  ,("Lauantai", "La")]
        , months = [("Tammikuu", "Tam"), ("Helmikuu", "Hel")
                   ,("Maaliskuu", "Maa"), ("Huhtikuu", "Huh")
                   ,("Toukokuu", "Tou"), ("Kesäkuu", "Kes")
                   ,("Heinäkuu", "Hei"), ("Elokuu", "Elo")
                   ,("Syyskuu", "Syy"), ("Lokakuu", "Lok")
                   ,("Marraskuu", "Mar"), ("Joulukuu", "Jou")]
        }
    , dayOfWeekFmt = "%A klo %l:%M %p"
    , thisYearFmt  = "%B %e"
    , prevYearFmt  = "%B %e, %Y"
    }

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Other

isAuthRoute :: Maybe (Route App) -> Bool
isAuthRoute (Just (AuthR _)) = True
isAuthRoute _ = False

recentBlogPosts :: Maybe Int -> Widget
recentBlogPosts mn = do
    mr    <- handlerToWidget getCurrentRoute
    posts <- handlerToWidget $ runDB $
        selectList [] $ Desc BlogPostLog : maybe [] (return . LimitTo) mn
    [whamlet|
$forall Entity _ post <- posts
  $with route <- BlogPostR (blogPostIdent post)
    <li :mr == Just route:.active>
      <a href=@{route}>#{blogPostTitle post}
|]

isSuccess, isError :: Html -> Html
isSuccess msg = do
    preEscapedToMarkup ("<div class=\"alert alert-success\" id=message>" :: T.Text)
    msg
    preEscapedToMarkup ("</div>" :: Text)

isError msg = do
    preEscapedToMarkup ("<div class=\"alert alert-error\" id=message>" :: T.Text)
    msg
    preEscapedToMarkup ("</div>" :: Text)
