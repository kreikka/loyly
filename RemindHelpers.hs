{-# LANGUAGE LambdaCase #-}
------------------------------------------------------------------------------
-- | 
-- Module         : RemindHelpers
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module RemindHelpers where

import           Prelude
import           Yesod
import           Model
import           Control.Applicative
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Process as P
import           System.Exit (ExitCode(..))
import           System.IO (hClose)

type RemindRes = Either Text Text

-- | Run a remind command
remindRun :: MonadIO m => [String] -> Text -> m RemindRes
remindRun ps ctx = liftIO $ do
    (Just inp, Just out, Just err, pid) <- P.createProcess
        ( (P.proc "remind" (ps ++ ["-"]))
            { P.std_in = P.CreatePipe
            , P.std_out = P.CreatePipe
            , P.std_err = P.CreatePipe
            }
        )
    T.hPutStr inp ctx >> hClose inp
    out' <- T.hGetContents out
    err' <- T.hGetContents err
    P.waitForProcess pid >>= return . \case
        ExitSuccess -> Right out'
        ExitFailure _ -> Left err'

-- prettyRemindS :: Text -> WidgetT site m ()
prettyRemindS txt = case T.words txt of
    date : _ : _ : _ : _ : time : msg -> [whamlet|<i>#{date} #{time} - #{T.unwords msg}|]
    _ -> [whamlet|error: no parse: #{txt}|]

-- remindListS :: (exceptions-0.6.1:Control.Monad.Catch.MonadThrow m, MonadBaseControl IO m, MonadIO m) => Text -> WidgetT site m ()
remindListS txt = do
    res <- fmap T.lines <$> remindRun ["-r", "-s+2"] txt
    [whamlet|
$case res
  $of Right xs
    $forall x <- xs
      <li.light>^{prettyRemindS x}
  $of Left err
      <li.alert-error>err
|]

-- combineReminds :: (YesodPersist site, YesodPersistBackend site ~ persistent-2.1.1:Database.Persist.Sql.Types.SqlBackend) => HandlerT site IO Text
combineReminds =
    T.unlines . map (calendarRemind . entityVal)
    <$> runDB (selectList [] [])

-- remindNextWeeks :: (YesodPersist site, YesodPersistBackend site ~ persistent-2.1.1:Database.Persist.Sql.Types.SqlBackend) => WidgetT site IO ()
remindNextWeeks = remindListS =<< handlerToWidget combineReminds

