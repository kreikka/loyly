{-# LANGUAGE LambdaCase, ConstraintKinds #-}
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
import           Database.Persist.Sql
import qualified System.Process as P
import           System.Exit (ExitCode(..))
import           System.IO (hClose)

type RemindRes = Either Text Text

type RemindCtx site = (Yesod site, YesodPersist site, YesodPersistBackend site ~ SqlBackend)

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

-- | Print a reminder line formatted inside an <i> element.
prettyRemindI :: Text -> WidgetT site IO ()
prettyRemindI txt = case T.words txt of
    date : _ : _ : _ : _ : time : msg -> [whamlet|<i>#{date} #{time} - #{T.unwords msg}|]
    _ -> [whamlet|error: no parse: #{txt}|]

-- | <li> entries for next reminders.
remindListS :: RemindCtx site => Text -> WidgetT site IO ()
remindListS txt = do
    res <- fmap T.lines <$> remindRun ["-r", "-s+2"] txt
    [whamlet|
$case res
  $of Right xs
    $forall x <- xs
      <li.light>^{prettyRemindI x}
  $of Left err
      <li.alert-error>err
|]

-- | Combine all remind entries from DB to a single text that can be fed to
-- 'remind'.
combineReminds :: RemindCtx site => HandlerT site IO Text
combineReminds = do
    txts <- runDB (selectList [] [])
    return $ T.unlines . concat $ map (T.lines . T.replace "\r\n" "\n" . calendarRemind . entityVal) txts

remindNextWeeks :: RemindCtx site => WidgetT site IO ()
remindNextWeeks = remindListS =<< handlerToWidget combineReminds

-- | A simple ascii calendar (remind -c)
remindAsciiCalendar :: RemindCtx site => HandlerT site IO RemindRes
remindAsciiCalendar = remindRun
    ["-c+4"       -- generate four week calendar
    , "-b1"       -- 24h time format
    , "-m"        -- week begins at monday
    , "-r"        -- disable RUN and shell()
    , "-w110,0,0" -- 110 character cells
    ] =<< combineReminds
