{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Model where

import Yesod
import Yesod.Markdown (Markdown)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.ByteString (ByteString)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.Time.Calendar (Day)
import Prelude

import Permissions

type EditLog = [(UTCTime, Text)]

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
