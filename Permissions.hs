------------------------------------------------------------------------------
-- | 
-- Module         : Permissions
-- Copyright      : (C) 2015 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
-- File Created   : 2015-03-26T20:25:27+0200
------------------------------------------------------------------------------
module Permissions where

import Prelude
import Data.Monoid
import Data.Bits
import Data.Typeable (Typeable)
import Database.Persist.Sql

-- | It holds that @p1 >= p2@ if Permissions p1 are at least as permissive
-- as p2.
newtype Permissions = Permissions { itemPermissions :: Int }
                    deriving (Eq, Ord, Typeable, Show, Read, PersistField, PersistFieldSql, Bits)

instance Monoid Permissions where
    mempty = Permissions zeroBits
    mappend (Permissions a) (Permissions b) = Permissions (a .|. b)

isReadableAll, isReadableMembers :: Permissions -> Bool
isReadableAll     p = testBit p 0
isReadableMembers p = testBit p 1 || isReadableAll p

readableNone, readableAll, readableMembers :: Permissions
readableNone    = mempty
readableAll     = Permissions (bit 0)
readableMembers = Permissions (bit 1)
