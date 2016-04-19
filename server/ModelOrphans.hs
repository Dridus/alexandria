{-# OPTIONS_GHC -fno-warn-orphans #-}
module ModelOrphans where

import ClassyPrelude
import Control.Lens (view, re)
import Data.Proxy (Proxy(Proxy))
import Database.Persist
import Database.Persist.Sql
import Slack
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)
import Web.PathPieces (PathPiece)

instance PersistField TS where
  toPersistValue = toPersistValue . view _TS
  fromPersistValue = map (view $ re _TS) . fromPersistValue
instance PersistFieldSql TS where
  sqlType _ = sqlType (Proxy :: Proxy Text)
deriving instance PathPiece TS
deriving instance FromHttpApiData TS
deriving instance ToHttpApiData TS

instance PersistField (ID a) where
  toPersistValue = toPersistValue . view _ID
  fromPersistValue = map (view $ re _ID) . fromPersistValue
instance PersistFieldSql (ID a) where
  sqlType _ = sqlType (Proxy :: Proxy Text)
deriving instance PathPiece (ID a)
deriving instance FromHttpApiData (ID a)
deriving instance ToHttpApiData (ID a)
