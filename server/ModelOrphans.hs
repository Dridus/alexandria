{-# OPTIONS_GHC -fno-warn-orphans #-}
module ModelOrphans where

import ClassyPrelude
import Control.Lens (view, re)
import Data.Proxy (Proxy(Proxy))
import Database.Persist
import Database.Persist.Sql
import Slack
import Web.HttpApiData
       ( FromHttpApiData(parseUrlPiece, parseHeader, parseQueryParam)
       , ToHttpApiData(toUrlPiece, toHeader, toQueryParam) )
import Web.PathPieces (PathPiece(fromPathPiece, toPathPiece))

instance PersistField MessageSubtype where
  toPersistValue = toPersistValue . messageSubtypeToText
  fromPersistValue = fromPersistValue >=> messageSubtypeFromText

instance PersistFieldSql MessageSubtype where
  sqlType _ = SqlOther "message_subtype"

instance PersistField TS where
  toPersistValue = toPersistValue . unTS
  fromPersistValue = fromPersistValue >=> ts
instance PersistFieldSql TS where
  sqlType _ = sqlType (Proxy :: Proxy Text)
instance PathPiece TS where
  fromPathPiece = either (const Nothing) Just . ts
  toPathPiece = unTS
instance FromHttpApiData TS where
  parseUrlPiece = ts
  parseHeader = ts . decodeUtf8
  parseQueryParam = ts
instance ToHttpApiData TS where
  toUrlPiece = unTS
  toHeader = encodeUtf8 . unTS
  toQueryParam = unTS

instance PersistField (ID a) where
  toPersistValue = toPersistValue . view _ID
  fromPersistValue = map (view $ re _ID) . fromPersistValue
instance PersistFieldSql (ID a) where
  sqlType _ = sqlType (Proxy :: Proxy Text)
deriving instance PathPiece (ID a)
deriving instance FromHttpApiData (ID a)
deriving instance ToHttpApiData (ID a)
