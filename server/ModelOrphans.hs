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

instance PersistField MessageSubtype where
  toPersistValue = toPersistValue . \ case
    BotMS              -> asText "bot_mesage"
    MeMS               -> "me_message"
    ChangedMS          -> "message_changed"
    DeletedMS          -> "message_deleted"
    ChannelJoinMS      -> "channel_join"
    ChannelLeaveMS     -> "channel_leave"
    ChannelTopicMS     -> "channel_topic"
    ChannelPurposeMS   -> "channel_purpose"
    ChannelNameMS      -> "channel_name"
    ChannelArchiveMS   -> "channel_archive"
    ChannelUnarchiveMS -> "channel_unarchive"
    GroupJoinMS        -> "group_join"
    GroupLeaveMS       -> "group_leave"
    GroupTopicMS       -> "group_topic"
    GroupPurposeMS     -> "group_purpose"
    GroupNameMS        -> "group_name"
    GroupArchiveMS     -> "group_archive"
    GroupUnarchiveMS   -> "group_unarchive"
    FileShareMS        -> "file_share"
    FileCommentMS      -> "file_comment"
    FileMentionMS      -> "file_mention"
  fromPersistValue = fromPersistValue >=> \ case
    "bot_mesage"        -> pure BotMS
    "me_message"        -> pure MeMS
    "message_changed"   -> pure ChangedMS
    "message_deleted"   -> pure DeletedMS
    "channel_join"      -> pure ChannelJoinMS
    "channel_leave"     -> pure ChannelLeaveMS
    "channel_topic"     -> pure ChannelTopicMS
    "channel_purpose"   -> pure ChannelPurposeMS
    "channel_name"      -> pure ChannelNameMS
    "channel_archive"   -> pure ChannelArchiveMS
    "channel_unarchive" -> pure ChannelUnarchiveMS
    "group_join"        -> pure GroupJoinMS
    "group_leave"       -> pure GroupLeaveMS
    "group_topic"       -> pure GroupTopicMS
    "group_purpose"     -> pure GroupPurposeMS
    "group_name"        -> pure GroupNameMS
    "group_archive"     -> pure GroupArchiveMS
    "group_unarchive"   -> pure GroupUnarchiveMS
    "file_share"        -> pure FileShareMS
    "file_comment"      -> pure FileCommentMS
    "file_mention"      -> pure FileMentionMS
    other               -> Left $ "unrecognized message subtype enum " <> other

instance PersistFieldSql MessageSubtype where
  sqlType _ = SqlOther "message_subtype"

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
