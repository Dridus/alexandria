module Slack where

import           ClassyPrelude
import           Control.Monad (mfilter)
import           Control.Monad.Except (ExceptT(ExceptT))
import           Control.Lens (Getter, Prism', prism', view, to)
import           Control.Lens.TH (makeLenses, makePrisms)
import           Data.Aeson ((.=), Value(Number, String), Object, FromJSON(parseJSON), ToJSON(toJSON), object)
import qualified Data.Aeson.BetterErrors as ABE
import qualified Data.Aeson.BetterErrors.Internal as ABEI
import qualified Data.HashMap.Strict as HM
import           Data.Proxy (Proxy(Proxy))
import           Data.Scientific (toBoundedInteger)
import           Data.Text (splitOn)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           TextShow (TextShow(showb))
import           TextShow.TH (deriveTextShow)

import           TextShowOrphans ()

data TS = TS { _tsTime :: Word32, _tsUnique :: Word32 } deriving (Eq, Ord, Read, Show)
asTS :: ABE.Parse Text TS
asTS = ABE.asText >>= either ABE.throwCustomError pure . ts
ts :: Text -> Either Text TS
ts t = case splitOn "." t of
  [readMay -> Just time, readMay -> Just unique] -> Right $ TS time unique
  other -> Left $ "couldn't parse as a time.unique pair, got parts: " <> tshow other
unTS :: TS -> Text
unTS (TS t u) = tshow t <> "." <> tshow u
tsToUTCTime :: TS -> UTCTime
tsToUTCTime (TS t _) = posixSecondsToUTCTime (fromIntegral t)
instance FromJSON TS where parseJSON = ABE.toAesonParser id asTS
instance ToJSON TS where toJSON = toJSON . unTS
makeLenses ''TS
deriveTextShow ''TS

newtype Time = Time { unTime :: Word32 } deriving (Eq, Ord, Read, Show)
asTime :: ABE.Parse Text Time
asTime =
  ABEI.ParseT . ReaderT $ \ (ABEI.ParseReader path value) ->
    ExceptT . Identity $ case value of
      String s | Just t <- readMay s ->
        Right (Time t)
      String s ->
        Left (ABEI.BadSchema (toList path) . ABEI.CustomError $ "tried to parse " <> tshow s <> " as time but couldn't")
      Number s | Just w32 <- toBoundedInteger s ->
        Right (Time w32)
      Number s ->
        Left . ABEI.BadSchema (toList path) . ABEI.CustomError $ "out of bound unix time " <> tshow s
      other ->
        Left . ABEI.BadSchema (toList path) . ABEI.CustomError $ "expected a time as string or number not " <> tshow other
instance ToJSON Time where toJSON = toJSON . unTime
makePrisms ''Time
deriveTextShow ''Time

newtype ID a = ID { unID :: Text } deriving (Eq, Ord, Read, Show)
asID :: ABE.Parse Text (ID a)
asID = ID <$> ABE.asText
instance FromJSON (ID a) where parseJSON = ABE.toAesonParser id asID
instance ToJSON (ID a) where toJSON = String . unID
makePrisms ''ID
deriveTextShow ''ID

idedName :: Getter s Text -> Getter s (ID k) -> (s -> Text)
idedName name ident s = view name s ++ " <" ++ view (ident . to unID) s ++ ">"

data Response a = ResponseNotOk !Text | ResponseOk a

data RtmStartRequest = RtmStartRequest { rtmStartToken :: Text }

data RtmStartRp = RtmStartRp
  { _rtmStartUrl      :: Text
  , _rtmStartSelf     :: Self
  , _rtmStartTeam     :: Team
  , _rtmStartUsers    :: [User]
  , _rtmStartChannels :: [Channel]
  , _rtmStartGroups   :: [Group]
  , _rtmStartIMs      :: [IM]
  , _rtmStartBots     :: [Bot] }

testRtmStartRp :: RtmStartRp
testRtmStartRp = RtmStartRp
  { _rtmStartUrl      = "url"
  , _rtmStartSelf     = Self (ID "UEMBOT") "Embot" mempty (Time 0) PresenceActive
  , _rtmStartTeam     = Team (ID "TTEAM") "Team" Nothing "domain" Nothing False mempty
  , _rtmStartUsers    = []
  , _rtmStartChannels = []
  , _rtmStartGroups   = []
  , _rtmStartIMs      = []
  , _rtmStartBots     = [] }

data Self = Self
  { _selfID             :: ID User
  , _selfName           :: Text
  , _selfPrefs          :: Object
  , _selfCreated        :: Time
  , _selfManualPresence :: Presence }

data Presence = PresenceActive | PresenceAway

data Team = Team
  { _teamID                :: ID Team
  , _teamName              :: Text
  , _teamEmailDomain       :: Maybe Text
  , _teamDomain            :: Text
  , _teamMsgEditWindowMins :: Maybe Int
  , _teamOverStorageLimit  :: Bool
  , _teamPrefs             :: Object }

data User = User
  { _userID                :: ID User
  , _userName              :: Text
  , _userRealName          :: Maybe Text
  , _userDeleted           :: Bool
  , _userColor             :: Maybe Text
  , _userTz                :: Maybe Tz
  , _userProfile           :: Profile
  , _userIsAdmin           :: Bool
  , _userIsOwner           :: Bool
  , _userIsPrimaryOwner    :: Bool
  , _userIsRestricted      :: Bool
  , _userIsUltraRestricted :: Bool
  , _userHas2fa            :: Bool
  , _userTwoFactorType     :: Maybe Text
  , _userHasFiles          :: Bool
  , _userPresence          :: Maybe Presence }

data Tz = Tz
  { _tz       :: Text
  , _tzLabel  :: Text
  , _tzOffset :: Int }

data Profile = Profile
  { _profileFirstName :: Maybe Text
  , _profileLastName  :: Maybe Text
  , _profileRealName  :: Maybe Text
  , _profileRealNameNormalized :: Maybe Text
  , _profileEmail     :: Maybe Text
  , _profileSkype     :: Maybe Text
  , _profilePhone     :: Maybe Text 
  , _profileImages    :: IntMap Text }

data Channel = Channel
  { _channelID          :: ID Channel
  , _channelName        :: Text
  , _channelCreated     :: Time
  , _channelCreator     :: ID User
  , _channelIsArchived  :: Bool
  , _channelIsGeneral   :: Bool
  , _channelMembers     :: [ID User]
  , _channelTopic       :: Maybe (SlackTracked Text)
  , _channelPurpose     :: Maybe (SlackTracked Text)
  , _channelIsMember    :: Bool
  , _channelLastRead    :: Maybe TS
  , _channelLatest      :: Maybe Message
  , _channelUnreadCount :: Maybe Int }

data Group = Group
  { _groupID          :: ID Group
  , _groupName        :: Text
  , _groupCreated     :: Time
  , _groupCreator     :: ID User
  , _groupIsArchived  :: Bool
  , _groupMembers     :: [ID User]
  , _groupTopic       :: Maybe (SlackTracked Text)
  , _groupPurpose     :: Maybe (SlackTracked Text)
  , _groupIsOpen      :: Bool
  , _groupLastRead    :: Maybe TS
  , _groupLatest      :: Maybe Message
  , _groupUnreadCount :: Maybe Int }

data IM = IM
  { _imID            :: ID IM
  , _imUser          :: ID User
  , _imCreated       :: Time
  , _imIsUserDeleted :: Bool
  , _imIsOpen        :: Bool
  , _imLastRead      :: Maybe TS
  , _imLatest        :: Maybe Message
  , _imUnreadCount   :: Maybe Int }

data Bot = Bot
  { _botID    :: ID Bot
  , _botName  :: Text
  , _botIcons :: HM.HashMap Text Text }

data Chat

data Message = Message
  { _messageChat         :: Maybe (ID Chat)
  , _messageUser         :: Maybe (ID User)
  , _messageSubtype      :: Maybe MessageSubtype
  , _messageText         :: Maybe Text
  , _messageTS           :: TS
  , _messageEdited       :: Maybe MessageEdited
  , _messageDeletedTS    :: Maybe TS
  , _messageEventTS      :: Maybe TS
  , _messageHidden       :: Bool
  , _messageAttachments  :: [Attachment]
  , _messageInviter      :: Maybe (ID User)
  , _messageIsStarred    :: Bool
  , _messagePinnedTo     :: [ID Channel]
  , _messageReactions    :: [MessageReaction] }

testMessage :: ID Chat -> ID User -> Text -> Message
testMessage chat from text = Message
  { _messageChat         = Just chat
  , _messageUser         = Just from
  , _messageSubtype      = Nothing
  , _messageText         = Just text
  , _messageTS           = TS 0 0
  , _messageEdited       = Nothing
  , _messageDeletedTS    = Nothing
  , _messageEventTS      = Nothing
  , _messageHidden       = False
  , _messageAttachments  = []
  , _messageInviter      = Nothing
  , _messageIsStarred    = False
  , _messagePinnedTo     = []
  , _messageReactions    = [] }

data MessageSubtype
  = BotMS | MeMS | ChangedMS | DeletedMS
  | ChannelJoinMS | ChannelLeaveMS | ChannelTopicMS | ChannelPurposeMS | ChannelNameMS | ChannelArchiveMS | ChannelUnarchiveMS
  | GroupJoinMS   | GroupLeaveMS   | GroupTopicMS   | GroupPurposeMS   | GroupNameMS   | GroupArchiveMS   | GroupUnarchiveMS
  | FileShareMS | FileCommentMS | FileMentionMS
  | PinnedItemMS | ReminderAddMS | ReminderDeleteMS | BotAddMS

data MessageEdited = MessageEdited
  { _messageEditedUser :: ID User
  , _messageEditedTS   :: TS }

data MessageReaction = MessageReaction
  { _messageReactionName :: Text
  , _messageReactionCount :: Int
  , _messageReactionUsers :: [ID User] }

data Attachment = Attachment
  { _attachmentFallback    :: Maybe Text
  , _attachmentColor       :: Maybe Text
  , _attachmentPretext     :: Maybe Text
  , _attachmentAuthorName  :: Maybe Text
  , _attachmentAuthorLink  :: Maybe Text
  , _attachmentAuthorIcon  :: Maybe Text
  , _attachmentTitle       :: Maybe Text
  , _attachmentTitleLink   :: Maybe Text
  , _attachmentText        :: Maybe Text
  , _attachmentFields      :: [AttachmentField]
  , _attachmentFromUrl     :: Maybe Text
  , _attachmentThumbUrl    :: Maybe Text
  , _attachmentThumbWidth  :: Maybe Int
  , _attachmentThumbHeight :: Maybe Int
  , _attachmentId          :: Int }

data AttachmentField = AttachmentField
  { _fieldTitle :: Text
  , _fieldValue :: Text
  , _fieldShort :: Bool }

data SlackTracked a = SlackTracked
  { _trackedValue   :: a
  , _trackedCreator :: ID User
  , _trackedLastSet :: Time }

data File = File
  { _fileID                 :: ID File
  , _fileCreated            :: Time
  , _fileTimestamp          :: Time
  , _fileName               :: Text
  , _fileTitle              :: Text
  , _fileMimeType           :: Text
  , _fileFileType           :: Text
  , _filePrettyType         :: Text
  , _fileUser               :: ID User
  , _fileMode               :: FileMode
  , _fileEditable           :: Bool
  , _fileIsExternal         :: Bool
  , _fileExternalType       :: Text
  , _fileSize               :: Word64
  , _fileURL                :: Text
  , _fileURLDownload        :: Text
  , _fileURLPrivate         :: Text
  , _fileURLPrivateDownload :: Text
  , _fileThumb              :: IntMap Text
  , _filePermalink          :: Text
  , _fileEditLink           :: Text
  , _filePreview            :: Text
  , _filePreviewHighlight   :: Text
  , _fileLines              :: Int
  , _fileLinesMore          :: Int
  , _fileIsPublic           :: Bool
  , _filePublicURLShared    :: Bool
  , _fileChannels           :: [ID Channel]
  , _fileGroups             :: [ID Group]
  , _fileIMs                :: [ID IM]
  , _fileInitialComment     :: Maybe Message
  , _fileNumStars           :: Int
  , _fileIsStarred          :: Bool }

data FileMode
  = FileHosted
  | FileExternal
  | FileSnippet
  | FilePost

data FileComment = FileComment
  { _fileCommentID        :: ID FileComment
  , _fileCommentTimestamp :: Time
  , _fileCommentUser      :: ID User
  , _fileCommentComment   :: Text }

data RtmEvent
  = RtmHello
  | RtmReplyOk Word64 (Maybe TS) (Maybe Text)
  | RtmReplyNotOk Word64 Int32 Text
  | RtmMessage Message
  | RtmChannelMarked (ChatMarked Channel)
  | RtmChannelCreated Channel
  | RtmChannelJoined Channel
  | RtmChannelLeft (ID Channel)
  | RtmChannelDeleted (ID Channel)
  | RtmChannelRenamed (ChatRenamed Channel)
  | RtmChannelArchive (ChatUser Channel)
  | RtmChannelUnarchive (ChatUser Channel)
  | RtmChannelHistoryChanged (ChatHistoryChanged Channel)
  | RtmIMCreated IMCreated
  | RtmIMOpen (ChatUser IM)
  | RtmIMClose (ChatUser IM)
  | RtmIMMarked (ChatMarked IM)
  | RtmIMHistoryChanged (ChatHistoryChanged IM)
  | RtmGroupJoined Group
  | RtmGroupLeft (ID Group)
  | RtmGroupOpen (ChatUser Group)
  | RtmGroupClose (ChatUser Group)
  | RtmGroupArchive (ID Group)
  | RtmGroupUnarchive (ID Group)
  | RtmGroupRename (ChatRenamed Group)
  | RtmGroupMarked (ChatMarked Group)
  | RtmGroupHistoryChanged (ChatHistoryChanged Group)
  | RtmFileCreated File
  | RtmFileShared File
  | RtmFileUnshared File
  | RtmFilePublic File
  | RtmFilePrivate (ID File)
  | RtmFileChange File
  | RtmFileDeleted FileDeleted
  | RtmFileCommentAdded FileCommentUpdated
  | RtmFileCommentEdited FileCommentUpdated
  | RtmFileCommentDeleted FileCommentDeleted
  | RtmPresenceChange PresenceChange
  | RtmManualPresenceChange Presence
  | RtmPrefChange PrefChange
  | RtmUserChange User
  | RtmUserTyping (ChatUser Chat)
  | RtmTeamJoin User
  | RtmStarAdded Star
  | RtmStarRemoved Star
  | RtmEmojiChanged TS
  | RtmCommandsChanged TS
  | RtmTeamPrefChange PrefChange
  | RtmTeamRename Text
  | RtmTeamDomainChange TeamDomainChange
  | RtmEmailDomainChanged EmailDomainChanged
  | RtmBotAdded Bot
  | RtmBotChanged Bot
  | RtmAccountsChanged

data ChatMarked a = ChatMarked
  { _chatMarkedChannel :: ID a
  , _chatMarkedTS      :: TS }

data ChatUser a = ChatUser
  { _chatUserUser      :: ID User
  , _chatUserChannelID :: ID a }

data ChatRenamed a = ChatRenamed
  { _chatRenamedChannelID :: ID a
  , _chatRenamedName      :: Text }

data ChatHistoryChanged a = ChatHistoryChanged
  { _chatHistoryChangedLatest  :: Text
  , _chatHistoryChangedTS      :: TS
  , _chatHistoryChangedEventTS :: TS }

data IMCreated = IMCreated
  { _imCreatedUser    :: ID User
  , _imCreatedChannel :: IM }

data FileDeleted = FileDeleted
  { _fileDeletedFileID  :: ID File
  , _fileDeletedEventTS :: TS }

data FileCommentUpdated = FileCommentUpdated
  { _fileCommentUpdatedFile    :: File
  , _fileCommentUpdatedComment :: FileComment }

data FileCommentDeleted = FileCommentDeleted
  { _fileCommentDeletedFile    :: File
  , _fileCommentDeletedComment :: ID FileComment }

data PresenceChange = PresenceChange
  { _presenceChangeUser     :: ID User
  , _presenceChangePresence :: Presence }

data PrefChange = PrefChange
  { _prefChangeName  :: Text
  , _prefChangeValue :: Value }

data Star = Star
  { _starUser    :: Text
  , _starItem    :: StarItem
  , _starEventTS :: TS }

data StarItem
  = StarItemMessage Message
  | StarItemFile File
  | StarItemFileComment File FileComment
  | StarItemChannel (ID Channel)
  | StarItemIM (ID IM)
  | StarItemGroup (ID Group)

data TeamDomainChange = TeamDomainChange
  { _teamDomainChangeUrl    :: Text
  , _teamDomainChangeDomain :: Text }

data EmailDomainChanged = EmailDomainChanged
  { _emailDomainChangedEmailDomain :: Text
  , _emailDomainChangedEventTS     :: TS }

data RtmSendMessage = RtmSendMessage
  { _sendMessageSeqnum :: Word64
  , _sendMessageChat   :: ID Chat
  , _sendMessageText   :: Text }

class SlackTyped a where
  isTypedID :: Proxy a -> ID b -> Bool
instance SlackTyped Channel where
  isTypedID _ = isPrefixOf "C" . unID
instance SlackTyped File where
  isTypedID _ (ID t) = "F" `isPrefixOf` t && not ("Fc" `isPrefixOf` t)
instance SlackTyped FileComment where
  isTypedID _ (ID t) = "Fc" `isPrefixOf` t
instance SlackTyped Group where
  isTypedID _ = isPrefixOf "G" . unID
instance SlackTyped Chat where
   isTypedID _ i
    =  isTypedID (Proxy :: Proxy Channel) i
    || isTypedID (Proxy :: Proxy IM) i
    || isTypedID (Proxy :: Proxy Group) i
instance SlackTyped IM where
  isTypedID _ = isPrefixOf "D" . unID
instance SlackTyped User where
  isTypedID _ = isPrefixOf "U" . unID

typedID :: forall a. SlackTyped a => Prism' (ID Chat) (ID a)
typedID = prism' (\ (ID t) -> ID t) asTypedID

asTypedID :: forall a b. SlackTyped b => ID a -> Maybe (ID b)
asTypedID i =
  if isTypedID (Proxy :: Proxy b) i
    then Just (ID . unID $ i)
    else Nothing

asChannelID :: ID Chat -> Maybe (ID Channel)
asChannelID = asTypedID
asGroupID :: ID Chat -> Maybe (ID Group)
asGroupID = asTypedID
asIMID :: ID Chat -> Maybe (ID IM)
asIMID = asTypedID

deriving instance Eq RtmStartRequest
deriving instance Eq RtmStartRp
deriving instance Eq Self
deriving instance Eq Team
deriving instance Eq User
deriving instance Eq Tz
deriving instance Eq Profile
deriving instance Eq Chat
deriving instance Eq Channel
deriving instance Eq Group
deriving instance Eq IM
deriving instance Eq Bot
deriving instance Eq MessageSubtype
deriving instance Enum MessageSubtype
deriving instance Ord MessageSubtype
deriving instance Bounded MessageSubtype
deriving instance Eq MessageReaction
deriving instance Eq Message
deriving instance Eq MessageEdited
deriving instance Eq Attachment
deriving instance Eq AttachmentField
deriving instance Eq a => Eq (SlackTracked a)
deriving instance Eq FileMode
deriving instance Eq File
deriving instance Eq FileComment
deriving instance Eq RtmEvent
deriving instance Eq a => Eq (ChatMarked a)
deriving instance Eq a => Eq (ChatUser a)
deriving instance Eq a => Eq (ChatRenamed a)
deriving instance Eq a => Eq (ChatHistoryChanged a)
deriving instance Eq IMCreated
deriving instance Eq FileDeleted
deriving instance Eq FileCommentUpdated
deriving instance Eq FileCommentDeleted
deriving instance Eq Presence
deriving instance Eq PresenceChange
deriving instance Eq PrefChange
deriving instance Eq Star
deriving instance Eq StarItem
deriving instance Eq TeamDomainChange
deriving instance Eq EmailDomainChanged
deriving instance Eq RtmSendMessage

makeLenses ''RtmStartRequest
makeLenses ''RtmStartRp
makeLenses ''Self
makeLenses ''Team
makeLenses ''User
makeLenses ''Tz
makeLenses ''Profile
makeLenses ''Channel
makeLenses ''Group
makeLenses ''IM
makeLenses ''Bot
makeLenses ''MessageReaction
makeLenses ''Message
makeLenses ''MessageEdited
makeLenses ''Attachment
makeLenses ''AttachmentField
makeLenses ''SlackTracked
makeLenses ''File
makeLenses ''FileComment
makePrisms ''RtmEvent
makeLenses ''ChatMarked
makeLenses ''ChatUser
makeLenses ''ChatRenamed
makeLenses ''ChatHistoryChanged
makeLenses ''IMCreated
makeLenses ''FileDeleted
makeLenses ''FileCommentUpdated
makeLenses ''FileCommentDeleted
makeLenses ''PresenceChange
makeLenses ''PrefChange
makeLenses ''Star
makePrisms ''StarItem
makeLenses ''TeamDomainChange
makeLenses ''EmailDomainChanged
makeLenses ''RtmSendMessage


instance TextShow Chat where
  showb _ = "Chat"

deriveTextShow ''RtmStartRequest
deriveTextShow ''RtmStartRp
deriveTextShow ''Self
deriveTextShow ''Presence
deriveTextShow ''Team
deriveTextShow ''User
deriveTextShow ''Tz
deriveTextShow ''Profile
deriveTextShow ''Channel
deriveTextShow ''Group
deriveTextShow ''IM
deriveTextShow ''Bot
deriveTextShow ''Message
deriveTextShow ''MessageSubtype
deriveTextShow ''MessageEdited
deriveTextShow ''MessageReaction
deriveTextShow ''Attachment
deriveTextShow ''AttachmentField
deriveTextShow ''SlackTracked
deriveTextShow ''File
deriveTextShow ''FileMode
deriveTextShow ''FileComment
deriveTextShow ''RtmEvent
deriveTextShow ''ChatMarked
deriveTextShow ''ChatUser
deriveTextShow ''ChatRenamed
deriveTextShow ''ChatHistoryChanged
deriveTextShow ''IMCreated
deriveTextShow ''FileDeleted
deriveTextShow ''FileCommentUpdated
deriveTextShow ''FileCommentDeleted
deriveTextShow ''PresenceChange
deriveTextShow ''PrefChange
deriveTextShow ''Star
deriveTextShow ''StarItem
deriveTextShow ''TeamDomainChange
deriveTextShow ''EmailDomainChanged
deriveTextShow ''RtmSendMessage

instance ToJSON RtmStartRequest where
  toJSON (RtmStartRequest { .. }) = object
    [ ("token", toJSON rtmStartToken) ]

asResponse :: ABE.Parse Text a -> ABE.Parse Text (Response a)
asResponse parseInner =
  ABE.key "ok" ABE.asBool >>= \ case
    True  -> ResponseOk <$> parseInner
    False -> ResponseNotOk <$> ABE.keyOrDefault "error" "unknown error" ABE.asText

asRtmStartRp :: ABE.Parse Text RtmStartRp
asRtmStartRp =
  RtmStartRp
    <$> ABE.key "url" ABE.asText
    <*> ABE.key "self" asSelf
    <*> ABE.key "team" asTeam
    <*> ABE.key "users" (ABE.eachInArray asUser)
    <*> ABE.key "channels" (ABE.eachInArray asChannel)
    <*> ABE.key "groups" (ABE.eachInArray asGroup)
    <*> ABE.key "ims" (ABE.eachInArray asIM)
    <*> ABE.key "bots" (ABE.eachInArray asBot)

asSelf :: ABE.Parse Text Self
asSelf =
  Self
    <$> ABE.key "id" asID
    <*> ABE.key "name" ABE.asText
    <*> ABE.key "prefs" ABE.asObject
    <*> ABE.key "created" asTime
    <*> ABE.key "manual_presence" asPresence

asPresence :: ABE.Parse Text Presence
asPresence =
  ABE.asText >>= \ case
    "active" -> pure PresenceActive
    "away"   -> pure PresenceAway
    other    -> ABE.throwCustomError $ "unknown presence value " <> other

asTeam :: ABE.Parse Text Team
asTeam =
  Team
    <$> ABE.key "id" asID
    <*> ABE.key "name" ABE.asText
    <*> (mfilter (not . null) <$> ABE.keyMay "email_domain" ABE.asText)
    <*> ABE.key "domain" ABE.asText
    <*> (mfilter (not . (==) (-1)) <$> ABE.keyMay "msg_edit_window_mins" ABE.asIntegral)
    <*> ABE.key "over_storage_limit" ABE.asBool
    <*> ABE.key "prefs" ABE.asObject

asUser :: ABE.Parse Text User
asUser =
  User
    <$> ABE.key "id" asID
    <*> ABE.key "name" ABE.asText
    <*> ABE.keyMay "real_name" ABE.asText
    <*> ABE.key "deleted" ABE.asBool
    <*> ABE.keyMay "color" ABE.asText
    <*> ( ( (,,) <$> (join <$> ABE.keyMay "tz" (ABE.perhaps ABE.asText))
                 <*> ABE.keyMay "tz_label" ABE.asText
                 <*> ABE.keyMay "tz_offset" ABE.asIntegral )
        >>= \ (tzMay, labelMay, offsMay) -> pure $ Tz <$> tzMay <*> labelMay <*> offsMay )
    <*> ABE.key "profile" asProfile
    <*> ABE.keyOrDefault "is_admin" False ABE.asBool
    <*> ABE.keyOrDefault "is_owner" False ABE.asBool
    <*> ABE.keyOrDefault "is_primary_owner" False ABE.asBool
    <*> ABE.keyOrDefault "is_restricted" False ABE.asBool
    <*> ABE.keyOrDefault "is_ultra_restricted" False ABE.asBool
    <*> ABE.keyOrDefault "has_2fa" False ABE.asBool
    <*> ABE.keyMay "two_factor_type" ABE.asText
    <*> ABE.keyOrDefault "has_files" False ABE.asBool
    <*> ABE.keyMay "presence" asPresence

asProfile :: ABE.Parse Text Profile
asProfile =
  Profile
    <$> ABE.keyMay "first_name" ABE.asText
    <*> ABE.keyMay "last_name" ABE.asText
    <*> ABE.keyMay "real_name" ABE.asText
    <*> ABE.keyMay "real_name_normalized" ABE.asText
    <*> ABE.keyMay "email" ABE.asText
    <*> ABE.keyMay "skype" ABE.asText
    <*> ABE.keyMay "phone" ABE.asText
    <*> asThumbs

asThumbs :: ABE.Parse Text (IntMap Text)
asThumbs = 
  mapFromList . catMaybes
    <$> mapM (\ n -> map (n, ) <$> ABE.keyMay ("image_" <> tshow n) ABE.asText)
             [24 :: Int, 32, 48, 72, 192, 512]

asChannel :: ABE.Parse Text Channel
asChannel =
  Channel
    <$> ABE.key "id" asID
    <*> ABE.key "name" ABE.asText
    <*> ABE.key "created" asTime
    <*> ABE.key "creator" asID
    <*> ABE.key "is_archived" ABE.asBool
    <*> ABE.keyOrDefault "is_general" False ABE.asBool
    <*> ABE.keyOrDefault "members" [] (ABE.eachInArray asID)
    <*> ABE.keyMay "topic" (asSlackTracked ABE.asText)
    <*> ABE.keyMay "purpose" (asSlackTracked ABE.asText)
    <*> ABE.keyOrDefault "is_member" False ABE.asBool
    <*> ABE.keyMay "last_read" asTS
    <*> ABE.keyMay "latest" asMessage
    <*> ABE.keyMay "unread_count" ABE.asIntegral

asGroup :: ABE.Parse Text Group
asGroup =
  Group
    <$> ABE.key "id" asID
    <*> ABE.key "name" ABE.asText
    <*> ABE.key "created" asTime
    <*> ABE.key "creator" asID
    <*> ABE.key "is_archived" ABE.asBool
    <*> ABE.keyOrDefault "members" [] (ABE.eachInArray asID)
    <*> ABE.keyMay "topic" (asSlackTracked ABE.asText)
    <*> ABE.keyMay "purpose" (asSlackTracked ABE.asText)
    <*> ABE.keyOrDefault "is_open" False ABE.asBool
    <*> ABE.keyMay "last_read" asTS
    <*> ABE.keyMay "latest" asMessage
    <*> ABE.keyMay "unread_count" ABE.asIntegral

asIM :: ABE.Parse Text IM
asIM =
  IM
    <$> ABE.key "id" asID
    <*> ABE.key "user" asID
    <*> ABE.key "created" asTime
    <*> ABE.keyOrDefault "is_user_deleted" False ABE.asBool
    <*> ABE.keyOrDefault "is_open" False ABE.asBool
    <*> ABE.keyMay "last_read" asTS
    <*> ABE.keyMay "latest" asMessage
    <*> ABE.keyMay "unread_count" ABE.asIntegral

asBot :: ABE.Parse Text Bot
asBot =
  Bot
    <$> ABE.key "id" asID
    <*> ABE.key "name" ABE.asText
    <*> ABE.keyOrDefault "icons" mempty (mapFromList <$> ABE.eachInObject ABE.asText)

asSlackTracked :: ABE.Parse Text a -> ABE.Parse Text (SlackTracked a)
asSlackTracked parseValue =
  SlackTracked
    <$> ABE.key "value" parseValue
    <*> ABE.key "creator" asID
    <*> ABE.key "last_set" asTime

asMessage :: ABE.Parse Text Message
asMessage =
  Message
    <$> ABE.keyMay "channel" asID
    <*> ABE.keyMay "user" asID
    <*> ABE.keyMay "subtype" asMessageSubtype
    <*> ABE.keyMay "text" ABE.asText
    <*> ABE.key "ts" asTS
    <*> ABE.keyMay "edited" asMessageEdited
    <*> ABE.keyMay "deleted_ts" asTS
    <*> ABE.keyMay "event_is" asTS
    <*> ABE.keyOrDefault "hidden" False ABE.asBool
    <*> ABE.keyOrDefault "attachments" [] (ABE.eachInArray asAttachment)
    <*> ABE.keyMay "inviter" asID
    <*> ABE.keyOrDefault "is_starred" False ABE.asBool
    <*> ABE.keyOrDefault "pinned_to" [] (ABE.eachInArray asID)
    <*> ABE.keyOrDefault "reactions" [] (ABE.eachInArray asMessageReaction)

asMessageSubtype :: ABE.Parse Text MessageSubtype
asMessageSubtype = ABE.asText >>= either ABE.throwCustomError pure . messageSubtypeFromText

messageSubtypeFromText :: Text -> Either Text MessageSubtype
messageSubtypeFromText = \ case
  "bot_message"       -> Right BotMS
  "me_message"        -> Right MeMS
  "message_changed"   -> Right ChangedMS
  "message_deleted"   -> Right DeletedMS
  "channel_join"      -> Right ChannelJoinMS
  "channel_leave"     -> Right ChannelLeaveMS
  "channel_topic"     -> Right ChannelTopicMS
  "channel_purpose"   -> Right ChannelPurposeMS
  "channel_name"      -> Right ChannelNameMS
  "channel_archive"   -> Right ChannelArchiveMS
  "channel_unarchive" -> Right ChannelUnarchiveMS
  "group_join"        -> Right GroupJoinMS
  "group_leave"       -> Right GroupLeaveMS
  "group_topic"       -> Right GroupTopicMS
  "group_purpose"     -> Right GroupPurposeMS
  "group_name"        -> Right GroupNameMS
  "group_archive"     -> Right GroupArchiveMS
  "group_unarchive"   -> Right GroupUnarchiveMS
  "file_share"        -> Right FileShareMS
  "file_comment"      -> Right FileCommentMS
  "file_mention"      -> Right FileMentionMS
  "pinned_item"       -> Right PinnedItemMS
  "reminder_add"      -> Right ReminderAddMS
  "reminder_delete"   -> Right ReminderDeleteMS
  "bot_add"           -> Right BotAddMS
  other               -> Left $ "unknown message subtype " <> other

messageSubtypeToText :: MessageSubtype -> Text
messageSubtypeToText = \ case
  BotMS              -> "bot_message"
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
  PinnedItemMS       -> "pinned_item"
  ReminderAddMS      -> "reminder_add"
  ReminderDeleteMS   -> "reminder_delete"
  BotAddMS           -> "bot_add"

instance FromJSON MessageSubtype where
  parseJSON = ABE.toAesonParser id asMessageSubtype

instance ToJSON MessageSubtype where
  toJSON = toJSON . messageSubtypeToText

asMessageEdited :: ABE.Parse Text MessageEdited
asMessageEdited =
  MessageEdited
    <$> ABE.key "user" asID
    <*> ABE.key "ts" asTS

asMessageReaction :: ABE.Parse Text MessageReaction
asMessageReaction =
  MessageReaction
    <$> ABE.key "name" ABE.asText
    <*> ABE.key "count" ABE.asIntegral
    <*> ABE.key "users" (ABE.eachInArray asID)

asAttachment :: ABE.Parse Text Attachment
asAttachment =
  Attachment
    <$> ABE.keyMay "fallback" ABE.asText
    <*> ABE.keyMay "color" ABE.asText
    <*> ABE.keyMay "pretext" ABE.asText
    <*> ABE.keyMay "author_name" ABE.asText
    <*> ABE.keyMay "author_link" ABE.asText
    <*> ABE.keyMay "author_icon" ABE.asText
    <*> ABE.keyMay "title" ABE.asText
    <*> ABE.keyMay "title_link" ABE.asText
    <*> ABE.keyMay "text" ABE.asText
    <*> ABE.keyOrDefault "fields" [] (ABE.eachInArray asAttachmentField)
    <*> ABE.keyMay "from_url" ABE.asText
    <*> ABE.keyMay "thumb_url" ABE.asText
    <*> ABE.keyMay "thumb_width" ABE.asIntegral
    <*> ABE.keyMay "thumb_height" ABE.asIntegral
    <*> ABE.keyOrDefault "id" 1 ABE.asIntegral -- FIXME? this defaulting is a lie!

asAttachmentField :: ABE.Parse Text AttachmentField
asAttachmentField =
  AttachmentField
    <$> ABE.key "title" ABE.asText
    <*> ABE.key "value" ABE.asText
    <*> ABE.key "short" ABE.asBool

asFile :: ABE.Parse Text File
asFile =
  File
    <$> ABE.key "id" asID
    <*> ABE.key "created" asTime
    <*> ABE.key "timestamp" asTime
    <*> ABE.key "name" ABE.asText
    <*> ABE.key "title" ABE.asText
    <*> ABE.key "mimetype" ABE.asText
    <*> ABE.key "filetype" ABE.asText
    <*> ABE.key "pretty_type" ABE.asText
    <*> ABE.key "user" asID
    <*> ABE.key "mode" asFileMode
    <*> ABE.key "editable" ABE.asBool
    <*> ABE.key "is_external" ABE.asBool
    <*> ABE.key "external_type" ABE.asText
    <*> ABE.key "size" ABE.asIntegral
    <*> ABE.key "url" ABE.asText
    <*> ABE.key "url_download" ABE.asText
    <*> ABE.key "url_private" ABE.asText
    <*> ABE.key "url_private_download" ABE.asText
    <*> asThumbs
    <*> ABE.key "permalink" ABE.asText
    <*> ABE.key "edit_link" ABE.asText
    <*> ABE.key "preview" ABE.asText
    <*> ABE.key "preview_highlight" ABE.asText
    <*> ABE.key "lines" ABE.asIntegral
    <*> ABE.key "lines_more" ABE.asIntegral
    <*> ABE.key "is_public" ABE.asBool
    <*> ABE.key "public_url_shared" ABE.asBool
    <*> ABE.keyOrDefault "channels" [] (ABE.eachInArray asID)
    <*> ABE.keyOrDefault "groups" [] (ABE.eachInArray asID)
    <*> ABE.keyOrDefault "ims" [] (ABE.eachInArray asID)
    <*> ABE.keyMay "initial_comment" asMessage
    <*> ABE.keyOrDefault "num_starts" 0 ABE.asIntegral
    <*> ABE.keyOrDefault "is_starred" False ABE.asBool

asFileMode :: ABE.Parse Text FileMode
asFileMode =
  ABE.asText >>= \ case
    "hosted"   -> pure FileHosted
    "external" -> pure FileExternal
    "snippet"  -> pure FileSnippet
    "post"     -> pure FilePost
    other      -> ABE.throwCustomError $ "unknown file mode " <> other

asFileComment :: ABE.Parse Text FileComment
asFileComment =
  FileComment
    <$> ABE.key "id" asID
    <*> ABE.key "timestamp" asTime
    <*> ABE.key "user" asID
    <*> ABE.key "comment" ABE.asText

asRtmEvent :: ABE.Parse Text RtmEvent
asRtmEvent =
  ABE.keyMay "reply_to" ABE.asIntegral >>= \ case
    Just seqnum -> 
      ABE.key "ok" ABE.asBool >>= \ case
        True  -> RtmReplyOk seqnum <$> ABE.keyMay "ts" asTS
                                   <*> ABE.keyMay "text" ABE.asText
        False -> ABE.key "error" ( RtmReplyNotOk seqnum <$> ABE.key "code" ABE.asIntegral
                                                        <*> ABE.key "msg" ABE.asText )
    Nothing ->
      ABE.key "type" ABE.asText >>= \ case
        "hello"                   -> pure RtmHello
        "message"                 -> RtmMessage <$> asMessage
        "channel_marked"          -> RtmChannelMarked <$> asChatMarked 
        "channel_created"         -> RtmChannelCreated <$> ABE.key "channel" asChannel
        "channel_joined"          -> RtmChannelJoined <$> ABE.key "channel" asChannel
        "channel_left"            -> RtmChannelLeft <$> ABE.key "channel" asID
        "channel_deleted"         -> RtmChannelDeleted <$> ABE.key "channel" asID
        "channel_rename"          -> RtmChannelRenamed <$> ABE.key "channel" asChatRenamed
        "channel_archive"         -> RtmChannelArchive <$> asChatUser
        "channel_unarchive"       -> RtmChannelUnarchive <$> asChatUser
        "channel_history_changed" -> RtmChannelHistoryChanged <$> asChatHistoryChanged
        "im_created"              -> RtmIMCreated <$> asIMCreated
        "im_open"                 -> RtmIMOpen <$> asChatUser
        "im_close"                -> RtmIMClose <$> asChatUser
        "im_marked"               -> RtmIMMarked <$> asChatMarked
        "im_history_changed"      -> RtmIMHistoryChanged <$> asChatHistoryChanged
        "group_joined"            -> RtmGroupJoined <$> ABE.key "channel" asGroup
        "group_left"              -> RtmGroupLeft <$> ABE.key "channel" asID
        "group_open"              -> RtmGroupOpen <$> asChatUser
        "group_close"             -> RtmGroupClose <$> asChatUser
        "group_archive"           -> RtmGroupArchive <$> ABE.key "channel" asID
        "group_unarchive"         -> RtmGroupUnarchive <$> ABE.key "channel" asID
        "group_rename"            -> RtmGroupRename <$> ABE.key "channel" asChatRenamed
        "group_marked"            -> RtmGroupMarked <$> asChatMarked
        "group_history_changed"   -> RtmGroupHistoryChanged <$> asChatHistoryChanged
        "file_created"            -> RtmFileCreated <$> ABE.key "file" asFile
        "file_shared"             -> RtmFileShared <$> ABE.key "file" asFile
        "file_unshared"           -> RtmFileUnshared <$> ABE.key "file" asFile
        "file_public"             -> RtmFilePublic <$> ABE.key "file" asFile
        "file_private"            -> RtmFilePrivate <$> ABE.key "file" asID
        "file_change"             -> RtmFileChange <$> ABE.key "file" asFile
        "file_deleted"            -> RtmFileDeleted <$> asFileDeleted
        "file_comment_added"      -> RtmFileCommentAdded <$> asFileCommentUpdated
        "file_comment_edited"     -> RtmFileCommentEdited <$> asFileCommentUpdated
        "file_comment_deleted"    -> RtmFileCommentDeleted <$> asFileCommentDeleted
        "presence_change"         -> RtmPresenceChange <$> asPresenceChange
        "manual_presence_change"  -> RtmManualPresenceChange <$> ABE.key "presence" asPresence
        "user_typing"             -> RtmUserTyping <$> asChatUser
        "pref_change"             -> RtmPrefChange <$> asPrefChange
        "user_change"             -> RtmUserChange <$> ABE.key "user" asUser
        "team_join"               -> RtmTeamJoin <$> ABE.key "user" asUser
        "star_added"              -> RtmStarAdded <$> asStar
        "star_removed"            -> RtmStarRemoved <$> asStar
        "emoji_changed"           -> RtmEmojiChanged <$> ABE.key "event_ts" asTS
        "commands_changed"        -> RtmCommandsChanged <$> ABE.key "event_ts" asTS
        "team_pref_change"        -> RtmTeamPrefChange <$> asPrefChange
        "team_rename"             -> RtmTeamRename <$> ABE.key "name" ABE.asText
        "team_domain_change"      -> RtmTeamDomainChange <$> asTeamDomainChange
        "email_domain_changed"    -> RtmEmailDomainChanged <$> asEmailDomainChanged
        "bot_added"               -> RtmBotAdded <$> ABE.key "bot" asBot
        "bot_changed"             -> RtmBotChanged <$> ABE.key "bot" asBot
        "accounts_changed"        -> pure RtmAccountsChanged
        other                     -> ABE.throwCustomError $ "unknown RTM event type " <> other


asChatMarked :: ABE.Parse Text (ChatMarked a)
asChatMarked =
  ChatMarked
    <$> ABE.key "channel" asID
    <*> ABE.key "ts" asTS

asChatUser :: ABE.Parse Text (ChatUser a)
asChatUser =
  ChatUser
    <$> ABE.key "channel" asID
    <*> ABE.key "user" asID

asChatRenamed :: ABE.Parse Text (ChatRenamed a)
asChatRenamed =
  ChatRenamed
    <$> ABE.key "id" asID
    <*> ABE.key "name" ABE.asText

asChatHistoryChanged :: ABE.Parse Text (ChatHistoryChanged a)
asChatHistoryChanged =
  ChatHistoryChanged
    <$> ABE.key "latest" ABE.asText
    <*> ABE.key "ts" asTS
    <*> ABE.key "event_ts" asTS

asIMCreated :: ABE.Parse Text IMCreated
asIMCreated =
  IMCreated
    <$> ABE.key "user" asID
    <*> ABE.key "channel" asIM

asFileDeleted :: ABE.Parse Text FileDeleted
asFileDeleted =
  FileDeleted
    <$> ABE.key "file_id" asID
    <*> ABE.key "event_ts" asTS

asFileCommentUpdated :: ABE.Parse Text FileCommentUpdated
asFileCommentUpdated =
  FileCommentUpdated
    <$> ABE.key "file" asFile
    <*> ABE.key "comment" asFileComment

asFileCommentDeleted :: ABE.Parse Text FileCommentDeleted
asFileCommentDeleted =
  FileCommentDeleted
    <$> ABE.key "file" asFile
    <*> ABE.key "comment" asID

asPresenceChange :: ABE.Parse Text PresenceChange
asPresenceChange =
  PresenceChange
    <$> ABE.key "user" asID
    <*> ABE.key "presence" asPresence

asPrefChange :: ABE.Parse Text PrefChange
asPrefChange =
  PrefChange
    <$> ABE.key "name" ABE.asText
    <*> ABE.key "value" (ABEI.withValue Right)

asStar :: ABE.Parse Text Star
asStar =
  Star
    <$> ABE.key "user" ABE.asText
    <*> ABE.key "item" asStarItem
    <*> ABE.key "event_ts" asTS

asStarItem :: ABE.Parse Text StarItem
asStarItem =
  ABE.key "type" ABE.asText >>= \ case
    "message"      -> StarItemMessage     <$> ABE.key "message" asMessage
    "file"         -> StarItemFile        <$> ABE.key "file" asFile
    "file_comment" -> StarItemFileComment <$> ABE.key "file" asFile <*> ABE.key "comment" asFileComment
    "channel"      -> StarItemChannel     <$> ABE.key "channel" asID
    "im"           -> StarItemIM          <$> ABE.key "im" asID
    "group"        -> StarItemGroup       <$> ABE.key "group" asID
    other          -> ABE.throwCustomError $ "unknown starrable item type " <> other

asTeamDomainChange :: ABE.Parse Text TeamDomainChange
asTeamDomainChange =
  TeamDomainChange
    <$> ABE.key "url" ABE.asText
    <*> ABE.key "domain" ABE.asText

asEmailDomainChanged :: ABE.Parse Text EmailDomainChanged
asEmailDomainChanged =
  EmailDomainChanged
    <$> ABE.key "email_domain" ABE.asText
    <*> ABE.key "event_ts" asTS

instance ToJSON RtmSendMessage where
  toJSON (RtmSendMessage seqnum chat message) = object
    [ "type"    .= ("message" :: Text)
    , "id"      .= seqnum
    , "channel" .= chat
    , "text"    .= message
    ]
