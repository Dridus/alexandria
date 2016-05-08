{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Control.Lens (_Just, at, preview, view, to)
import Database.Persist.Quasi
import ModelOrphans ()
import qualified Slack

type FieldPair = (Bool, Text)

type SlackChannelId = Slack.ID Slack.Channel
type SlackUserId    = Slack.ID Slack.User

share [mkPersist sqlSettings { mpsGenerateLenses = True }, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")

fromSlackUser :: Slack.User -> Entity User
fromSlackUser = Entity
  <$> view (Slack.userID . to UserKey)
  <*> ( User
    <$> view Slack.userName
    <*> view Slack.userRealName
    <*> view Slack.userDeleted
    <*> view Slack.userColor
    <*> preview (Slack.userTz . _Just . Slack.tz)
    <*> preview (Slack.userTz . _Just . Slack.tzLabel)
    <*> preview (Slack.userTz . _Just . Slack.tzOffset)
    <*> preview (Slack.userProfile . Slack.profileImages . at 72 . _Just)
    <*> preview (Slack.userProfile . Slack.profileImages . at 512 . _Just) )

fromSlackChannel :: Slack.Channel -> Entity Channel
fromSlackChannel = Entity
  <$> view (Slack.channelID . to ChannelKey)
  <*> ( Channel
    <$> view Slack.channelName
    <*> view Slack.channelIsArchived
    <*> view Slack.channelIsGeneral
    <*> fromMaybe "" . preview (Slack.channelTopic . _Just . Slack.trackedValue) )

fromSlackMessage :: SlackChannelId -> Slack.Message -> Message
fromSlackMessage channelKey = Message
  <$> pure channelKey
  <*> view Slack.messageTS
  <*> view (Slack.messageTS . to Slack.tsToUTCTime)
  <*> view Slack.messageUser
  <*> view Slack.messageSubtype
  <*> view Slack.messageText
  <*> preview (Slack.messageEdited . _Just . Slack.messageEditedUser)
  <*> preview (Slack.messageEdited . _Just . Slack.messageEditedTS)
  <*> preview (Slack.messageEdited . _Just . Slack.messageEditedTS . to Slack.tsToUTCTime)

fromSlackAttachment :: MessageId -> Slack.Attachment -> MessageAttachment
fromSlackAttachment messageId = MessageAttachment
  <$> pure messageId
  <*> view Slack.attachmentId
  <*> view Slack.attachmentFallback
  <*> view Slack.attachmentColor
  <*> view Slack.attachmentPretext
  <*> view Slack.attachmentAuthorName
  <*> view Slack.attachmentAuthorLink
  <*> view Slack.attachmentAuthorIcon
  <*> view Slack.attachmentTitle
  <*> view Slack.attachmentTitleLink
  <*> view ( Slack.attachmentFields
           . to ( mapFromList
                . map ( view Slack.fieldTitle
                    &&& ((,) <$> view Slack.fieldShort <*> view Slack.fieldValue) ) ) )

fromSlackMessageReaction :: MessageId -> Slack.MessageReaction -> MessageReaction
fromSlackMessageReaction messageId = MessageReaction
  <$> pure messageId
  <*> view Slack.messageReactionName
  <*> view Slack.messageReactionCount
  <*> view Slack.messageReactionUsers
