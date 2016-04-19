{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Control.Lens (_Just, at, preview, view, toListOf, each, to)
import Database.Persist.Quasi
import ModelOrphans
import qualified Slack

type FieldPair = (Bool, Text)

share [mkPersist sqlSettings { mpsGenerateLenses = True }, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")

fromSlackUser :: Slack.User -> User
fromSlackUser = Entity
  <$> view Slack.userID
  <*> ( User
    <$> view Slack.userName
    <*> view Slack.userRealName
    <*> view Slack.userDeleted
    <*> view Slack.userColor
    <*> preview (Slack.userTz . Slack.tz)
    <*> preview (Slack.userTz . Slack.tzLabel)
    <*> preview (Slack.userTz . Slack.tzOffset)
    <*> preview (Slack.userProfile . Slack.profileImages . at 72 . _Just)
    <*> preview (Slack.userProfile . Slack.profileImages . at 512 . _Just) )

fromSlackChannel :: Slack.Channel -> Entity Channel
fromSlackChannel = Entity
  <$> view Slack.channelID
  <*> ( Channel
    <$> view Slack.channelName
    <*> view Slack.channelIsArchived
    <*> view Slack.channelIsGeneral
    <*> map (fromMaybe "") . view (Slack.channelTopic . Slack.trackedValue) )

fromSlackMessage :: Slack.Message -> Maybe Message
fromSlackMessage message =
  flip fromMessageWithChannel message <$> preview (Slack.messageChat . _Just . Slack.typedID)
  where
    fromMessageWithChannel channelID = Message
      <$> pure (ChannelKey channelID)
      <*> view Slack.messageTS
      <*> view Slack.messageUser
      <*> view Slack.messageText
      <*> preview (Slack.messageEdited . _Just . Slack.messageEditedUser)
      <*> preview (Slack.messageEdited . _Just . Slack.messageEditedTS)

fromSlackAttachment :: MessageId -> Slack.Attachment -> MessageAttachment
fromSlackAttachment messageId = MessageAttachment
  <$> pure messageId
  <*> view Slack.attachmentFallback
  <*> view Slack.attachmentColor
  <*> view Slack.attachmentPretext
  <*> view Slack.attachmentAuthorName
  <*> view Slack.attachmentAuthorLink
  <*> view Slack.attachmentAuthorIcon
  <*> view Slack.attachmentTitle
  <*> view Slack.attachmentTitleLink
  <*> view Slack.attachmentFields
      . to ( mapFromList
           . map ( view Slack.fieldTitle
               &&& ((,) <$> view Slack.fieldShort <*> view Slack.fieldValue) ) )

fromSlackMessageReaction :: MessageId -> Slack.MessageReaction -> MessageReaction
fromSlackMessageReaction messageId = MessageReaction
  <$> pure messageId
  <*> view Slack.messageReactionName
  <*> view Slack.messageReactionCount
  <*> toListOf (Slack.messageReactionUsers . each . to UserKey)
