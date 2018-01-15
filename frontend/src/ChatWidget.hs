{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
module ChatWidget (
    chatWidget
  ) where

import Control.Monad (void)
import Data.Functor.Identity
import Data.Proxy (Proxy(..))

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL


import Data.GADT.Compare
import Data.GADT.Show
import Data.Dependent.Sum
import Data.Dependent.Map

import Data.Binary

import Servant.API
import Servant.Reflex

import Reflex.Dom.Core

import Common.Servant.API
import Common.WebSocket.Message

#ifndef __GHCJS__
import Util.Run

go :: IO ()
go = run chatWidget
#endif

data Notification a where
  NLoggedIn :: Notification String
  NLoggedOut :: Notification String
  NMessageSent :: Notification (String, String)
  NUserList :: Notification [String]

instance GEq Notification where
  geq NLoggedIn NLoggedIn =
    Just Refl
  geq NLoggedOut NLoggedOut =
    Just Refl
  geq NMessageSent NMessageSent =
    Just Refl
  geq NUserList NUserList =
    Just Refl
  geq _ _ =
    Nothing

instance GCompare Notification where
  gcompare NLoggedIn NLoggedIn = GEQ
  gcompare NLoggedIn _ = GLT
  gcompare _ NLoggedIn = GGT
  gcompare NLoggedOut NLoggedOut = GEQ
  gcompare NLoggedOut _ = GLT
  gcompare _ NLoggedOut = GGT
  gcompare NMessageSent NMessageSent = GEQ
  gcompare NMessageSent _ = GLT
  gcompare _ NMessageSent = GGT
  gcompare NUserList NUserList = GEQ

instance GShow Notification where
  gshowsPrec _ NLoggedIn =
    showString "LoggedIn"
  gshowsPrec _ NLoggedOut =
    showString "LoggedOut"
  gshowsPrec _ NMessageSent =
    showString "MessageSent"
  gshowsPrec _ NUserList =
    showString "UserList"

convertMessage :: Message -> DSum Notification Identity
convertMessage (LoggedIn u) =
  NLoggedIn :=> Identity u
convertMessage (LoggedOut u) =
  NLoggedOut :=> Identity u
convertMessage (MessageSent u m) =
  NMessageSent :=> Identity (u, m)
convertMessage (UserList us) =
  NUserList :=> Identity us

ip :: Text
ip = "127.0.0.1"

port :: Int
port = 9090

loginWidget ::
  forall t m.
  MonadWidget t m =>
  Workflow t m ()
loginWidget = Workflow $ el "div" $ mdo

  let
    baseUrl = BaseFullUrl Http ip port "/"
    (login :<|> _ :<|> _ :<|> _) =
      client (Proxy :: Proxy MyAPI2) (Proxy :: Proxy m) (Proxy :: Proxy ()) (pure baseUrl)

  ti <- textInput def
  let
    checkName t =
      if Text.null (Text.strip t)
      then Left "Name cannot be empty"
      else Right (UserName . Text.unpack $ t)
    dName =
      checkName <$> ti ^. textInput_value

  dError <- holdDyn "" . leftmost $ [eError, "" <$ eOut]
  dynText dError

  eLogin <- button "Login"

  eRes <- login dName eLogin

  let
    eError = fmapMaybe reqFailure eRes
    eOut = fmapMaybe reqSuccess eRes

  pure ((), loggedInWidget . getUserId <$> eOut)


displayNotification :: Message -> Text
displayNotification (LoggedIn u) =
  Text.pack . mconcat $ [u, " joined"]
displayNotification (LoggedOut u) =
  Text.pack . mconcat $ [u, " left"]
displayNotification (MessageSent u m) =
  Text.pack . mconcat $ [u, ": ", m]
displayNotification (UserList us) =
  Text.pack . mconcat $ us

-- TODO disable the message input unless the ws is connected
-- TODO have a scrollable message area to display notifications in arrival order

userWidget ::
  MonadWidget t m =>
  EventSelector t Notification ->
  m ()
userWidget sel = do
  let
    eJoin = select sel NLoggedIn
    ePart = select sel NLoggedOut
    eUserList = select sel NUserList

  dMap <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            (\k -> Map.insert k k) <$> eJoin
          , Map.delete <$> ePart
          , (\us -> const . Map.fromList . zip us $ us) <$> eUserList
          ]

  _ <- el "ul" . list dMap $
    el "li" . dynText . fmap Text.pack

  pure ()

notificationWidget ::
  MonadWidget t m =>
  EventSelector t Notification ->
  m ()
notificationWidget sel = do
  let
    displayJoin u =
      Text.pack . mconcat $ [u, " joined"]
    eJoin =
      select sel NLoggedIn
    eJoinOut =
      displayJoin <$> eJoin

    displayPart u =
      Text.pack . mconcat $ [u, " left"]
    ePart =
      select sel NLoggedOut
    ePartOut =
      displayPart <$> ePart

    displayMessage (u, m) =
      Text.pack . mconcat $ [u, ": ", m]
    eMessage =
      select sel NMessageSent
    eMessageOut =
      displayMessage <$> eMessage

  dList <- foldDyn (++) [] . mergeWith (++) . fmap (fmap pure) $ [
      eJoinOut
    , ePartOut
    , eMessageOut
    ]

  _ <- el "ul" . simpleList (fmap reverse dList) $
    el "li" . dynText

  pure ()

loggedInWidget ::
  forall t m.
  MonadWidget t m =>
  Int ->
  Workflow t m ()
loggedInWidget i = Workflow $ mdo
  let
    baseUrl = BaseFullUrl Http ip port "/"
    (_ :<|> message :<|> _ :<|> logout) =
      client (Proxy :: Proxy MyAPI2) (Proxy :: Proxy m) (Proxy :: Proxy ()) (pure baseUrl)

  let
    wsUrl = mconcat ["ws://", ip, ":", (Text.pack . show $ port), "/user/", (Text.pack . show $ i), "/notifications"]
    wsConfig = WebSocketConfig (never :: Event t [B.ByteString]) ((1000, "Logging out") <$ eLogout) False
  ws <- webSocket wsUrl wsConfig

  let
    eNotification = decode . BL.fromStrict <$> ws ^. webSocket_recv
    selNotification = fan $ fromList . pure . convertMessage <$> eNotification

  userWidget selNotification
  notificationWidget selNotification

  ti <- textInput def
  let
    checkMessage t =
      if Text.null (Text.strip t)
      then Left "Message cannot be empty"
      else Right (MessageBody . Text.unpack $ t)
    dMessage =
      checkMessage <$> ti ^. textInput_value

  eMessageClick <- button "Send"

  eLogoutClick <- button "Logout"

  let
    dId = pure . Right $ i

  eMessageRes <- message dId dMessage eMessageClick
  eLogoutRes <- logout dId eLogoutClick

  let
    eMessageError = fmapMaybe reqFailure eMessageRes
    eMessage = fmapMaybe reqSuccess eMessageRes

    eLogoutError = fmapMaybe reqFailure eLogoutRes
    eLogout = fmapMaybe reqSuccess eLogoutRes

  dError <- holdDyn "" . leftmost $ [eMessageError, eLogoutError, "" <$ eMessage, "" <$ eLogout]
  dynText dError

  pure ((), loginWidget <$ eLogout)

chatWidget ::
  forall t m.
  MonadWidget t m =>
  m ()
chatWidget = do

  _ <- workflow loginWidget

  pure ()
