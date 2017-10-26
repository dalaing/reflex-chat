{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module ChatWidget (
    chatWidget
  ) where

import Control.Monad (void)
import Data.Proxy (Proxy(..))

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

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

loginWidget ::
  forall t m.
  MonadWidget t m =>
  Workflow t m ()
loginWidget = Workflow $ el "div" $ mdo

  let
    baseUrl = BaseFullUrl Http "127.0.0.1" 9090 "/"
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

loggedInWidget ::
  forall t m.
  MonadWidget t m =>
  Int ->
  Workflow t m ()
loggedInWidget i = Workflow $ mdo
  let
    baseUrl = BaseFullUrl Http "127.0.0.1" 9090 "/"
    (_ :<|> message :<|> _ :<|> logout) =
      client (Proxy :: Proxy MyAPI2) (Proxy :: Proxy m) (Proxy :: Proxy ()) (pure baseUrl)

  let
    wsUrl = mconcat ["ws://127.0.0.1:9090/user/", (Text.pack . show $ i), "/notifications"]
    wsConfig = WebSocketConfig (never :: Event t [B.ByteString]) ((1000, "Logging out") <$ eLogout) False
  ws <- webSocket wsUrl wsConfig

  let
    wsRx = displayNotification . decode . BL.fromStrict <$> ws ^. webSocket_recv

  dRx <- holdDyn "" wsRx

  dynText $ dRx

  ti <- textInput def
  let
    checkMessage t =
      if Text.null (Text.strip t)
      then Left "Message cannot be empty"
      else Right (MessageBody . Text.unpack $ t)
    dMessage =
      checkMessage <$> ti ^. textInput_value

  eMessageClick <- button "Send"

  let
    dId = pure . Right $ i

  eMessageRes <- message dId dMessage eMessageClick

  let
    eMessageError = fmapMaybe reqFailure eMessageRes
    eMessage = fmapMaybe reqSuccess eMessageRes

  eLogoutClick <- button "Logout"

  eLogoutRes <- logout dId eLogoutClick

  let
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
