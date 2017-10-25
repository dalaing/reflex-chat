{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module ChatWidget (
    chatWidget
  ) where

import Data.Proxy (Proxy(..))

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Servant.API
import Servant.Reflex

import Reflex.Dom.Core

import Common.Servant.API

#ifndef __GHCJS__
import Util.Run

go :: IO ()
go = run chatWidget
#endif

loginWidget ::
  MonadWidget t m =>
  (Dynamic t (Either Text UserName) -> Event t () -> m (Event t (ReqResult () UserId))) ->
  m (Event t UserId)
loginWidget login = el "div" $ mdo
  ti <- textInput def
  let
    checkName t =
      if Text.null (Text.strip t)
      then Left "Name cannot be empty"
      else Right (UserName . Text.unpack $ t)
    dName =
      checkName <$> ti ^. textInput_value

  dError <- holdDyn "" . leftmost $ ["" <$ eOut, eError]
  dynText dError

  eLogin <- button "Login"

  eRes <- login dName eLogin

  let
    eError = fmapMaybe reqFailure eRes
    eOut = fmapMaybe reqSuccess eRes

  pure eOut

logoutWidget ::
  MonadWidget t m =>
  m (Event t ())
logoutWidget = do
  eLogout <- button "Logout"
  pure eLogout

chatWidget ::
  forall t m.
  MonadWidget t m =>
  m ()
chatWidget = do

  let
    baseUrl = BaseFullUrl Http "127.0.0.1" 9090 "/"
    (login :<|> message :<|> _ :<|> logout) =
      client (Proxy :: Proxy MyAPI2) (Proxy :: Proxy m) (Proxy :: Proxy ()) (pure baseUrl)

  _ <- loginWidget login

  pure ()
