{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Backend.MyAPI2 (
    go
  ) where

import Control.Monad (void)
import Control.Concurrent (forkIO)

import Data.Proxy (Proxy(..))

import Control.Monad.STM (atomically)

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)

import System.FilePath

import Servant.API
import Servant.Server hiding (route)

import Servant.Server.Internal.ServantErr

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsApp)
import Network.Wai.Application.Static
import Network.WebSockets (defaultConnectionOptions)

import qualified Data.ByteString as B

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex hiding (Request, Response)

import Reflex.Basic.Host
import Reflex.Server.Servant
import Reflex.Server.Wai
import Reflex.Server.WebSocket
import Reflex.Server.WebSocket.Binary

import Util.List
import Util.Ticket

import Common.Servant.API
import Common.WebSocket.Message (Message(..))

data EntryIn t tag =
  EntryIn {
    eiName :: UserName
  , eiMessage :: Event t (tag, MessageBody)
  , eiNotification :: Event t (tag, Request)
  , eiLogout :: Event t tag
  }

data EntryOut t tag =
  EntryOut {
    eoMessage :: Event t tag
  , eoNotification :: Event t (tag, Response)
  , eoLogout :: Event t tag
  }

convertEntry ::
  forall t tag m.
  ( Reflex t
  , Monad m
  , PerformEvent t m
  , TriggerEvent t m
  , MonadHold t m
  , MonadAdjust t m
  , PostBuild t m
  , MonadFix m
  , MonadIO (Performable m)
  , MonadIO m
  ) =>
  EntryIn t tag ->
  m (EntryOut t tag)
convertEntry (EntryIn name eMessageIn eNotificationIn eLogout) = mdo
  wsm <- liftIO . atomically $ mkWsManager 100
  eWsd <- wsData wsm
  dCount <- count eWsd
  dMap <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            Map.insert <$> current dCount <@> eWsd
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dModel <- list dMap $ \dv -> do
    v <- sample . current $ dv
    let
      eTx = (\(_, MessageBody m) -> [MessageSent "" m]) <$> eMessageIn
      eClose = (1000, "Bye") <$ eLogout
      wsc = WebSocketConfig eTx eClose
    ws :: WebSocket t () <- accept v wsc (void eLogout)
    pure . void $ _wsClosed ws

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dModel

    eMessageOut = fst <$> eMessageIn

    f (tag, req) =
      case websocketsApp defaultConnectionOptions (void . handleConnection wsm) req of
        Nothing -> (tag, responseServantErr err404)
        Just res -> (tag, res)

    eNotificationOut =
      f <$> eNotificationIn

  pure $ EntryOut eMessageOut eNotificationOut eLogout

mkEntryIn ::
  ( Eq k
  , Reflex t
  ) =>
  k ->
  UserName ->
  Event t (tag, (k, MessageBody)) ->
  Event t (tag, k, Request) ->
  Event t (tag, k) ->
  EntryIn t tag
mkEntryIn k name eMessage eNotification eLogout =
  EntryIn
    name
    (fmap (\(tag, (_, msg)) -> (tag, msg)) $ eMessage)
    (fmap (\(tag, _, req) -> (tag, req)) . ffilter (\(_, k', _) -> k == k') $ eNotification)
    (fmap fst . ffilter (\(_, k') -> k == k') $ eLogout)

myAPI2Network ::
  ( Ord tag
  , Show tag
  , Reflex t
  ) =>
  EventsIn' t tag () MyAPI2 ->
  BasicGuest t m (EventsOut t tag MyAPI2)
myAPI2Network (eLoginReq :<|> eMessageReq :<|> eNotificationReq :<|> eLogoutReq) = mdo

  dCount <- count eLoginReq

  dmEntryIn :: Dynamic t (Map Int (EntryIn t tag)) <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            (\k (_, v) -> Map.insert k (mkEntryIn k v eMessageReq eNotificationReq eLogoutReq)) <$> current dCount <@> eLoginReq
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dmEntryOut <- list dmEntryIn $ \dv -> do
    v <- sample . current $ dv
    convertEntry v

  let
    eRemoves = fmap Map.keys . switch . current . fmap (mergeMap . fmap eoLogout) $ dmEntryOut
    eMessageResSuccess = fmap (Map.fromList . fmap (\t -> (t, Right NoContent)) . Map.elems) . switch . current . fmap (mergeMap . fmap eoMessage) $ dmEntryOut
    eNotificationResSuccess = fmap (Map.fromList . Map.elems) . switch . current . fmap (mergeMap . fmap eoNotification) $ dmEntryOut
    eLogoutResSuccess = fmap (Map.fromList . fmap (\t -> (t, Right NoContent)) . Map.elems) . switch . current . fmap (mergeMap . fmap eoLogout) $ dmEntryOut

  let
    -- TODO not for actual use (leaks info)
    alreadyLoggedIn =
      err404 { errBody = "Already logged in" }

    mkLogin m c (t, n) =
      if Map.null . Map.filter (== n) . fmap eiName $ m
      then Map.singleton t (Right (UserId c))
      else Map.singleton t (Left alreadyLoggedIn)
    eLoginRes =
      mkLogin <$> current dmEntryIn <*> current dCount <@> eLoginReq

    -- TODO not for actual use (leaks info)
    notLoggedIn =
      err404 { errBody = "Not logged in" }

    mkMessageFail m (t, (k, _)) =
      if Map.member k m then Nothing else Just (Map.singleton t (Left notLoggedIn))
    eMessageResFail =
      fmapMaybe id (mkMessageFail <$> current dmEntryIn <@> eMessageReq)
    eMessageRes =
      leftmost [eMessageResSuccess, eMessageResFail]

    mkNotificationFail m (t, k, _) =
      if Map.member k m then Nothing else Just (Map.singleton t $ responseServantErr notLoggedIn)
    eNotificationResFail =
      fmapMaybe id (mkNotificationFail <$> current dmEntryIn <@> eNotificationReq)
    eNotificationRes =
      leftmost [eNotificationResSuccess, eNotificationResFail]

    mkLogoutFail m (t, k) =
      if Map.member k m then Nothing else Just (Map.singleton t (Left notLoggedIn))
    eLogoutResFail =
      fmapMaybe id (mkLogoutFail <$> current dmEntryIn <@> eLogoutReq)
    eLogoutRes =
      leftmost [eLogoutResSuccess, eLogoutResFail]

  performEvent_ $ liftIO . print <$> eLoginReq
  performEvent_ $ liftIO . print <$> eLoginRes

  pure $ eLoginRes :<|> eMessageRes :<|> eNotificationRes :<|> eLogoutRes

go :: IO ()
go = do
  td <- newTicketDispenser
  let mkT = atomically $ getNextTicket td
  app <- basicHost $ serverGuest (Proxy :: Proxy MyAPI2) mkT myAPI2Network
  run 9090 $ \req respond -> do
    if B.isPrefixOf "/files/" (rawPathInfo req)
    then staticApp (defaultFileServerSettings "./") req respond
    else app req respond
