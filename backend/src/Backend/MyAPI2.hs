{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
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

import Util.Ticket

import Common.Servant.API
import Common.WebSocket.Message (Message(..))

data EntryIn t tag =
  EntryIn {
    eiName :: UserName
  , eiNotifications :: Event t [Message]
  , eiNotification :: Event t (tag, Request)
  , eiLogout :: Event t tag
  }

data EntryOut t tag =
  EntryOut {
    eoNotification :: Event t (tag, Response)
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
  [String] ->
  EntryIn t tag ->
  m (EntryOut t tag)
convertEntry users (EntryIn name eNotifications eNotificationIn eLogout) = mdo
  wsm <- liftIO . atomically $ mkWsManager 100
  eWsd <- wsData wsm
  dCount <- count eWsd
  dMap <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            Map.insert <$> current dCount <@> eWsd
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dModel <- list dMap $ \dv -> mdo
    v <- sample . current $ dv
    let
      eTx = leftmost [eNotifications, [UserList users] <$ _wsOpen ws]
      eClose = (1000, "Bye") <$ eLogout
      wsc = WebSocketConfig eTx eClose
    ws :: WebSocket t () <- accept v wsc (void eLogout)
    pure . void $ _wsClosed ws

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dModel

    f (tag, req) =
      case websocketsApp defaultConnectionOptions (void . handleConnection wsm) req of
        Nothing -> (tag, responseServantErr err404)
        Just res -> (tag, res)

    eNotificationOut =
      f <$> eNotificationIn

  pure $ EntryOut eNotificationOut eLogout

mkEntryIn ::
  ( Eq k
  , Reflex t
  ) =>
  k ->
  UserName ->
  Event t [Message] ->
  Event t (tag, k, Request) ->
  Event t (tag, k) ->
  EntryIn t tag
mkEntryIn k name eNotifications eNotification eLogout =
  EntryIn
    name
    eNotifications
    (fmap (\(tag, _, req) -> (tag, req)) . ffilter (\(_, k', _) -> k == k') $ eNotification)
    (fmap fst . ffilter (\(_, k') -> k == k') $ eLogout)

myAPI2Network ::
  ( Ord tag
  , Reflex t
  ) =>
  EventsIn' t tag () MyAPI2 ->
  BasicGuest t m (EventsOut t tag MyAPI2)
myAPI2Network (eLoginReq :<|> eMessageReq :<|> eNotificationReq :<|> eLogoutReq) = mdo

  dCount <- count eLoginReq

  dmEntryIn :: Dynamic t (Map Int (EntryIn t tag)) <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            (\k (_, v) -> Map.insert k (mkEntryIn k v eNotifications eNotificationReq eLogoutReq)) <$> current dCount <@> eLoginReq
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dmEntryOut <- list dmEntryIn $ \dv -> do
    v <- sample . current $ dv
    m <- sample . current $ dmEntryIn
    convertEntry (Map.elems . fmap (getUserName . eiName) $ m) v

  let
    eRemoves = fmap Map.keys . switch . current . fmap (mergeMap . fmap eoLogout) $ dmEntryOut
    eNotificationResSuccess = fmap (Map.fromList . Map.elems) . switch . current . fmap (mergeMap . fmap eoNotification) $ dmEntryOut

  let
    -- TODO not for actual use (leaks info)
    alreadyLoggedIn =
      err404 { errBody = "Already logged in" }

    mkLoginNotification m (_, n) =
        if Map.null . Map.filter (== n) . fmap eiName $ m
        then Just $ LoggedIn . getUserName $ n
        else Nothing
    eLoginNotification =
      fmapMaybe id (mkLoginNotification <$> current dmEntryIn <@> eLoginReq)
    mkLogin m c (t, n) =
      Map.singleton t $
        if Map.null . Map.filter (== n) . fmap eiName $ m
        then Right (UserId c)
        else Left alreadyLoggedIn
    eLoginRes =
      mkLogin <$> current dmEntryIn <*> current dCount <@> eLoginReq

    -- TODO not for actual use (leaks info)
    notLoggedIn =
      err404 { errBody = "Not logged in" }

    mkMessageNotification m (_, (k, MessageBody msg)) =
      (\e -> MessageSent (getUserName . eiName $ e) msg) <$> Map.lookup k m
    eMessageNotification =
      fmapMaybe id (mkMessageNotification <$> current dmEntryIn <@> eMessageReq)
    mkMessageResponse m (t, (k, _)) =
      Map.singleton t $
        if Map.member k m
        then Right NoContent
        else Left notLoggedIn
    eMessageRes =
      mkMessageResponse <$> current dmEntryIn <@> eMessageReq

    mkNotificationFail m (t, k, _) =
      if Map.member k m then Nothing else Just (Map.singleton t $ responseServantErr notLoggedIn)
    eNotificationResFail =
      fmapMaybe id (mkNotificationFail <$> current dmEntryIn <@> eNotificationReq)
    eNotificationRes =
      leftmost [eNotificationResSuccess, eNotificationResFail]

    mkLogoutNotification m (_, k) =
      LoggedOut . getUserName . eiName <$> Map.lookup k m
    eLogoutNotification =
      fmapMaybe id (mkLogoutNotification <$> current dmEntryIn <@> eLogoutReq)
    mkLogoutResponse m (t, k) =
      Map.singleton t $
        if Map.member k m
        then Right NoContent
        else Left notLoggedIn
    eLogoutRes =
      mkLogoutResponse <$> current dmEntryIn <@> eLogoutReq

    eNotifications =
      mergeWith (++) . fmap (fmap pure) $ [
          eLoginNotification
        , eMessageNotification
        , eLogoutNotification
        ]

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

{-
data Input a where
  ILogin        :: Input UserName
  IMessage      :: Input (UserId, MessageBody)
  INotification :: Input (UserId, Request)
  ILogout       :: Input (UserId, ())

data ClientInput a where
  CIMessage      :: ClientInput MessageBody
  CINotification :: ClientInput Request
  CILogout       :: ClientInput ()

inputToClientInput ::
  DSum Input Identity ->
  Maybe (UserId, DSum ClientInput Identity)
inputToClientInput i =
  case i of
    ILogin :=> Identity _ -> Nothing
    IMessage :=> Identity (i, mb) -> Just (i, CIMessage :=> Identity mb)
    INotification :=> Identity (i, r) -> Just (i, CINotification :=> Identity r)
    ILogout :=> Identity (i, u) -> Just (i, CILogout :=> Identity u)

clientInputToInput ::
  UserId ->
  DSum ClientInput Identity ->
  DSum Input Identity
clientInputToInput i ci =
  case ci of
    CIMessage :=> Identity mb -> IMessage :=> Identity (i, mb)
    CINotification :=> Identity r -> INotification :=> Identity (i, r)
    CILogout :=> Identity u -> ILogout :=> Identity (i, u)

data Output a where
  OLogin        :: Output UserId
  OMessage      :: Output ()
  ONotification :: Output Response
  OLogout       :: Output ()

data ClientOutput a where
  COMessage      :: ClientOutput ()
  CONotification :: ClientOutput Response
  COLogout       :: ClientOutput ()

outputToClientOutput ::
  DSum Output Identity ->
  Maybe (DSum ClientOutput Identity)
outputToClientOutput o =
  case o of
    OLogin :=> _ -> Nothing
    OMesssage :=> Identity u -> Just (COMessage :=> Identity u)
    ONotification :=> Identity r -> Just (CONotifcation :=> Identity r)
    OLogout :=> Identity u -> Just (COLogout :=> Identity u)

clientOutputToOutput ::
  DSum ClientOutput Identity ->
  DSum Output Identity
clientOutputToOutput co =
  case co of
    COMessage :=> Identity u -> OMessage :=> Identity u
    CONotification :=> Identity r -> ONotification :=> Identity r
    COLogout :=> Identity u -> OLogout :=> Identity u

data State = State Int (Map Int ClientState)

data ClientState = ClientState

check ::
  Dynamic t State ->
  Event t (DMap Input Identity) ->
  m (Event t e, Event t (DMap Input Identity))
check =
  undefined

checkClient ::
  Dynamic t ClientState ->
  Event t (DMap ClientInput Identity) ->
  m (Event t e, Event t (DMap ClientInput Identity))
checkClient =
  undefined

accumulate ::
  Event t (DMap Input Identity) ->
  m (Dynamic t State)
accumulate =
  undefined

accumulateClient ::
  Event t (DMap ClientInput Identity) ->
  m (Dynamic t ClientState)
accumulateClient =
  undefined

process ::
  Dynamic t State ->
  Event t (DMap Input Identity) ->
  m (Event t e, Event t (DMap Output Identity))
process =
  undefined

processClient ::
  Dynamic t ClientState ->
  Event t (DMap ClientInput Identity) ->
  m (Event t e, Event t (DMap ClientOutput Identity))
processClient =
  undefined
-}

