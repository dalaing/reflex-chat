{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backend.MyAPI1 (
    go
  ) where

import Data.Proxy (Proxy(..))

import Control.Monad.STM (atomically)

import Control.Monad.Trans (MonadIO, liftIO)

import System.FilePath

import Servant.API
import Servant.Server

import Servant.Server.Internal.ServantErr

import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex hiding (Request, Response)

import Reflex.Basic.Host
import Reflex.Server.Servant
import Reflex.Server.Wai

import Util.Ticket

import Common.Servant.API

data EntryIn t tag =
  EntryIn {
    eiName :: UserName
  , eiFile :: Event t (tag, Request)
  , eiLogout :: Event t tag
  }

data EntryOut t tag =
  EntryOut {
    eoFile :: Event t (tag, Response)
  , eoLogout :: Event t tag
  }

convertEntry ::
  ( Reflex t
  , Monad m
  , PerformEvent t m
  , MonadIO (Performable m)
  ) =>
  EntryIn t tag ->
  m (EntryOut t tag)
convertEntry (EntryIn (UserName name) eFileIn eLogout) = do
  let
    app = staticApp (defaultFileServerSettings $ "test-files" </> name)
    f (tag, req) = do
      res <- pumpWaiApplication app req
      pure (tag, res)
  eFileOut <- performEvent $ liftIO . f <$> eFileIn
  pure $ EntryOut eFileOut eLogout

mkEntryIn ::
  ( Eq k
  , Reflex t
  ) =>
  k ->
  UserName ->
  Event t (tag, k, Request) ->
  Event t (tag, k) ->
  EntryIn t tag
mkEntryIn k name eFile eLogout =
  EntryIn
    name
    (fmap (\(tag, _, req) -> (tag, req)) . ffilter (\(_, k', _) -> k == k') $ eFile)
    (fmap fst . ffilter (\(_, k') -> k == k') $ eLogout)

myAPI1Network ::
  ( Ord tag
  , Reflex t
  ) =>
  EventsIn' t tag () MyAPI1 ->
  BasicGuest t m (EventsOut t tag MyAPI1)
myAPI1Network (eLoginReq :<|> eFileReq :<|> eLogoutReq) = mdo

  dCount <- count eLoginReq

  dmEntryIn :: Dynamic t (Map Int (EntryIn t tag)) <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            (\k (_, v) -> Map.insert k (mkEntryIn k v eFileReq eLogoutReq)) <$> current dCount <@> eLoginReq
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dmEntryOut <- list dmEntryIn $ \dv -> do
    v <- sample . current $ dv
    convertEntry v

  let
    eRemoves = fmap Map.keys . switch . current . fmap (mergeMap . fmap eoLogout) $ dmEntryOut
    eFileResSuccess = fmap (Map.fromList . Map.elems) . switch . current . fmap (mergeMap . fmap eoFile) $ dmEntryOut
    eLogoutResSuccess = fmap (Map.fromList . fmap (\t -> (t, Right NoContent)) . Map.elems) . switch . current . fmap (mergeMap . fmap eoLogout) $ dmEntryOut

  let
    alreadyLoggedIn =
      err404
    mkLogin m c (t, n) =
      if Map.null . Map.filter (== n) . fmap eiName $ m
      then Map.singleton t (Right (UserId c))
      else Map.singleton t (Left alreadyLoggedIn)
    eLoginRes =
      mkLogin <$> current dmEntryIn <*> current dCount <@> eLoginReq

    notLoggedIn =
      err404

    mkFileFail m (t, k, _) =
      if Map.member k m then Nothing else Just (Map.singleton t $ responseServantErr notLoggedIn)
    eFileResFail =
      fmapMaybe id (mkFileFail <$> current dmEntryIn <@> eFileReq)
    eFileRes =
      leftmost [eFileResSuccess, eFileResFail]

    mkLogoutFail m (t, k) =
      if Map.member k m then Nothing else Just (Map.singleton t (Left notLoggedIn))
    eLogoutResFail =
      fmapMaybe id (mkLogoutFail <$> current dmEntryIn <@> eLogoutReq)
    eLogoutRes =
      leftmost [eLogoutResSuccess, eLogoutResFail]

  pure $ eLoginRes :<|> eFileRes :<|> eLogoutRes

go :: IO ()
go = do
  td <- newTicketDispenser
  let mkT = atomically $ getNextTicket td
  app <- basicHost $ serverGuest (Proxy :: Proxy MyAPI1) mkT myAPI1Network
  run 9090 app
