module Common.WebSocket.Message (
    Message(..)
  ) where

import Data.Word (Word8)
import Data.Binary

type User = String
type MessageContent = String

data Message =
    LoggedIn User
  | LoggedOut User
  | MessageSent User MessageContent

instance Binary Message where
  put (LoggedIn u) =
    put (0 :: Word8) >> put u
  put (LoggedOut u) =
    put (1 :: Word8) >> put u
  put (MessageSent u m) =
    put (2 :: Word8) >> put u >> put m

  get = do
    c <- get :: Get Word8
    case c of
      0 -> LoggedIn <$> get
      1 -> LoggedOut <$> get
      2 -> MessageSent <$> get <*> get
