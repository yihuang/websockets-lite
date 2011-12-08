{-# LANGUAGE DeriveDataTypeable #-}
module Network.Sockjs
  ( Sockjs
  , iterSockjs

  , Sink
  , getSink
  , sendSink

  , SockjsException (..)
  , catchSockjs
  , throwSockjs

  , recv
  , recvWith
  , send
  , close
  ) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (Exception, SomeException)
import Data.Typeable (Typeable)
import Data.ByteString (ByteString)
import Data.Attoparsec (parseOnly, Parser)
import Data.Enumerator (Iteratee, throwError, catchError)
import qualified Data.Enumerator.List as EL
import qualified Network.WebSockets as WS

type Sink = ByteString -> IO ()

data SockjsState = SockjsState
  { stateSink :: Sink
  }

type Sockjs = StateT SockjsState (Iteratee ByteString IO)

iterSockjs :: Sockjs a -> Sink -> Iteratee ByteString IO a
iterSockjs sockjs sink = evalStateT sockjs (SockjsState sink)

data SockjsException
  = ConnectionClosed
  | ParseError String
  deriving (Typeable, Show)

instance Exception SockjsException

catchSockjs :: Sockjs a -> (SomeException -> Sockjs a) -> Sockjs a
catchSockjs sockjs handle = StateT $ \st ->
    runStateT sockjs st `catchError` (\e -> runStateT (handle e) st)

throwSockjs :: Exception e => e -> Sockjs a
throwSockjs e = lift $ throwError e

getSink :: Sockjs Sink
getSink = gets stateSink

sendSink :: Sink -> ByteString -> Sockjs ()
sendSink sink s = liftIO (sink s)

recv :: Sockjs ByteString
recv = lift $ do
    m <- EL.head
    case m of
        Just msg -> return msg
        Nothing -> throwError ConnectionClosed

send :: ByteString -> Sockjs ()
send msg = getSink >>= flip sendSink msg

recvWith :: Parser a -> Sockjs a
recvWith p = do
    s <- recv
    case parseOnly p s of
        Left e  -> throwSockjs (ParseError e)
        Right a -> return a

close :: Sockjs ()
close = throwSockjs WS.ConnectionClosed
