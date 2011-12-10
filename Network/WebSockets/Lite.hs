{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Network.WebSockets.Lite
  ( WSLite
  , iterWSLite
  , toWebSockets

  , Sink
  , getSink
  , sendSink

  , WSLiteException (..)
  , catchError
  , throwError

  , UpProtocol (..)
  , DownProtocol (..)
  , close
  , recvBS
  , tryRecv
  , recv
  , send
  ) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (Exception, SomeException)

import Data.Maybe
import Data.Monoid
import Data.Typeable (Typeable)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Enumerator (Iteratee)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import qualified Network.WebSockets as WS

type Sink = ByteString -> IO ()

type WSLite = ReaderT Sink (Iteratee ByteString IO)

iterWSLite :: WSLite a -> Sink -> Iteratee ByteString IO a
iterWSLite lite sink = runReaderT lite sink

toWebSockets :: WSLite () -> WS.Request -> WS.WebSockets WS.Hybi00 ()
toWebSockets lite req = do
    WS.acceptRequest req
    sink <- WS.getSink
    let sink' = WS.sendSink sink . WS.textData
        iter = iterWSLite lite sink'
    WS.runIteratee iter `WS.catchWsError`
        (liftIO . putStrLn . ("uncaught exception: "++) . show)

data WSLiteException = ConnectionClosed
  deriving (Typeable, Show)

instance Exception WSLiteException

catchError :: WSLite a -> (SomeException -> WSLite a) -> WSLite a
catchError sockjs handle = ReaderT $ \env ->
    runReaderT sockjs env `E.catchError` (\e -> runReaderT (handle e) env)

throwError :: Exception e => e -> WSLite a
throwError e = lift $ E.throwError e

-- | protocol for request.
class UpProtocol a where
    decode :: ByteString -> Either String a

-- | protocol for reply.
class DownProtocol a where
    encode :: a -> ByteString

instance DownProtocol ByteString where
    encode = id

instance UpProtocol ByteString where
    decode = Right

send :: DownProtocol a => a -> WSLite ()
send a = getSink >>= flip sendSink a

recvBS :: WSLite ByteString
recvBS = lift $ do
    ms <- EL.head
    case ms of
        Just s -> return s
        Nothing -> E.throwError ConnectionClosed

tryRecv :: UpProtocol a => WSLite (Either String a)
tryRecv = do
    s <- recvBS
    return (decode s)

recv :: UpProtocol a => WSLite a
recv = do
    r <- tryRecv
    case r of
        Right a -> return a
        Left e  -> do send ("parse fail:" `mappend` S.pack e)
                      close

getSink :: WSLite Sink
getSink = ask

sendSink :: DownProtocol a => Sink -> a -> WSLite ()
sendSink sink a = liftIO (sink (encode a))

close :: WSLite a
close = throwError ConnectionClosed
