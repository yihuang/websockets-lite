{-# LANGUAGE OverloadedStrings #-}
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Applicative
import Control.Concurrent.MVar

import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Attoparsec
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
import Data.Enumerator (catchError, throwError)

import Network.Sockjs
import qualified Network.WebSockets as WS

echo :: Sockjs ()
echo = (forever $ recv >>= send) `catchSockjs` (\e -> liftIO $ putStrLn "closed")

close' :: Sockjs ()
close' = close

data ChatMessage = ChatJoin ByteString
                 | ChatData ByteString

chatP :: Parser ChatMessage
chatP = ChatJoin <$> (string "join" *> skipSpace *> takeByteString)
    <|> ChatData <$> takeByteString

recvChat :: Sockjs ChatMessage
recvChat = recvWith chatP

chat :: MVar (Map ByteString Sink) -> Sockjs ()
chat clients = do
    msg <- recvChat
    case msg of
        ChatJoin name -> do
            sink <- getSink
            exists <- liftIO $ modifyMVar clients $ \clients' -> do
                case M.lookup name clients' of
                    Nothing -> return (M.insert name sink clients', False)
                    Just _  -> return (clients', True)
            when exists $ do
                send "User already exists."
                close

            flip catchSockjs (handleDisconnect name) $ do
                broadcast $ mconcat ["User ", name, " joined."]
                forever $ do
                    ChatData msg <- recvChat
                    broadcast msg
        _ -> send "invalid message."
  where
    broadcast msg = do
        sinks <- M.elems <$> liftIO (readMVar clients)
        forM_ sinks (flip sendSink msg)
    handleDisconnect name e = case fromException e of
        Just ConnectionClosed -> do
            liftIO $ modifyMVar_ clients $ return . M.delete name
            broadcast $ mconcat [name, " disconnected."]
        _ -> return ()

wsSockjs :: Sockjs () -> WS.Request -> WS.WebSockets WS.Hybi00 ()
wsSockjs sockjs req = do
    WS.acceptRequest req
    WS.sendTextData ("o\n"::ByteString)
    sink <- WS.getSink
    let sink' = WS.sendSink sink . WS.textData
        iter = iterSockjs sockjs sink'
    flip WS.catchWsError (liftIO . print) $
        WS.runIteratee iter
    WS.sendTextData ("c[3000, \"Go away!\"]"::ByteString)

main :: IO ()
main = do
    state <- newMVar M.empty
    WS.runServer "127.0.0.1" 3000 (wsSockjs (chat state))
    -- WS.runServer "127.0.0.1" 3000 (wsSockjs echo)
