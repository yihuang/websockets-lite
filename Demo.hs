{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
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
import qualified Data.ByteString.Char8 as S
import Data.Enumerator (catchError, throwError)

import Network.Sockjs
import qualified Network.WebSockets as WS
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import Data.FileEmbed (embedDir)

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
            exists <- liftIO $ modifyMVar clients $ \cs -> do
                case M.lookup name cs of
                    Nothing -> return (M.insert name sink cs, False)
                    Just _  -> return (cs, True)
            when exists $ do
                send "User already exists."
                close

            flip catchSockjs (handleDisconnect name) $ do
                welcome name
                broadcast $ mconcat [name, " joined."]
                forever $ do
                    msg <- recv
                    broadcast $ mconcat [name, ": ", msg]
        _ -> send "invalid message."
  where
    broadcast msg = do
        sinks <- M.elems <$> liftIO (readMVar clients)
        forM_ sinks (flip sendSink msg)
    welcome name = do
        users <- filter (/=name) . M.keys <$> liftIO (readMVar clients)
        send $ "Welcome! Users: " `mappend` S.intercalate ", " users

    handleDisconnect name e = case fromException e of
        Just ConnectionClosed -> do
            liftIO $ modifyMVar_ clients $ return . M.delete name
            broadcast $ mconcat [name, " disconnected."]
        _ -> return ()

wsSockjs :: Sockjs () -> WS.Request -> WS.WebSockets WS.Hybi00 ()
wsSockjs sockjs req = do
    WS.acceptRequest req
    WS.sendTextData ("o"::ByteString)
    sink <- WS.getSink
    let sink' = WS.sendSink sink . WS.textData . (\s -> mconcat ["a[\"", s, "\"]"])
        iter = iterSockjs sockjs sink'
    flip WS.catchWsError (liftIO . print) $
        WS.runIteratee iter
    WS.sendTextData ("c[3000, \"Go away!\"]"::ByteString)

staticApp :: Application
staticApp = Static.staticApp Static.defaultFileServerSettings
  { Static.ssFolder = Static.embeddedLookup $ Static.toEmbedded $(embedDir "static")
  }

main :: IO ()
main = do
    putStrLn "http://localhost:9160/client.html"
    state <- newMVar M.empty
    Warp.runSettings Warp.defaultSettings
      { Warp.settingsPort = 9160
      , Warp.settingsIntercept = WaiWS.intercept (wsSockjs (chat state))
      } staticApp
