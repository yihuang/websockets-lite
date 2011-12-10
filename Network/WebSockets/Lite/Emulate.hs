module Network.WebSockets.Lite.Emulate
  ( StreamChan
  , iterStreamChan
  , enumStreamChan
  , runWSLite
  ) where

import Data.ByteString (ByteString)
import Prelude hiding (catch)
import Control.Exception
import Control.Monad.IO.Class
import Data.Enumerator
import Control.Concurrent.STM
import Network.WebSockets.Lite

type StreamChan a = TChan (Stream a)

iterStreamChan :: StreamChan a -> Iteratee a IO ()
iterStreamChan ch = continue go
  where
    go EOF = liftIO $ atomically $ writeTChan ch EOF
    go stream = liftIO (atomically $ writeTChan ch stream) >> continue go

enumStreamChan :: StreamChan a -> Enumerator a IO b
enumStreamChan ch = checkContinue0 $ \loop f -> do
    stream <- liftIO $ atomically $ readTChan ch
    f stream >>== loop

runWSLite :: StreamChan ByteString -> StreamChan ByteString -> WSLite () -> IO ()
runWSLite inchan outchan lite = do
    let sink s = atomically $ writeTChan outchan (Chunks [s])
        iter = iterWSLite lite sink
    catch (run_ $ enumStreamChan inchan $$ iter)
        (\e -> liftIO . putStrLn . ("uncaught exception: "++) . show $ (e::SomeException))
