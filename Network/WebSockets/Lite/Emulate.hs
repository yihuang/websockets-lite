module Network.WebSockets.Lite.Emulate
  ( runWSLite
  ) where

import Data.ByteString (ByteString)
import Prelude hiding (catch)
import Control.Exception
import Control.Monad.IO.Class
import Data.Enumerator
import Network.WebSockets.Lite

runWSLite :: Enumerator ByteString IO () -> (ByteString -> IO ()) -> WSLite () -> IO ()
runWSLite enum sink lite = do
    let iter = iterWSLite lite sink
    catch (run_ $ enum $$ iter)
        (\e -> liftIO . putStrLn . ("uncaught exception: "++) . show $ (e::SomeException))
