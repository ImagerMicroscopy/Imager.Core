{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module SimpleJSONServer (
    runServer,
    MessageHandler,
    ResponseType (..),
    ServerSettings (..),
    defaultSettings
)
where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import Data.Serialize hiding(encode, decode)
import qualified Data.Text as T
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
import qualified Network.Socket.ByteString.Lazy as SL
import System.IO
import System.Timeout

type MessageHandler a = Value -> a -> IO (ResponseType, a)

data ResponseType = ResponseJSON !Value
                  | ResponseLBS  !L.ByteString
                  | ResponseBSList ![ByteString]

instance NFData ResponseType where
    rnf (ResponseJSON v) = rnf v
    rnf (ResponseLBS v) = rnf v
    rnf (ResponseBSList v) = rnf v

data ServerSettings = ServerSettings {
                        ssBindToAllInterfaces :: !Bool
                      , ssReceiveTimeout :: !(Maybe Int)
                      , ssHandlerTimeout :: !(Maybe Int)
                      , ssMaxMessageSize :: !Int
                      , ssDebugMessages    :: !Bool
                      , ssPrependPacketSize :: !Bool
}

defaultSettings :: ServerSettings
defaultSettings = ServerSettings True (Just 250000) (Just 2000000) 20000 False True
runServer :: PortNumber -> MessageHandler a -> a -> ServerSettings -> IO ()
runServer portNumber handler env settings =
    withSocketsDo $
    head <$> getAddrInfo hints interface (Just (show portNumber)) >>= \serveraddr ->
    socket (addrFamily serveraddr) Stream defaultProtocol >>= \listenSock ->
    bind listenSock (addrAddress serveraddr) >>
    (listen listenSock 16 >> messageLoop listenSock handler env settings) `finally` (close listenSock)
    where
        hints = Just (defaultHints {addrFlags = [AI_PASSIVE]})
        interface = if (ssBindToAllInterfaces settings)
                    then Nothing
                    else Just "localhost"

messageLoop :: Socket -> MessageHandler a -> a -> ServerSettings -> IO ()
messageLoop listenSock  handler env settings =
    try (
        bracket (accept listenSock) (close . fst) (\(sock, _) ->
            handleMessage sock handler env settings)) >>= \result ->
    case result of
        Left (_ :: IOException) -> messageLoop listenSock handler env settings
        Right newEnv              -> messageLoop listenSock handler newEnv settings


handleMessage :: Socket -> MessageHandler a -> a -> ServerSettings -> IO a
handleMessage sock handler env settings =
    receiveMessage sock settings >>= \msg ->
    case msg of
        Left e  -> let encoded = encode $ object ["error" .= (T.pack e)]
                   in  when (prependPacketSize) (sendAll sock (encodeInt32 (L.length encoded + 4))) >>
                       SL.sendAll sock encoded >> return env
        Right m -> when (debug) (putStrLn ("Received:\n" ++ show msg)) >>
                   timeout timeoutDuration (try (handler m env)) >>= \result ->
                   case result of
                       Nothing ->
                           when (debug) (putStrLn "handler timeout") >>
                           let encoded = (encode (object [("error", "handlertimeout")]))
                           in  when (prependPacketSize) (sendAll sock (encodeInt32 (L.length encoded + 4))) >>
                               SL.sendAll sock encoded >> return env
                       Just v -> case v of
                           Right (ResponseJSON response, newEnv) ->
                                when (debug) (putStrLn ("Sending: " ++ show response)) >>
                                let encoded = encode response
                                in  when (prependPacketSize) (sendAll sock (encodeInt32 (L.length encoded + 4))) >>
                                    SL.sendAll sock (encode response) >> return newEnv
                           Right (ResponseLBS response, newEnv) ->
                                when (debug) (putStrLn ("Sending: " ++ show response)) >>
                                when (prependPacketSize) (sendAll sock (encodeInt32 (L.length response + 4))) >>
                                SL.sendAll sock response >> return newEnv
                           Right (ResponseBSList response, newEnv) ->
                                when (debug) (putStrLn ("Sending: " ++ show response)) >>
                                --writeFile "debugout.txt" (show (concat (map B.unpack response))) >>   -- provides debugging of binary data if needed
                                --B.writeFile "debugbinary.bin" (mconcat response) >>
                                when (prependPacketSize) (sendAll sock (encodeInt32 ((sum (map B.length response)) + 4))) >>
                                sendMany sock response >> return newEnv
                           Left (e :: SomeException) ->
                                when (debug) (putStrLn "Invalid request") >>
                                let encoded = encode (object ["error" .= displayException e])
                                in  when (prependPacketSize) (sendAll sock (encodeInt32 (L.length encoded + 4))) >>
                                    SL.sendAll sock encoded >> return env
    where
        handlerTimeout = ssHandlerTimeout settings
        timeoutDuration | isJust handlerTimeout = fromJust handlerTimeout
                        | otherwise             = maxBound
        debug = ssDebugMessages settings
        prependPacketSize = ssPrependPacketSize settings
        encodeInt32 :: (Integral a) => a -> ByteString
        encodeInt32 n = runPut $ putWord32host (fromIntegral n)

receiveMessage :: Socket -> ServerSettings -> IO (Either String Value)
receiveMessage sock settings = timeout receiveTimeout (readMessage L.empty) >>= \result ->
                               case result of
                                 Nothing        -> return (Left "receive timeout")
                                 Just (Nothing) -> return (Left "invalid or incomplete JSON")
                                 Just (Just v)  -> return (Right v)
    where
        receiveTimeout = if (isJust $ ssReceiveTimeout settings) then fromJust (ssReceiveTimeout settings) else (maxBound :: Int)
        maxMessageSize = ssMaxMessageSize settings
        readMessage :: L.ByteString -> IO (Maybe Value)
        readMessage accum =
            SL.recv sock 4096 >>= \message ->
            if ((L.length message == 0) || (L.length accum + L.length message > (fromIntegral maxMessageSize)))
              then return Nothing   -- remote side closed connection or message too long
              else
                let newAccum = L.append accum message
                in case (decode newAccum) of
                    Nothing         -> readMessage newAccum
                    Just (Object v) -> return (Just (Object v))
                    _               -> return Nothing
