{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module FilterWheel where

import Control.Monad
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.IORef
import Data.List
import Data.Serialize
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Hardware.Serialport

data FilterWheelDesc = ThorlabsFW103HDesc {
                           fw103DescName :: !Text
                         , fw103DescPortName :: !String
                         , fw103DescFilters :: [(Text, Int)]
                       }
                     deriving (Read, Show)

data FilterWheel = ThorlabsFW103H !Text ![(Text, Int)] !SerialPort !(IORef Bool) !(IORef Int)

data JogDirection = JogForward | JogBack
                  deriving (Eq)

openFilterWheels :: [FilterWheelDesc] -> IO [LightFilter]
openFilterWheels = mapM_ openFilterWheel
  where
    openFilterWheel (ThorlabsFW103HDesc name portName chs) =
        let port = openSerial portName (defaultSerialSettings {commSpeed = CS115200})
        in ThorlabsFW103H name (validateChannels chs) <$> port <*> newIORef False <*> newIORef 0
    validateChannels :: Text -> [(Int, Text)] -> [(Int, Text)]
    validateChannels name chs | haveDuplicates chs = error ("duplicate channels for " ++ T.unpack name)
                              | invalidFilterIndices chs = error ("invalid filter indices for " ++ T.unpack name)
                              | invalidFilterNames chs = error ("invalid filter names for " ++ T.unpack name)
                              | otherwise = chs
      where
        haveDuplicates chs = (nodups (map fst chs) && nodups (map snd chs))
        nodups xs = (nub xs) == xs
        invalidFilterIndices = any (\(i, _) -> not (within i 0 5))
        invalidFilterNames = any (\(_, n) -> T.null n)

closeFilterWheels :: [FilterWheel] -> IO ()
closeFilterWheels = mapM_ closeFilterWheels
  where
    closeFilterWheels (ThorlabsFW103H _ _ port _ _) = closeSerial port

filterWheelHasChannel :: FilterWheel -> Text -> Bool
filterWheelHasChannel (ThorlabsFW103H _ chs _ _ _) c = c `elem` (map snd chs)

switchToChannel :: FilterWheel -> Text -> IO (Either String ())
switchToChannel fn chName | not (filterWheelHasChannel fw chName) = error "no matching channel for filter wheel"
                          | otherwise = switchToChannel' fw chName
  where
    switchToChannel' fn (ThorlabsFW103H _ chs port haveInitRef currChannelRef) =
        runExceptT (
            initFW103HIfNeeded >>

        )
        where
          initFW103HIfNeeded =
              readIORef haveInitRef >>= \haveInit
              if (haveInit)
              then return (Right ())
              else
                  sendReceiveFW103HMessage port (fw103HMessageWithParam (0x43, 0x04) 0) (0x44, 0x04) >>= \result ->
                  case result of
                    Left e -> return e
                    Right _ -> writeIORef haveInitRef true >>
                               writeIORef currChannelRef 0 >>
                               return (Right ())
          moveToFilter fn =
              let filterIndex = fromJust (lookup fn chs)
              in

sendReceiveFW103HMessage :: SerialPort -> ByteString -> (Int, Int) -> IO (Either String ())
sendReceiveFW103HMessage port msg (resp1, resp2) =
    send port msg >>
    readAtLeastNBytesFromSerial port 6 >>= \response ->
    if (B.take 2 response /= B.pack [resp1, resp2])
    then return (Left ("unexpected response from FW103H: " ++ show response)))
    else return (Right ())

fw103HJogMessage :: JogDirection -> IO ByteString
fw103HJogMessage JogForward = B.pack [0x6A, 0x04, 0x01, 0x01, 0x50, 0x01]
fw103HJogMessage JogBack    = B.pack [0x6A, 0x04, 0x01, 0x02, 0x50, 0x01]

jogActions :: Int -> Int -> Int -> [IO (Either String ())]
jogActions nFilters current target = 
    where
        nJogStepsForward :: Int -> Int -> Int -> Int
        nJogStepsForward nFilters current target | target >= current = target - current
                                                 | otherwise = (nFilters - current) + target
        nJogStepsBackward :: Int -> Int -> Int -> Int
        nJogStepsBackward nFilters current target | target <= current = current - target
                                                  | otherwise = current + (nFilters - target)

fw103HMessage :: (Int, Int) -> ByteString
fw103HMessage mID = fw103HMessageWithParam mID 0

fw103HMessageWithParam :: (Int, Int) -> Int -> ByteString
fw103HMessage (mID1, mID2) mParam = runPut $
    mapM_ putWord8 [fromIntegral mID1, fromIntegral mID2, fromIntegral mParam, 0, 0x50, 0x01]
