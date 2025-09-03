module Utils.WaitableChannel where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Maybe

-- A channel to send data from a writer to a reader thread. The channel is unbounded, but the reader can
-- wait until the channel is empty. The channel has a distinct reader and writer end. If the reader thread
-- encounters an exception, it will be thrown in the writer thread when it accesses the channel.
data WaitableChannelWriter a = WaitableChannelWriter {
                                   wcwChan :: TChan a
                                 , wcwTVar :: TVar Int    -- incremented when a dataset is submitted to the channel, decremented only once the image has been sent
                                                                        -- (not when it is removed from the queue!)
                                 , wcwReaderAsync :: Async ()        -- the channel reader
                               }

data WaitableChannelReader a = WaitableChannelReader {
                                   wcrChan :: TChan a
                                 , wcrTVar :: TVar Int
                               }

newWaitableChannel :: (WaitableChannelReader a -> IO ()) -> IO (WaitableChannelWriter a)
newWaitableChannel readerAction =
    newTChanIO >>= \chan ->
    newTVarIO 0 >>= \tvar ->
    pure (WaitableChannelReader chan tvar) >>= \channelReadEnd ->
    async (readerAction channelReadEnd) >>= \as ->
    pure (WaitableChannelWriter chan tvar as)

writeWriteableChannel :: WaitableChannelWriter a -> a -> IO ()
writeWriteableChannel chan dat =
    -- check if the thread reading from the channel has encountered an exception
    poll (wcwReaderAsync chan) >>= \status ->
    when (isJust status) (
        case (fromJust status) of
            Left e  -> throwIO e
            Right _ -> throwIO (userError "the WaitableChannel reader thread is not running")) >>
    atomically (
        writeTChan (wcwChan chan) dat >>
        modifyTVar' (wcwTVar chan) (+1))

-- Allow the writer to wait until the channel is empty. If the reader encountered an exception then rethrow it.
waitUntilWaitableChannelIsEmpty :: WaitableChannelWriter a -> IO ()
waitUntilWaitableChannelIsEmpty channel =
    race_ (waitForException channel) (waitUntilChannelEmpty channel)
    where
        waitForException :: WaitableChannelWriter a -> IO ()
        waitForException = wait . wcwReaderAsync
        waitUntilChannelEmpty :: WaitableChannelWriter a -> IO ()
        waitUntilChannelEmpty (WaitableChannelWriter chan tvar _) =
            atomically $
                readTVar tvar >>= \numImageSetsInFlight ->
                if (numImageSetsInFlight == 0)
                    then pure ()
                    else retry  -- block until the TVar (or channel) is next modified, after which this function will be run again

-- Look at the next item in the channel. Blocks until it is available.
peekWaitableChannel :: WaitableChannelReader a -> IO a
peekWaitableChannel channel = atomically $ peekTChan (wcrChan channel)

-- Remove the next item in the channel and return it. Blocks until it is available.
readWaitableChannel :: WaitableChannelReader a ->  IO a
readWaitableChannel channel = 
    atomically $ readTChan (wcrChan channel) >>= \item ->
                 modifyTVar' (wcrTVar channel) (\n -> n - 1) >>
                 pure item
