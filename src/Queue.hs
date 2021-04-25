module Queue (
  Queue,
  new,
  enqueue,
  dequeue,
) where

import Control.Concurrent.STM.TChan (TChan)
import qualified Control.Concurrent.STM.TChan as TChan
import Control.Monad.STM as STM


newtype Queue a = Queue (TChan a)


new :: IO (Queue a)
new = Queue <$> TChan.newTChanIO


enqueue :: Queue a -> a -> IO ()
enqueue (Queue c) a = STM.atomically $ TChan.writeTChan c a


dequeue :: Queue a -> IO a
dequeue (Queue c) = STM.atomically $ do
  isEmpty <- TChan.isEmptyTChan c
  if isEmpty then STM.retry else TChan.readTChan c
