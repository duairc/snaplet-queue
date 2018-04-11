{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Queue
    ( Queue, initQueue, asynchronously, asynchronouslyTop, synchronously
    )
where

-- base ----------------------------------------------------------------------
import           Control.Concurrent (forkIO, getNumCapabilities)
import           Control.Monad ((>=>), forever, replicateM_)
import           Control.Monad.IO.Class (liftIO)


-- mtl -----------------------------------------------------------------------
import           Control.Monad.Reader.Class (ask)


-- snap ----------------------------------------------------------------------
import           Snap.Snaplet
                     ( SnapletInit, makeSnaplet
                     , SnapletLens, with, withTop
                     , Handler
                     )


-- snaplet-queue -------------------------------------------------------------
import           Control.Monad.Task (Task, task, logErrors)


-- stm -----------------------------------------------------------------------
import           Control.Concurrent.STM.TChan
                     ( TChan, newTChan, readTChan, writeTChan
                     )
import           Control.Monad.STM (atomically)


------------------------------------------------------------------------------
newtype Queue = Queue (TChan (IO ()))


-------------------------------------------------------------------------------
initQueue :: SnapletInit b Queue
initQueue = makeSnaplet "Queue" description Nothing $ liftIO $ do
    queue <- atomically newTChan
    n <- getNumCapabilities
    replicateM_ n $ forkIO (go queue)
    pure $ Queue queue
  where
    description = "Asynchronously queue tasks in web handlers to be executed\
        \ later."
    go queue = forever $ do
        io <- atomically $ readTChan queue
        io


------------------------------------------------------------------------------
asynchronously :: SnapletLens v Queue -> Task b v () -> Handler b v ()
asynchronously lens action = do
    Queue queue <- with lens ask
    io <- task $ logErrors action
    liftIO $ atomically $ writeTChan queue io


------------------------------------------------------------------------------
asynchronouslyTop :: SnapletLens b Queue -> Task b v () -> Handler b v ()
asynchronouslyTop lens action = do
    Queue queue <- withTop lens ask
    io <- task $ logErrors action
    liftIO $ atomically $ writeTChan queue io


------------------------------------------------------------------------------
synchronously :: Task b v a -> Handler b v a
synchronously = task >=> liftIO
