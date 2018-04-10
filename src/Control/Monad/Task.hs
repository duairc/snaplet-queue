{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Task
    ( Task
    , task, logErrors, log
    , getSnapletState, getsSnapletState, putSnapletState, modifySnapletState
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (Alternative)
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative)
#endif
import           Control.Exception (SomeException)
import           Control.Monad (MonadPlus)
#if MIN_VERSION_base(4, 9, 0)
import           Control.Monad.Fail (MonadFail)
#endif
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO)
import           Data.IORef (IORef)
import           Prelude hiding (log)


-- bytestring ----------------------------------------------------------------
import           Data.ByteString (ByteString)


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
import           Monad.Abort (MonadAbort)
import           Monad.Fork (MonadFork)
import           Monad.Mask (MonadMask)
import           Monad.Recover (MonadRecover)
import           Monad.Reader (MonadReader)
import           Monad.ST (MonadST)
import           Monad.State (MonadState)
import           Monad.Try (MonadTry, mtry)


-- mtl -----------------------------------------------------------------------
import qualified Control.Monad.Reader.Class as MTL
import qualified Control.Monad.State.Class as MTL


-- snap ----------------------------------------------------------------------
import           Snap.Snaplet
                    ( MonadSnaplet, Handler, Snaplet
                    , with, withTop, with', withTop', getLens, getOpaqueConfig
                    )


-- snaplet-queue -------------------------------------------------------------
import           Control.Monad.Trans.Task (TaskT)
import qualified Control.Monad.Trans.Task as T
import           Monad.Log (MonadLog)
import qualified Monad.Log as G
import           Monad.Lens (MonadLens)
import qualified Monad.Lens as L


------------------------------------------------------------------------------
newtype Task b v a = Task (TaskT b v IO a)
  deriving
    ( Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix, MonadIO
    , MonadLog, MonadLens (Snaplet b) (Snaplet v), MonadReader v, MonadState v
    , MTL.MonadReader v, MTL.MonadState v, MonadFork, MonadMask, MonadST IORef
    , MonadAbort SomeException, MonadRecover SomeException, MonadInner IO
#if MIN_VERSION_base(4, 9, 0)
    , MonadFail
#endif
    )


------------------------------------------------------------------------------
instance Iso1 (Task b v) where
    type Codomain1 (Task b v) = TaskT b v IO
    to1 (Task m) = m
    from1 = Task


------------------------------------------------------------------------------
instance MonadTry (Task b v) where
    mtry = from1 . fmap (either (Left . from1) Right) . mtry . to1


------------------------------------------------------------------------------
instance MonadInnerControl IO (Task b v) where
    suspendI = defaultSuspendI
    resumeI = defaultResumeI
    captureI = defaultCaptureI
    extractI = defaultExtractI
    mapI = defaultMapI


------------------------------------------------------------------------------
instance MonadInnerInvariant IO (Task b v) IO (Task b v) where
    hoistisoI = defaultHoistisoI


------------------------------------------------------------------------------
instance MonadInnerFunctor IO (Task b v) IO (Task b v) where
    hoistI = defaultHoistI


------------------------------------------------------------------------------
instance MonadSnaplet Task where
    with l = from1 . T.with l . to1
    with' l = from1 . T.with' l . to1
    withTop l = from1 . T.withTop l . to1
    withTop' l = from1 . T.withTop' l . to1
    getLens = L.getLens
    getOpaqueConfig = from1 T.getOpaqueConfig


------------------------------------------------------------------------------
task :: Task b v a -> Handler b v (IO a)
task = T.task . to1


------------------------------------------------------------------------------
logErrors :: Task b v () -> Task b v ()
logErrors = from1 . T.logErrors . to1


------------------------------------------------------------------------------
log :: ByteString -> Task b v ()
log = G.log


------------------------------------------------------------------------------
getSnapletState :: Task b v (Snaplet v)
getSnapletState = Task T.getSnapletState


------------------------------------------------------------------------------
putSnapletState :: Snaplet v -> Task b v ()
putSnapletState = Task . T.putSnapletState


------------------------------------------------------------------------------
modifySnapletState :: (Snaplet v -> Snaplet v) -> Task b v ()
modifySnapletState = Task . T.modifySnapletState


------------------------------------------------------------------------------
getsSnapletState :: (Snaplet v -> a) -> Task b v a
getsSnapletState = Task . T.getsSnapletState
