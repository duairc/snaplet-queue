{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Task
    ( TaskT, task, logErrors
    , with, with', withTop, withTop', getOpaqueConfig
    , getSnapletState, getsSnapletState, putSnapletState, modifySnapletState
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (Alternative)
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative)
#endif
import           Control.Exception (AsyncException, SomeException)
import           Control.Monad (MonadPlus, liftM)
#if MIN_VERSION_base(4, 9, 0)
import           Control.Monad.Fail (MonadFail)
#endif
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO)
import           Prelude hiding (log)


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
                     ( MonadTrans, lift, defaultLift3
                     , MonadTransControl, LayerResult, LayerState
                     , DefaultLayerResult3, DefaultLayerState3
                     , suspend, defaultSuspend3, resume, defaultResume3
                     , capture, defaultCapture3, extract, defaultExtract3
                     , MInvariant, hoistiso, defaultHoistiso3
                     , MFunctor, hoist, defaultHoist3
                     , MonadInner, Iso1, Codomain1, from1, to1
                     )
import           Monad.Catch (MonadCatch, catches)
import qualified Monad.Catch as E
import           Monad.Mask (MonadMask, mask)
import           Monad.Reader (MonadReader, reader, ask, local)
import           Monad.State (MonadState, state, get, put)
import           Monad.Throw (throw)


-- lens ----------------------------------------------------------------------
import           Control.Lens.Getter ((^.))
import           Control.Lens.Lens (cloneLens)
import           Control.Lens.Setter (set)


-- mtl -----------------------------------------------------------------------
import qualified Control.Monad.Reader.Class as MTL
import qualified Control.Monad.State.Class as MTL


-- snap ----------------------------------------------------------------------
import           Snap.Snaplet
                     ( Handler, Snaplet, SnapletLens, subSnaplet
                     , SnapletConfig, snapletConfig, snapletValue
                     )
import qualified Snap.Snaplet as S


-- snap-core -----------------------------------------------------------------
import           Snap.Core (liftSnap)
import           Snap.Internal.Core (sget, _snapLogError)


-- snaplet-queue -------------------------------------------------------------
import           Control.Monad.Trans.Lens (LensT, runLensT, zoom, focus)
import           Control.Monad.Trans.Log (LogT, runLogT)
import           Monad.Log (MonadLog, log)
import           Monad.Lens (MonadLens)


-- text ----------------------------------------------------------------------
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.State.Strict (StateT, evalStateT)


------------------------------------------------------------------------------
newtype TaskT b v m a =
    TaskT (LogT IO (LensT (Snaplet b) (Snaplet v) (StateT (Snaplet b) m)) a)
  deriving
    ( Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix, MonadIO
    , MonadLens (Snaplet b) (Snaplet v)
#if MIN_VERSION_base(4, 9, 0)
    , MonadFail
#endif
    )
deriving instance MonadInner IO m => MonadLog (TaskT b v m)


------------------------------------------------------------------------------
instance Iso1 (TaskT b v m) where
    type Codomain1 (TaskT b v m) =
        LogT IO (LensT (Snaplet b) (Snaplet v) (StateT (Snaplet b) m))
    to1 (TaskT m) = m
    from1 = TaskT


------------------------------------------------------------------------------
type instance LayerResult (TaskT b v) = DefaultLayerResult3
    (LogT IO) (LensT (Snaplet b) (Snaplet v)) (StateT (Snaplet b))
type instance LayerState (TaskT b v) = DefaultLayerState3
    (LogT IO) (LensT (Snaplet b) (Snaplet v)) (StateT (Snaplet b))


------------------------------------------------------------------------------
instance MonadTrans (TaskT b v) where
    lift = defaultLift3


------------------------------------------------------------------------------
instance MonadTransControl (TaskT b v) where
    suspend = defaultSuspend3
    resume = defaultResume3
    capture = defaultCapture3
    extract = defaultExtract3


------------------------------------------------------------------------------
instance MInvariant (TaskT b v) where
    hoistiso = defaultHoistiso3


------------------------------------------------------------------------------
instance MFunctor (TaskT b v) where
    hoist = defaultHoist3


------------------------------------------------------------------------------
instance Monad m => MonadReader v (TaskT b v m) where
    ask = get
    local f m = do
        current <- ask
        put (f current)
        result <- m
        put current
        return result


------------------------------------------------------------------------------
instance Monad m => MonadState v (TaskT b v m) where
    get = getsSnapletState (^. snapletValue)
    put = modifySnapletState . set snapletValue


------------------------------------------------------------------------------
instance Monad m => MTL.MonadReader v (TaskT b v m) where
    reader = reader
    ask = ask
    local = local


------------------------------------------------------------------------------
instance Monad m => MTL.MonadState v (TaskT b v m) where
    state = state
    get = get
    put = put


------------------------------------------------------------------------------
with :: Monad m => SnapletLens v v' -> TaskT b v' m a -> TaskT b v m a
with lens (TaskT m) = TaskT (hoist (zoom (cloneLens (subSnaplet lens))) m)


------------------------------------------------------------------------------
with' :: Monad m
    => SnapletLens (Snaplet v) v' -> TaskT b v' m a -> TaskT b v m a
with' lens (TaskT m) = TaskT (hoist (zoom (cloneLens lens)) m)


------------------------------------------------------------------------------
withTop :: Monad m => SnapletLens b v' -> TaskT b v' m a -> TaskT b v m a
withTop lens (TaskT m) = TaskT (hoist (focus (cloneLens (subSnaplet lens))) m)


------------------------------------------------------------------------------
withTop' :: Monad m
    => SnapletLens (Snaplet b) v' -> TaskT b v' m a -> TaskT b v m a
withTop' lens (TaskT m) = TaskT (hoist (focus (cloneLens lens)) m)


------------------------------------------------------------------------------
getOpaqueConfig :: Monad m => TaskT b v m SnapletConfig
getOpaqueConfig = getsSnapletState (^. snapletConfig)


------------------------------------------------------------------------------
getSnapletState :: Monad m => TaskT b v m (Snaplet v)
getSnapletState = TaskT get


------------------------------------------------------------------------------
putSnapletState :: Monad m => Snaplet v -> TaskT b v m ()
putSnapletState = TaskT . put


------------------------------------------------------------------------------
modifySnapletState :: Monad m => (Snaplet v -> Snaplet v) -> TaskT b v m ()
modifySnapletState f = getSnapletState >>= putSnapletState . f


------------------------------------------------------------------------------
getsSnapletState :: Monad m => (Snaplet v -> a) -> TaskT b v m a
getsSnapletState = flip liftM getSnapletState


------------------------------------------------------------------------------
task :: Monad m => TaskT b v m a -> Handler b v (m a)
task (TaskT m) = do
    logger <- liftSnap $ _snapLogError <$> sget
    lens <- S.getLens
    snaplet <- S.withTop' id S.getSnapletState
    return $ evalStateT (runLensT (runLogT m logger) (cloneLens lens)) snaplet


------------------------------------------------------------------------------
logErrors :: (MonadCatch m, MonadInner IO m, MonadMask m)
    => TaskT b v m () -> TaskT b v m ()
logErrors m = mask $ \unmask -> do
    unmask m `catches` [E.Handler async, E.Handler sync]
  where
    async (e :: AsyncException) = throw e
    sync (e :: SomeException) = do
        log (encodeUtf8 (T.pack (show e)))
        return ()
