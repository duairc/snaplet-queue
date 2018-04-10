{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Log
    ( Logger, LogT, runLogT, log
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (Alternative)
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative)
#endif
import           Control.Monad (MonadPlus)
#if MIN_VERSION_base(4, 9, 0)
import           Control.Monad.Fail (MonadFail)
#endif
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Zip (MonadZip)
import           Prelude hiding (log)


-- bytestring ----------------------------------------------------------------
import           Data.ByteString (ByteString)


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
                     ( MonadTrans
                     , MonadTransControl, LayerResult, LayerState
                     , suspend, resume, capture, extract
                     , defaultSuspend, defaultResume, defaultCapture
                     , defaultExtract
                     , MInvariant, MFunctor
                     , MonadInner, liftI
                     , Iso1, Codomain1, from1, to1
                     )


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Reader (ReaderT (ReaderT), runReaderT)


------------------------------------------------------------------------------
type Logger m = ByteString -> m ()


------------------------------------------------------------------------------
newtype LogT i m a = LogT (ReaderT (Logger i) m a)
  deriving
    ( MonadTrans, MInvariant, MFunctor
    , Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix, MonadZip
    , MonadIO
#if MIN_VERSION_base(4, 9, 0)
    , MonadFail
#endif
    )


------------------------------------------------------------------------------
instance Iso1 (LogT i m) where
    type Codomain1 (LogT i m) = ReaderT (Logger i) m
    to1 (LogT m) = m
    from1 = LogT


------------------------------------------------------------------------------
type instance LayerResult (LogT i) = LayerResult (ReaderT (Logger i))
type instance LayerState (LogT i) = LayerState (ReaderT (Logger i))


------------------------------------------------------------------------------
instance MonadTransControl (LogT i) where
    suspend = defaultSuspend
    resume = defaultResume
    capture = defaultCapture
    extract = defaultExtract


------------------------------------------------------------------------------
runLogT :: LogT i m a -> Logger i -> m a
runLogT (LogT m) = runReaderT m
{-# INLINE runLogT #-}


------------------------------------------------------------------------------
log :: MonadInner i m => ByteString -> LogT i m ()
log message = LogT $ ReaderT $ \logger -> liftI $ logger message
{-# INLINE log #-}
