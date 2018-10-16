{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Monad.Log
    ( MonadLog, log
    )
where

-- base ----------------------------------------------------------------------
import           Prelude hiding (log)


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift (MonadInner, MonadTrans, lift)


-- snap ----------------------------------------------------------------------
import           Snap.Snaplet (Handler)


-- snap-core -----------------------------------------------------------------
import           Snap.Core (Snap, logError)


-- snaplet-queue -------------------------------------------------------------
import           Control.Monad.Trans.Log (Logger, LogT)
import qualified Control.Monad.Trans.Log as L


------------------------------------------------------------------------------
class Monad m => MonadLog m where
    log :: Logger m


------------------------------------------------------------------------------
instance MonadLog Snap where
    log = logError


------------------------------------------------------------------------------
instance MonadLog (Handler b v) where
    log = logError


------------------------------------------------------------------------------
instance MonadInner i m => MonadLog (LogT i m) where
    log = L.log


------------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-} (MonadTrans t, MonadLog m, Monad (t m)) =>
    MonadLog (t m)
  where
    log = lift . log
