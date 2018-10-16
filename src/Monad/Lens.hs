{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Monad.Lens
    ( MonadLens, getLens
    )
where

-- layers --------------------------------------------------------------------
import           Control.Monad (liftM)
import           Control.Monad.Lift ( MonadTrans, lift)


-- lens ----------------------------------------------------------------------
import           Control.Lens.Lens (cloneLens)


-- snap ----------------------------------------------------------------------
import           Snap.Snaplet (Snaplet, Handler, Initializer)
import qualified Snap.Snaplet as S


-- snaplet-queue -------------------------------------------------------------
import           Control.Monad.Trans.Lens (LensT)
import qualified Control.Monad.Trans.Lens as L


------------------------------------------------------------------------------
class Monad m => MonadLens b v m | m -> b v where
    getLens :: Functor f => m ((v -> f v) -> b -> f b)


------------------------------------------------------------------------------
instance MonadLens (Snaplet b) (Snaplet v) (Handler b v) where
    getLens = liftM cloneLens S.getLens


------------------------------------------------------------------------------
instance MonadLens (Snaplet b) (Snaplet v) (Initializer b v) where
    getLens = cloneLens <$> S.getLens


------------------------------------------------------------------------------
instance Monad m => MonadLens b v (LensT b v m) where
    getLens = L.getLens


------------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-} (MonadTrans t, MonadLens b v m, Monad (t m)) =>
    MonadLens b v (t m)
  where
    getLens = lift getLens
