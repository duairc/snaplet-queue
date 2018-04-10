{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Lens
    ( LensT, runLensT, zoom, focus
    , getLens
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (Alternative, Const (Const), getConst)
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative)
#endif
import           Control.Monad (MonadPlus, liftM)
#if MIN_VERSION_base(4, 9, 0)
import           Control.Monad.Fail (MonadFail)
#endif
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO)
#if MIN_VERSION_base(4, 4, 0)
import           Control.Monad.Zip (MonadZip)
#endif
import           Data.Functor.Identity (Identity (Identity), runIdentity)


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
                     ( MonadTrans, MonadTransControl, MInvariant, MFunctor
#if __GLASGOW_HASKELL__ >= 704
                     , LayerResult, LayerState
                     , suspend, resume, capture, extract
#endif
                     )
import           Monad.State (MonadState, state, get)
import           Monad.Reader (MonadReader, reader, ask, local)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Reader (ReaderT (ReaderT), runReaderT)


------------------------------------------------------------------------------
newtype Lens s a = Lens (forall f. Functor f => (s -> f s) -> a -> f a)


------------------------------------------------------------------------------
newtype LensT b v m a = LensT (ReaderT (Lens v b) m a)
  deriving
    ( MonadTrans, MInvariant, MFunctor
#if __GLASGOW_HASKELL__ < 704
    , MonadTransControl
#endif
    , Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix, MonadIO
#if MIN_VERSION_base(4, 4, 0)
    , MonadZip
#endif
#if MIN_VERSION_base(4, 9, 0)
    , MonadFail
#endif
    )


------------------------------------------------------------------------------
runLensT :: ()
    => LensT b v m a -> (forall f. Functor f => (v -> f v) -> b -> f b) -> m a
runLensT (LensT m) f = runReaderT m (Lens f)
{-# INLINE runLensT #-}


#if __GLASGOW_HASKELL__ >= 704
------------------------------------------------------------------------------
instance MonadTransControl (LensT b v) where
    suspend (LensT m) = suspend m
    {-# INLINE suspend #-}
    resume = LensT . resume
    {-# INLINE resume #-}
    capture = LensT capture
    {-# INLINE capture #-}
    extract _ (Identity a) = Right a
    {-# INLINE extract #-}


------------------------------------------------------------------------------
type instance LayerResult (LensT b v) = LayerResult (ReaderT (Lens v b))
type instance LayerState (LensT b v) = LayerState (ReaderT (Lens v b))


#endif
------------------------------------------------------------------------------
instance MonadReader b m => MonadReader v (LensT b v m) where
    reader f = LensT (ReaderT (\lens -> reader (f . view lens)))
    {-# INLINE reader #-}

    ask = LensT (ReaderT (\lens -> liftM (view lens) ask))
    {-# INLINE ask #-}

    local f (LensT (ReaderT m)) =
        LensT (ReaderT (\lens -> local (over lens f) (m lens)))
    {-# INLINE local #-}


------------------------------------------------------------------------------
instance MonadState b m => MonadState v (LensT b v m) where
    state f = LensT (ReaderT (state . go))
      where
        go lens b = fmap (flip (set lens) b) (f (view lens b))
    {-# INLINE state #-}

    get = LensT (ReaderT (\lens -> liftM (view lens) get))
    {-# INLINE get #-}


------------------------------------------------------------------------------
zoom :: (forall f. Functor f => (v' -> f v') -> v -> f v) -> LensT b v' m a
    -> LensT b v m a
zoom i (LensT (ReaderT m)) = LensT (ReaderT (m . compose (Lens i)))
{-# INLINE zoom #-}


------------------------------------------------------------------------------
focus :: (forall f. Functor f => (v' -> f v') -> b -> f b) -> LensT b v' m a
    -> LensT b v m a
focus i (LensT (ReaderT m)) = LensT (ReaderT (\_ -> m (Lens i)))
{-# INLINE focus #-}


------------------------------------------------------------------------------
getLens :: (Functor f, Applicative m) => LensT b v m ((v -> f v) -> b -> f b)
getLens = LensT (ReaderT $ \(Lens lens) -> pure lens)
{-# INLINE getLens #-}


------------------------------------------------------------------------------
view :: Lens s a -> a -> s
view (Lens l) = getConst . l Const
{-# INLINE view #-}


------------------------------------------------------------------------------
over :: Lens s a -> (s -> s) -> a -> a
over (Lens l) f = runIdentity . l (Identity . f)
{-# INLINE over #-}


------------------------------------------------------------------------------
set :: Lens s a -> s -> a -> a
set (Lens l) a = runIdentity . l (\_ -> Identity a)
{-# INLINE set #-}


------------------------------------------------------------------------------
compose :: Lens t s -> Lens s a -> Lens t a
compose (Lens m) (Lens l) = Lens (\f -> l (m f))
{-# INLINE compose #-}
