{-# LANGUAGE FlexibleInstances, FunctionalDependencies, TypeFamilies, Rank2Types, UndecidableInstances #-}

module Control.Monad.Trans.Control.Identity (

  -- * MonadTransControlIdentity
    MonadTransControlIdentity (..)
  , RunIdentity
  -- * Defaults
  , defaultLiftWithIdentity
  , RunIdentityDefault

  -- * MonadBaseControlIdentity
  , MonadBaseControlIdentity (..)
  , RunIdentityInBase
  -- * Defaults
  , defaultLiftBaseWithIdentity
  , RunIdentityInBaseDefault

  -- * MonadTransFunctor
  , MonadTransFunctor (..)
  , hoistTrans
) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader

{- | The 'MonadTransControlIdentity' type class is a stronger version of
  'MonadTransControl':

  'MonadTransControl' instances are aware of the monadic state of the
  transformer and allow to save and restore this state.
  'MonadTransControl' instances exist for exactly those transformers,
  that don't have any monadic state.

  So for any instance of this class this should hold:

  @forall a. 'StT' t a ~ a@

  This can't be given as a constraint to the class due to limitations
  regarding the @TypeFamilies@ extension.
-}
class MonadTransControl t => MonadTransControlIdentity t where
  liftWithIdentity :: Monad m => ((RunIdentity t) -> m b) -> t m b

type RunIdentity t = forall n b. Monad n => t n b -> n b

type RunIdentityDefault t = forall n b. (Monad n, StT t b ~ b) => t n b -> n b

defaultLiftWithIdentity :: (Monad m, MonadTransControl t)
                        => ((RunIdentityDefault t) -> m b)
                        -> t m b
defaultLiftWithIdentity = liftWith

instance MonadTransControlIdentity IdentityT where
  liftWithIdentity = defaultLiftWithIdentity

instance MonadTransControlIdentity (ReaderT r) where
  liftWithIdentity = defaultLiftWithIdentity

-- | Instances of this class are the same as instances of 'MonadUnliftIO', but for any base monad.
class MonadBaseControl b m => MonadBaseControlIdentity b m | m -> b where
  liftBaseWithIdentity :: ((RunIdentityInBase m b) -> b a) -> m a

type RunIdentityInBase m b = forall a. m a -> b a

type RunIdentityInBaseDefault t m b = forall a. Monad m => t m a -> b a

defaultLiftBaseWithIdentity :: (MonadBaseControlIdentity b m, MonadTransControlIdentity t)
                            => ((RunIdentityInBaseDefault t m b) -> b a)
                            -> t m a
defaultLiftBaseWithIdentity inner = liftWithIdentity $ \ runId ->
  liftBaseWithIdentity $ \ runIdInBase ->
    inner $ runIdInBase . runId

instance MonadBaseControl b b => MonadBaseControlIdentity b b where
  liftBaseWithIdentity inner = inner id

instance MonadBaseControlIdentity b m => MonadBaseControlIdentity b (IdentityT m) where
  liftBaseWithIdentity = defaultLiftBaseWithIdentity

instance MonadBaseControlIdentity b m => MonadBaseControlIdentity b (ReaderT r m) where
  liftBaseWithIdentity = defaultLiftBaseWithIdentity

class MonadTransControlIdentity t => MonadTransFunctor t where -- TODO: does the superclass here really make sense
  liftMap :: (m a -> n b) -> t m a -> t n b

instance MonadTransFunctor IdentityT where
  liftMap f = IdentityT . f . runIdentityT

instance MonadTransFunctor (ReaderT r) where
  liftMap f m = ReaderT $ f . runReaderT m

hoistTrans :: (MonadBaseControl b m, MonadBaseControl b (t m), MonadTransFunctor t)
           => t b a
           -> t m a
hoistTrans a = (=<<) restoreM $ liftBaseWith $ \ runInBase ->
                 runInBase $ liftMap liftBase $ a
