{-# LANGUAGE FlexibleInstances, FunctionalDependencies, TypeFamilies, Rank2Types, UndecidableInstances #-}

module Control.Monad.Trans.Control.Identity (

  -- * MonadTransControlIdentity
    MonadTransControlIdentity (..)
  , defaultLiftWithIdentity

  -- * MonadBaseControlIdentity
  -- | Regarding the 'IO' base monad this can be seen as an alternative
  -- way to implement 'MonadUnliftIO'.
  , MonadBaseControlIdentity (..)
  , defaultLiftBaseWithIdentity

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
  'MonadTransControlIdentity' instances on the other hand exist only for
  exactly those transformers, that don't have any monadic state.

  So for any instance of this class this should hold:

  @forall a. 'StT' t a ~ a@

  This can't be given as a constraint to the class due to limitations
  regarding the @TypeFamilies@ extension.
-}
class MonadTransControl t => MonadTransControlIdentity t where
  liftWithIdentity :: Monad m => ((forall x. t m x -> m x) -> m a) -> t m a

defaultLiftWithIdentity :: (Monad m, MonadTransControl t)
                        => ((forall x. StT t x ~ x => t m x -> m x) -> m a)
                        -> t m a
defaultLiftWithIdentity = liftWith

instance MonadTransControlIdentity IdentityT where
  liftWithIdentity = defaultLiftWithIdentity

instance MonadTransControlIdentity (ReaderT r) where
  liftWithIdentity = defaultLiftWithIdentity

{- | The 'MonadBaseControlIdentity' type class is a stronger version of
  'MonadBaseControl'.

  Just like 'MonadTransControlIdentity' instances of
  'MonadBaseControlIdentity' hold no monadic state:

  @forall a. 'StM' m a ~ a@
-}
class MonadBaseControl b m => MonadBaseControlIdentity b m | m -> b where
  liftBaseWithIdentity :: ((forall x. m x -> b x) -> b a) -> m a

defaultLiftBaseWithIdentity :: (MonadBaseControlIdentity b m, MonadTransControlIdentity t)
                            => ((forall x. t m x -> b x) -> b a)
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
