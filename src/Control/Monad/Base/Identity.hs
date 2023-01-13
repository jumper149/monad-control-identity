{-# LANGUAGE RankNTypes #-}

module Control.Monad.Base.Identity where

import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Data.Functor.Identity

{- | 'MonadBaseIdentity' is a type class for monads, that don't hold any monadic state just like
  'Control.Monad.Trans.Control.Identity.MonadBaseControlIdentity'.

  But instances of 'MonadBaseIdentity' don't even have any monadic state in their base monad.
  That's why we can completely unlift values out of the monad.
-}

class Monad m => MonadBaseIdentity m where
  withIdentity :: ((forall x. m x -> x) -> a) -> m a

instance MonadBaseIdentity Identity where
  withIdentity f = Identity $ f runIdentity

instance MonadBaseIdentity ((->) r) where
  withIdentity f r = f $ \ g -> g r

instance MonadBaseIdentity m => MonadBaseIdentity (IdentityT m) where
  withIdentity f = IdentityT $ withIdentity $ \identity ->
    f (identity . runIdentityT)

instance MonadBaseIdentity m => MonadBaseIdentity (ReaderT r m) where
  withIdentity f = ReaderT $ \r -> withIdentity $ \identity ->
    f (identity . (`runReaderT` r))
