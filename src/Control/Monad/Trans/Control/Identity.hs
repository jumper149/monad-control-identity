{-# LANGUAGE FlexibleInstances, FunctionalDependencies, TypeFamilies, Rank2Types, UndecidableInstances #-}

module Control.Monad.Trans.Control.Identity (

-- * MonadTransControlIdentity
  MonadTransControlIdentity (..)
{- | 'MonadTransControlIdentity' instances can easily be created for
  monad transformers, because of the superclass 'MonadTransControl':

@
newtype ExampleT = ...
  deriving ('Monad', 'Control.Monad.Trans.Class.MonadTrans')

instance 'MonadTransControl' ExampleT where
  ...

instance 'MonadTransControlIdentity' ExampleT where
  'liftWithIdentity' = 'defaultLiftWithIdentity'
@
-}
, defaultLiftWithIdentity

-- * MonadBaseControlIdentity
-- | Regarding the 'IO' base monad this can be seen as an alternative,
-- but equivalent, way to implement 'MonadUnliftIO'.
, MonadBaseControlIdentity (..)
{- | Just like 'MonadTransControlIdentity', 'MonadBaseControlIdentity'
  instances can easily be created for monad transformers:

@
instance 'MonadBaseControlIdentity' b m => 'MonadBaseControlIdentity' b (ExampleT m) where
  'liftBaseWithIdentity' = 'defaultLiftBaseWithIdentity'
@
  -}
, defaultLiftBaseWithIdentity

) where

import Control.Monad.ST.Lazy as L
import Control.Monad.ST.Strict as S
import Control.Monad.STM
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Data.Functor.Identity

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

instance MonadBaseControlIdentity IO IO where
  liftBaseWithIdentity inner = inner id

instance MonadBaseControlIdentity Maybe Maybe where
  liftBaseWithIdentity inner = inner id

instance MonadBaseControlIdentity (Either e) (Either e) where
  liftBaseWithIdentity inner = inner id

instance MonadBaseControlIdentity [] [] where
  liftBaseWithIdentity inner = inner id

instance MonadBaseControlIdentity ((->) r) ((->) r) where
  liftBaseWithIdentity inner = inner id

instance MonadBaseControlIdentity Identity Identity where
  liftBaseWithIdentity inner = inner id

instance MonadBaseControlIdentity STM STM where
  liftBaseWithIdentity inner = inner id

instance MonadBaseControlIdentity (S.ST s) (S.ST s) where
  liftBaseWithIdentity inner = inner id

instance MonadBaseControlIdentity (L.ST s) (L.ST s) where
  liftBaseWithIdentity inner = inner id

instance MonadBaseControlIdentity b m => MonadBaseControlIdentity b (IdentityT m) where
  liftBaseWithIdentity = defaultLiftBaseWithIdentity

instance MonadBaseControlIdentity b m => MonadBaseControlIdentity b (ReaderT r m) where
  liftBaseWithIdentity = defaultLiftBaseWithIdentity
