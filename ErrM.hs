-- BNF Converter: Error Monad
-- Copyright (C) 2004  Author:  Aarne Ranta

-- This file comes with NO WARRANTY and may be used FOR ANY PURPOSE.
module ErrM where

-- the Error monad: like Maybe type with error msgs

import Control.Monad (MonadPlus(..), liftM, ap)
import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad.Trans

data Err a = Ok a | Bad String
  deriving (Read, Show, Eq, Ord)

instance Monad Err where
  return      = Ok
  fail        = Bad
  Ok a  >>= f = f a
  Bad s >>= _ = Bad s

instance Applicative Err where
  pure = Ok
  (Bad s) <*> _ = Bad s
  (Ok f) <*> o  = liftM f o


instance Functor Err where
  fmap = liftM

instance MonadPlus Err where
  mzero = Bad "Err.mzero"
  mplus (Bad _) y = y
  mplus x       _ = x

instance Alternative Err where
  empty = mzero
  (<|>) = mplus

-- newtype ErrT m a = ErrT { runErrT :: m (Err a) }

-- instance (Monad m) => Functor (ErrT m) where
--   fmap = liftM

-- instance (Monad m) => Applicative (ErrT m) where
--   pure = return
--   (<*>) = ap 

-- instance Monad m => Monad (ErrT m) where
--   return = ErrT . return . Ok
--   x >>= f = ErrT $ do errValue <- runErrT x
--                       case errValue of
--                         Bad s   -> return (Bad s)
--                         Ok value -> runErrT $ f value

-- instance MonadTrans ErrT where
--   lift = ErrT . liftM Ok