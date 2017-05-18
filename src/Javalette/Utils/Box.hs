{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Javalette.Utils.Box
  ( Box()
  , box
  , unbox
  , unboxWith
  , both
  , embed
  , projection
  , swap
  , Transfer(..)
  ) where

data Box m b = forall a . Box (m a) (a -> b)

box :: m a -> (a -> b) -> Box m b
box = Box

embed :: m a -> Box m a
embed act = Box act id

projection :: Functor m => Box m b -> m b
projection (Box a p) = p <$> a

swap :: (forall a . m a -> n a) -> Box m t -> Box n t
swap f (Box m t) = Box (f m) t

both :: Applicative m => Box m b0 -> Box m b1 -> Box m (b0, b1)
Box a0 f0 `both` Box a1 f1
  = Box ((,) <$> a0 <*> a1) $ \(a0', a1') -> (f0 a0', f1 a1')

instance (Applicative m, Monoid b) => Monoid (Box m b) where
  mempty = Box (pure ()) (const mempty)
  Box a0 f0 `mappend` Box a1 f1
    = Box ((,) <$> a0 <*> a1)
    $ \(a0', a1') -> f0 a0' `mappend` f1 a1'

class Transfer m n where
  transfer :: m a -> n a

unbox :: Transfer m n => Functor n => Box m b -> n b
unbox = unboxWith transfer

unboxWith :: Functor n => (forall a . m a -> n a) -> Box m b -> n b
unboxWith t (Box a f) = f <$> t a
