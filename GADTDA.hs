{-# LANGUAGE CPP #-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef FAST
#ifdef FORCE-SC
{-# LANGUAGE BangPatterns #-}
#endif
#endif FORCE-SC

module GADTDA
  (Proxy(..), El(..), prxProof, primitiveR,
   Children(..), allbuR
#ifdef FAST
   ,Implies(..)
#endif
  )  where

import Common

#ifdef FAST
#ifdef FORCE-SC
import GHC.Exts( SpecConstrAnnotation(..) )
#endif
#endif

--------------------------------------------------------------------------------

data Proxy (a :: * -> *) = Proxy

class El a g where proof :: g a

prxProof :: El a g => Proxy g -> g a
prxProof _ = proof

primitiveR :: El a g => (forall a. g a -> a -> m a) -> a -> m a
primitiveR r = r proof

--------------------------------------------------------------------------------

#ifdef FAST
class Children g a where
  allR :: Applicative f => Proxy g ->
    (forall a. (El a g) => a -> f a) -> a -> f a

allR_m :: (Children g a, Monad m) => Proxy g ->
    (forall a. (El a g) => a -> m a) -> a -> m a
allR_m = \prx f -> unwrapMonad . allR prx (WrapMonad . f)
{-# INLINE allR_m #-}

class Implies g where
  impl :: g a -> ((Children g a, El a g) => r) -> r

#ifdef FORCE-SC
data SPEC = SPEC | SPEC_dummy_to_prevent_WW
{-# ANN type SPEC ForceSpecConstr #-}
#endif

allbuR :: forall m g a. (Monad m, El a g, Implies g) => Proxy g ->
  (forall a. El a g => a -> m a) -> a -> m a
allbuR prx r =
#ifdef FORCE-SC
  let go :: forall a. SPEC -> g a -> a -> m a
      go !_SPEC prf = impl prf $
        r <=< allR_m prx (go _SPEC proof)
  in go SPEC (proof :: g a)
#else
  let go :: forall a. g a -> a -> m a
      go prf = impl prf $
        r <=< allR_m prx (go proof)
  in go (proof :: g a)
#endif FORCE-SC
{-# INLINE allbuR #-}

#else

class Children g a where
  allR :: Applicative i => Proxy g ->
    (forall a. (Children g a, El a g) => a -> i a) -> a -> i a

-- giving allR this type results in mutually recursive dictionaries, which ends
-- up preventing the static-argument transformation: one dictionary becomes a
-- loop-breaker

allR_m :: (Children g a, Monad m) => Proxy g ->
    (forall a. (Children g a, El a g) => a -> m a) -> a -> m a
allR_m = \prx f -> unwrapMonad . allR prx (WrapMonad . f)
{-# INLINE allR_m #-}

allbuR :: forall m g a. (Monad m, Children g a, El a g) => Proxy g ->
  (forall a. (Children g a, El a g) => a -> m a) -> a -> m a
allbuR prx r =
  let go :: forall a. (Children g a, El a g) => a -> m a
      go = r <=< allR_m prx go
  in go
{-# INLINE allbuR #-}

#endif FAST
