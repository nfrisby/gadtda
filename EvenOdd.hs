{-# LANGUAGE CPP #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EvenOdd where

import GADTDA

import Common

--------------------------------------------------------------------------------

data Even = Nil | Even Int Odd deriving Show
data Odd = Odd Int Even deriving Show

--------------------------------------------------------------------------------

data TreeGADT :: * -> * where
  TreeE :: TreeGADT Even
  TreeO :: TreeGADT Odd
  TreeI :: TreeGADT Int

pTree :: Proxy TreeGADT
pTree = Proxy

instance El Even TreeGADT where proof = TreeE
instance El Odd TreeGADT where proof = TreeO
instance El Int TreeGADT where proof = TreeI

instance Children TreeGADT Even where
  allR _ = \r -> \case
    Nil -> pure Nil
    Even i o -> Even <$> r i <*> r o
  {-# INLINE allR #-}

instance Children TreeGADT Odd where
  allR _ = \r -> \case
    Odd i e -> Odd <$> r i <*> r e
  {-# INLINE allR #-}

instance Children TreeGADT Int where
  allR _ = const pure
  {-# INLINE allR #-}

#ifdef FAST
instance Implies TreeGADT where
  impl TreeE k = k
  impl TreeO k = k
  impl TreeI k = k
  {-# INLINE impl #-}
#endif FAST
