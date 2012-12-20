{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Increase where

import Common
import EvenOdd
import GADTDA

increase :: Int -> Even -> Either String Even
increase k = allbuR pTree $ primitiveR $ \case
  TreeI -> pure . (+k)
  _ -> return
