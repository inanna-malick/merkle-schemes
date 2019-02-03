{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}


module Errors where

-- todo different name? hierarchy of errors?
-- | Errors that can occur while comparing two merkle trees
data MerkleTreeCompareError = HashValidationError | LookupError deriving Show
