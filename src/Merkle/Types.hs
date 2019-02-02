{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}

module Merkle.Types where

--------------------------------------------
import qualified Data.Hashable as Hash
import           GHC.Generics (Generic)
--------------------------------------------
import           Util.RecursionSchemes (Term(..))
--------------------------------------------

type Hash = Int

-- | Pointer to a hash-identified entity
newtype Pointer = Pointer { unPointer :: Hash }
  deriving (Eq, Ord, Show, Generic)
instance Hash.Hashable Pointer

-- | Some entity (f a) identified by a hash pointer. Can either be a direct or indirect reference
data HashIdentifiedEntity (f :: * -> *) (a :: *)
  = Direct   Pointer (f a) -- node id is included in node metadata of direct ref (pointer)
  | Indirect Pointer       -- indirect ref is just a pointer in some hash-addressed store
  deriving (Eq, Show, Functor)

mtPointer :: Term (HashIdentifiedEntity f) -> Pointer
mtPointer (In (Direct p _)) = p
mtPointer (In (Indirect p)) = p
