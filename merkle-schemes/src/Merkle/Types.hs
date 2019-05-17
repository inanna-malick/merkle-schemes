{-# LANGUAGE MultiParamTypeClasses #-}

module Merkle.Types where

--------------------------------------------
import           Control.Applicative (Const(..))
import qualified Data.Aeson as AE
--------------------------------------------
import           Util.RecursionSchemes (Algebra)
--------------------------------------------

class Hashable h (f :: * -> *) where
  -- flatten a single layer of structure where all
  -- sub-layers are hash pointers down to a hash
  hash :: Algebra f (Const h f)


-- Hash (raw hash type) (functor type)
type Hash = Const

newtype HashTerm h f = HashTerm { unHashTerm :: f (Const h f)}

instance (AE.ToJSON h, AE.ToJSON1 f) => AE.ToJSON (HashTerm h f) where
  toJSON = AE.liftToJSON AE.toJSON AE.toJSONList . unHashTerm

instance (AE.FromJSON h, AE.FromJSON1 f) => AE.FromJSON (HashTerm h f) where
  parseJSON = fmap HashTerm . AE.liftParseJSON AE.parseJSON AE.parseJSONList
