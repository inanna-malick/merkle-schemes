-- FIXME: can I remove this?
{-# LANGUAGE IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Aeson.Orphans where

--------------------------------------------
import           Data.Aeson
--------------------------------------------
import           Util.RecursionSchemes
--------------------------------------------

-- FIXME: orphans, but these should exist
instance (ToJSON1 f, ToJSON a) => ToJSON (f a) where
  toJSON = liftToJSON toJSON toJSONList

instance (FromJSON1 f, FromJSON a) => FromJSON (f a) where
  parseJSON = liftParseJSON parseJSON parseJSONList

instance (Traversable f, FromJSON1 f) => FromJSON (Fix f) where
  parseJSON = anaM parseJSON

instance (Functor f, ToJSON1 f) => ToJSON (Fix f) where
  toJSON = cata toJSON
