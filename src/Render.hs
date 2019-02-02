{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Render where

--------------------------------------------
import qualified Text.PrettyPrint as Y
--------------------------------------------
import           RecursionSchemes (Algebra, cata)
import           Types
--------------------------------------------

showMerkleTree :: MerkleTree -> String
showMerkleTree = Y.render . showMerkleTree'

showMerkleTree' :: MerkleTree -> Y.Doc
showMerkleTree' = cata alg
  where
    alg :: Algebra (HashIdentifiedEntity (NamedEntity Tree)) Y.Doc
    alg (Direct (Pointer hash) mtl) =
        mconcat [ Y.int hash -- todo: idea: shorten hash to get human readable pointers..
                , Y.text "#["
                , showMerkleTreeLayer id mtl
                , Y.text "]"
                ]
    alg (Indirect (Pointer hash)) =
      mconcat [Y.int hash, Y.text "#[ref only]"]

showConcreteMerkleTreeLayer :: ConcreteMerkleTreeLayer -> String
showConcreteMerkleTreeLayer = Y.render . showMerkleTreeLayer showMerkleTree'

showMerkleTreeLayer :: (a -> Y.Doc) -> MerkleTreeLayer a -> Y.Doc
showMerkleTreeLayer f mtl = case mtl of
    (NamedEntity name (Leaf body)) ->
      mconcat [Y.text $ mconcat ["Leaf(", name, "): "],  Y.text body]
    (NamedEntity name (Node children)) ->
        mconcat [ Y.text $ mconcat ["Node(", name, "): "]
                , Y.nest 2 $ Y.hsep $ fmap f children
                ]
