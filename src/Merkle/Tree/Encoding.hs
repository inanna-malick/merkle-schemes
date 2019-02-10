module Merkle.Tree.Encoding where

--------------------------------------------
import           Data.Aeson
import           Data.Hashable (Hashable(..))
import           Data.Text
--------------------------------------------
import           Merkle.Types (Pointer(..))
import           Merkle.Tree.Types
import           Util.MyCompose
--------------------------------------------

-- | merkle tree in which the top layer is known to be substantiated and
--   all sub-nodes are represented using hash addressed pointers
--   newtype and not type alias so we can have clean typeclass instances
newtype ShallowMerkleTreeLayer = SMTL { unSMTL :: (Named :+ Tree) Pointer}

instance Hashable ShallowMerkleTreeLayer where
  hashWithSalt s (SMTL (C (n, (Leaf contents))))
    = s `hashWithSalt` hash n `hashWithSalt` hash contents
  hashWithSalt s (SMTL (C (n, (Node contents))))
    = s `hashWithSalt` hash n `hashWithSalt` hash (fmap unPointer contents)

instance ToJSON ShallowMerkleTreeLayer where
    -- this generates a Value
    toJSON (SMTL (C (name, (Leaf body)))) =
        object [ "type" .= ("leaf" :: Text)
               , "name" .= pack name
               , "body" .= pack body
               ]
    toJSON (SMTL (C (name, (Node pointers)))) =
        object [ "type" .= ("node" :: Text)
               , "name" .= pack name
               , "children" .= toJSON (fmap unPointer pointers)
               ]

instance FromJSON ShallowMerkleTreeLayer where
    parseJSON = withObject "ShallowMerkleTreeLayer" $ \v -> do
        name <- v .: "name"
        typ  <- v .: "type"
        case typ of
          "node" -> do
              children <- fmap Pointer <$> v .: "children"
              pure . SMTL $ C (name, Node children)
          "leaf" -> do
              body <- v .: "body"
              pure . SMTL $ C (name, Leaf body)
          x -> fail $ "unsupported node type " ++ x
