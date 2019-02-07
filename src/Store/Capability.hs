module Store.Capability where

--------------------------------------------
import           Merkle.Tree.Types
import           Util.MyCompose
import           Util.RecursionSchemes
--------------------------------------------

-- | Some capability to interact with some hash addressed merkle tree store
data Store m
  = Store
  { -- | given a pointer, fetch the corresponding entity. Provides type-level guarantee that
    --   at least one level of structure is fetched '(Named :+ Tree)' while allowing for multiple
    --   levels of structure to be returned in one call via 'MerkleTree' subnode type
    sDeref :: Pointer -> m $ Named :+ Tree $ LazyMerkleTree
    -- | given a shallow layer of structure with subnodes identified by a pointer, store it.
    -- this allows for each store to use its own hash algorithm - not sure if I like that
  , sUploadShallow :: Named :+ Tree $ Pointer -> m Pointer
  }

-- | consume effectful tree, annotate nodes with hash,
--   adds them to some global store during this traversal
-- NOTE: fully consumes potentially-infinite effectful stream and may not terminate
addTreeToStore
  :: forall m
   . Monad m
  => Store m
  -> Fix (Named :+ Tree)
  -> m $ Fix $ WithHash :+ (Named :+ Tree)
addTreeToStore store = cata alg
  where
    alg :: Algebra (Named :+ Tree) (m $ Fix $ WithHash :+ Named :+ Tree)
    alg (C (name, entity')) = do
      entity <- (name,) <$> case entity' of
        Leaf body -> pure $ Leaf body
        Node children -> do
          children' <- traverse id children
          pure $ Node children'
      p <- sUploadShallow store . fmap pointer $ C entity
      pure . Fix . C . (p,) $ C entity
