module Search where

--------------------------------------------
import           Data.List (isSubsequenceOf)
import           Util.MyCompose
import           Util.RecursionSchemes
import           Merkle.Tree.Types
--------------------------------------------

-- | append lazy stream? this should work lol
andThen
  :: forall m x
   . Monad m
  => Fix $ m :+ Maybe :+ (,) x
  -> Fix $ m :+ Maybe :+ (,) x
  -> Fix $ m :+ Maybe :+ (,) x
andThen a b = cata alg a
  where
    alg :: Algebra (m :+ Maybe :+ (,) x)
                   (Fix $ m :+ Maybe :+ (,) x)
    alg (C e) = Fix $ C $ e >>= worker

    worker
      :: Maybe :+ (,) x $ Fix $ m :+ Maybe :+ (,) x
      -> m $ Maybe :+ (,) x $ Fix $ m :+ Maybe :+ (,) x
    worker (C (Just (x, fix))) = pure . C $ Just (x, fix)
    worker (C Nothing) = f b

    f :: Fix $ m :+ Maybe :+ (,) x
      -> m $ Maybe :+ (,) x $ Fix $ m :+ Maybe :+ (,) x
    f (Fix (C x)) = x

-- strict stream consume, may not terminate, etc etc
consume
  :: forall m a
   . Monad m
  => (a -> m ())
  -> Fix $  m :+ Maybe :+ (,) a
  -> m ()
consume f = cata (\(C e) -> do
                       (C e') <- e
                       case e' of
                         Just (a,m) -> do
                           f a
                           m
                         Nothing -> pure ()
                   )

-- | lazy search that produces a stream of leaf file body from a tree
search
  :: forall m
   . Monad m
  => String
  -> Fix $ m :+ Named :+ Tree
  -> Fix $ m :+ Maybe :+ (,) String
search query = cata alg
  where
    alg ::  Algebra (m :+ Named :+ Tree)
                   (Fix $ m :+ Maybe :+ (,) String)
    alg (C e) = Fix $ C $ e >>= worker

    worker :: Named :+ Tree $ Fix $ m :+ Maybe :+ (,) String
           -> m $ Maybe :+ (,) String $ Fix $ m :+ Maybe :+ (,) String
    worker (C (_, Node ns))
      = getCompose . unfix $ foldr andThen (Fix (C $ pure $ C Nothing)) ns
    worker (C (_, Leaf body))
      | query `isSubsequenceOf` body
          = pure $ C $ Just (body, Fix $ C $ pure $ C Nothing)
      | otherwise = pure $ C Nothing
