module Control.RecursionSchemes where

newtype Fix f = Fix { unFix :: f (Fix f) }

-- | generalized fold
cata
  :: Functor f
  => (f a -> a)
  -> Fix f
  -> a
cata alg x = alg             -- a
           . fmap (cata alg) -- f a
           . unFix           -- f (Fix f)
           $ x               -- Fix f


-- | generalized unfold
ana :: Functor f
    => (a -> f a)
    -> a
    -> Fix f
ana alg x = Fix            -- Fix f
          . fmap (ana alg) -- f (Fix f)
          . alg            -- f a
          $ x              -- a

-- | generalized monadic fold
cataM
  :: (Monad m, Traversable f)
  => (f a -> m a)
  -> Fix f
  -> m a
cataM alg x = do
    let fFixF = unFix x              -- f (Fix f)
    fA <- traverse (cataM alg) fFixF -- m (f a)
    alg fA                           -- m a

-- | generalized monadic unfold
anaM :: (Monad m, Traversable f)
     => (a -> m (f a))
     -> a
     -> m (Fix f)
anaM alg x = do
  fA    <- alg x                  -- m (f a)
  fFixF <- traverse (anaM alg) fA -- m (f (Fix f))
  pure $ Fix fFixF                -- m (Fix f) 
