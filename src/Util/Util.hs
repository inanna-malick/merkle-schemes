module Util.Util where -- this module name is a sin against god, todo something descriptive

import           Control.Monad.Except

mapErrUtil :: Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
mapErrUtil f = mapExceptT (fmap (either (Left . f) Right))
