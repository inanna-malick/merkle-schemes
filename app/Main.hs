module Main where

--------------------------------------------
import           Control.Monad.Reader (runReaderT)
--------------------------------------------
import           HGit.Runtime.Capabilities
import           HGit.Runtime.Commands
import           HGit.Runtime.Network
import           HGit.Runtime.RunCmd
--------------------------------------------


main :: IO ()
main = parse >>= \case
  (Left InitRepo) -> initRepo
  (Left (InitServer port)) -> mkLocalCaps >>= runServer port
  (Right repoCmd) -> do
    base  <- hgitBaseDir
    state <- readState
    stores  <- mkCaps state
    let caps = RepoCaps stores state base
    mNextState <- runReaderT (runCommand repoCmd) caps
    maybe (pure ()) writeState mNextState
