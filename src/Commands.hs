
module Commands (MetaCommand(..), RepoCommand(..), parse) where

import Options.Applicative
import Data.Semigroup ((<>))

import HGit.Types.HGit


parse :: IO (MetaCommand `Either` RepoCommand)
parse = execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "do some git/mercurial type stuff"
     <> header "hgit - an implementation of core git/mercurial features using recursion schemes" )

-- initialize repo structure
data MetaCommand
  = InitRepo
  | InitServer Int

data RepoCommand
  -- switch directory state to that of new branch (nuke and rebuild via store)
  -- fails if any changes exist in current dir (diff via status /= [])
  = CheckoutBranch BranchName
  | SetRemoteRepo String Int
  | UnsetRemoteRepo
  -- neither look at or modify filesystem state
  | PullBranch BranchName -- todo plan for name conflict - probably just overwrite, implicit -f, lmao lmao etc
  | PushBranch BranchName -- todo plan for name conflict - probably just overwrite, implicit -f, lmao lmao etc
  -- create new branch with same root commit as current branch. changes are fine
  | MkBranch BranchName
  -- merge some branch into the current one (requires no changes)
  | MkMergeCommit BranchName CommitMessage
  | MkCommit CommitMessage
  | GetStatus -- get status of current repo (diff current state vs. that of last commit on branch)
  | GetDiff BranchName BranchName

parser :: Parser (MetaCommand `Either` RepoCommand)
parser
  = subparser
     ( command "pull"     (info (fmap Right pullOptions)     ( progDesc "pull a branch"         ))
    <> command "push"     (info (fmap Right pushOptions)     ( progDesc "push a branch"         ))
    <> command "setrem"   (info (fmap Right setremOptions)   ( progDesc "set remote addr"       ))
    <> command "unsetrem" (info (fmap Right unsetremOptions) ( progDesc "unset remote addr"     ))
    <> command "checkout" (info (fmap Right checkoutOptions) ( progDesc "checkout a branch"     ))
    <> command "branch"   (info (fmap Right branchOptions)   ( progDesc "create a new branch"   ))
    <> command "init"     (info (fmap Left  initROptions)    ( progDesc "create a new repo"     ))
    <> command "server"   (info (fmap Left  initSOptions)    ( progDesc "initialize a server"   ))
    <> command "commit"   (info (fmap Right commitOptions)   ( progDesc "create a new commit"   ))
    <> command "status"   (info (fmap Right statusOptions)   ( progDesc "show repo status"      ))
    <> command "diff"     (info (fmap Right diffOptions)     ( progDesc "show diff of branches" ))
    <> command "merge"    (info (fmap Right mergeOptions)    ( progDesc "merge a branch into the current one" ))
      )
  where
    setremOptions
        = SetRemoteRepo
      <$> strArgument
          ( metavar "PATH"
         <> help "remote repo path"
          )
      <*> argument auto
          ( metavar "PORT"
         <> help "remote repo port"
         <> showDefault
         <> value 8888
          )
    unsetremOptions = pure UnsetRemoteRepo

    pullOptions
        = PullBranch
      <$> strArgument
          ( metavar "BRANCHNAME"
         <> help "branch to pull"
          )
    pushOptions
        = PushBranch
      <$> strArgument
          ( metavar "BRANCHNAME"
         <> help "branch to push"
          )
    checkoutOptions
        = CheckoutBranch
      <$> strArgument
          ( metavar "BRANCHNAME"
         <> help "branch to checkout"
          )
    branchOptions
        = MkBranch
      <$> strArgument
          ( metavar "BRANCHNAME"
         <> help "branch to create"
          )
    initSOptions  = pure $ InitServer 8888
    initROptions  = pure InitRepo
    commitOptions
        = MkCommit
      <$> strArgument
          ( metavar "MESSAGE"
         <> help "commit msg"
          )
    mergeOptions
        = MkMergeCommit
      <$> strArgument
          ( metavar "BRANCHNAME"
         <> help "branch to merge into the current one"
          )
      <*> strArgument
          ( metavar "MESSAGE"
         <> help "commit msg"
          )
    statusOptions  = pure GetStatus
    diffOptions
        = GetDiff
      <$> strArgument
          ( metavar "BEFORE"
         <> help "'before' branch name"
          )
      <*> strArgument
          ( metavar "AFTER"
         <> help "'after' branch name"
          )
