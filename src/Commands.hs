
module Commands (Command(..), parse)where

import Options.Applicative
import Data.List.NonEmpty
import Data.Semigroup ((<>))

import HGit.Types


parse :: IO Command
parse = execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "do some git/mercurial type stuff"
     <> header "hgit - an implementation of core git/mercurial features using recursion schemes" )

data PathMatcher = WildCard -- | Concrete String

data Command
  -- switch directory state to that of new branch (nuke and rebuild via store)
  -- fails if any changes exist in current dir (diff via status /= [])
  = CheckoutBranch BranchName (NonEmpty PathMatcher)
  -- create new branch with same root commit as current branch. changes are fine
  | MkBranch BranchName
  -- merge some branch into the current one (requires no changes)
  | MkMergeCommit BranchName CommitMessage
  -- add everything in current repo to the current branch in a new commit w/ msg
  -- and update current branch
  -- initialize repo in current directory with provided name
  | InitRepo
  | MkCommit CommitMessage
  | GetStatus -- get status of current repo (diff current state vs. that of last commit on branch)
  | GetDiff BranchName BranchName

parser :: Parser Command
parser
  = subparser
     ( command "checkout" (info checkoutOptions  ( progDesc "checkout a branch"     ))
    <> command "branch"   (info branchOptions    ( progDesc "create a new branch"   ))
    <> command "init"     (info initOptions      ( progDesc "create a new repo"     ))
    <> command "commit"   (info commitOptions    ( progDesc "create a new commit"   ))
    <> command "status"   (info statusOptions    ( progDesc "show repo status"      ))
    <> command "diff"     (info diffOptions      ( progDesc "show diff of branches" ))
    <> command "merge"    (info mergeOptions     ( progDesc "merge a branch into the current one" ))
      )
  where
    checkoutOptions
        = CheckoutBranch
      <$> strArgument
          ( metavar "BRANCHNAME"
         <> help "branch to checkout"
          )
      <*> pure (pure WildCard) -- placeholder
    branchOptions
        = MkBranch
      <$> strArgument
          ( metavar "BRANCHNAME"
         <> help "branch to create"
          )
    initOptions  = pure InitRepo
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
