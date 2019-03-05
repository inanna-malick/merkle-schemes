using the provided dockerfile:
```
> docker build -t pkinsky/hgit:demo . 
> docker run -it pkinsky/hgit:demo bash
```


let's create an empty repo:
```
bash:/# mkdir repo
bash:/# cd repo
bash:/repo# hgit init
bash:/repo# tree .hgit
.hgit
|-- state
`-- store
bash:/repo# cat .hgit/state
{"branches":{"default":"xqgnewjrafBb"},"currentBranch":"default"}
```

`hgit init` creates a `.hgit` directory with a `store` subdir that will be used to store various hash-identified objects (commits, dir trees, file blobs, etc) and a `state` file used to track repo state, which consists of the current branch and a map of branch names to commit pointers.

Repos start with the 'default' branch, which is initialized as pointer to the null commit ("xqgnewjrafBb" is a pointer to the null commit)

-- TODO: replace with '0'/'z'
-- TODO: state -> state.json
-- TODO: prev  -> parents

Ok, cool. Let's add some files to the repo.

```
bash:/repo# echo "totally a real project" > README.md
bash:/repo# echo "pls no infringe" > LICENSE

bash:/repo# hgit status
current branch: default
diffs:
	EntityCreated at LICENSE
	EntityCreated at README.md
bash:/repo# hgit commit "initial demo project"
bash:/repo# cat .hgit/state
{"branches":{"default":"xE9cQhCc0Vvk"},"substantiated":[],"currentBranch":"default"}

```

We've created a few simple files and commited them, updating the 'default' branch's commit pointer to "xE9cQhCc0Vvk". Let's take a look at the contents of our store, starting with that commit.

```
bash:/repo# tree .hgit
.hgit
|-- state
`-- store
    |-- xE9cQhCc0Vvk
    |-- xfUffAHam2Ob
    |-- xodfMZhuFTpg
    `-- yvdB7xqeqRxi


bash:/repo# cat .hgit/store/xE9cQhCc0Vvk 
{"root":"xfUffAHam2Ob","name":"initial demo project","type":"commit","prev":["xqgnewjrafBb"]}

bash:/repo# cat .hgit/store/xfUffAHam2Ob 
{"children":[{"path":"LICENSE","pointer":"yvdB7xqeqRxi","type":"file"},{"path":"README.md","pointer":"xodfMZhuFTpg","type":"file"}],"type":"dir"}

bash:/repo# cat .hgit/store/yvdB7xqeqRxi 
{"contents":"pls no infringe\n","type":"blob"}

bash:/repo# cat .hgit/store/xodfMZhuFTpg 
{"contents":"totally a real project\n","type":"blob"}
```

so here we have our commit ("xE9cQhCc0Vvk"), containing a pointer to the null commit and a root directory with our LICENSE and README.md files.

Now we're going to create a branch to demonstrate diffing branches (you'll have to take my word for it, but this is all done lazily):

```
bash:/repo# hgit status
current branch: default
diffs:
bash:/repo# hgit branch diff-example
bash:/repo# mkdir foo
bash:/repo# touch foo/bar
bash:/repo# touch baz

bash:/repo# tree
|-- LICENSE
|-- README.md
|-- baz
`-- foo
    `-- bar

bash:/repo# hgit status
current branch: diff-example
diffs:
	EntityCreated at baz
	EntityCreated at foo
    
bash:/repo# hgit commit "diff example branch a"
```

that's our first branch, 'diff-example'. Now lets hop back over to 'default' and make some changes there, too.

```
bash:/repo# hgit checkout default
bash:/repo# touch foo
bash:/repo# mkdir baz
bash:/repo# touch baz/bar

bash:/repo# tree
|-- LICENSE
|-- README.md
|-- baz
|   `-- bar
`-- foo

bash:/repo# hgit status
current branch: default
diffs:
	EntityCreated at baz
	EntityCreated at foo
    
bash:/repo# hgit commit "diff example branch b"

bash:/repo# hgit diff default diff-example
	DirReplacedWithFile at baz
	FileReplacedWithDir at foo
bash:/repo# hgit diff diff-example default
	FileReplacedWithDir at baz
	DirReplacedWithFile at foo
```

Here you can see `hgit diff` handling both the file-replaced-with-dir and dir-replaced-with-file cases.


Now let's try merging two branches!

```
bash:/repo# hgit branch merge-example

bash:/repo# hgit status
current branch: merge-example
diffs:

bash:/repo# mkdir branchtest
bash:/repo# touch branchtest/a
bash:/repo# echo "same same" > b

bash:/repo# hgit status
current branch: merge-example
diffs:
	EntityCreated at branchtest
	EntityCreated at b
bash:/repo# tree
.
|-- LICENSE
|-- README.md
|-- b
|-- baz
|   `-- bar
|-- branchtest
|   `-- a
`-- foo
```

Here's our branch - now let's make some changes in the 'default' branch that, while touching the same files and directories, are non-conflicting.

```
bash:/repo# hgit checkout default

bash:/repo# echo "same same" > b
bash:/repo# mkdir branchtest
bash:/repo# touch branchtest/b

bash:/repo# hgit status
current branch: default
diffs:
	EntityCreated at branchtest
	EntityCreated at b
bash:/repo# tree
.
|-- LICENSE
|-- README.md
|-- b
|-- baz
|   `-- bar
|-- branchtest
|   `-- b
`-- foo

bash:/repo# hgit commit "merge example branch b"
```

These changes are non-conflicting - both branches have created a file at 'b' with the same contents, and added files with different names to the directory 'branchtest'. They can be merged.

```
bash:/repo# hgit status
current branch: default
diffs:

bash:/repo# hgit merge merge-example "merge test"
bash:/repo# tree
.
|-- LICENSE
|-- README.md
|-- b
|-- baz
|   `-- bar
|-- branchtest
|   |-- a
|   `-- b
`-- foo
```


Here's a counterexample, in which a merge fails due to a non-resolvable change:

```
bash:/repo# hgit branch merge-fail-example
bash:/repo# echo "mario" > character
bash:/repo# hgit commit "merge fail branch a"
```

in one branch, we will create a file, 'character', containing the string "mario".

```
bash:/repo# hgit checkout default
bash:/repo# echo "luigi" > character
bash:/repo# hgit commit "merge fail branch b"
bash:/repo# hgit merge merge-fail-example "failing merge"
hgit: user error (merge nonviable due to: MergeViolation {mergeViolationPath = ["character"]})
```

As expected, this merge fails - the algorithm has no way of knowing which character to pick.
