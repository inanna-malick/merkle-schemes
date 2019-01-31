to run:

```
cabal sandbox init
cabal install --only-dependencies
cabal build
cabal run
```

expected output, showing comparison of `examples/before` to `examples/after[1..3]`:

```
Running merkle-dag-compare...
comparing before to after1
(...)
[LeafModified ("leaf2","bar\n","baz\n")]

comparing before to after2
(...)
[FileReplacedWithDir "leaf2"]

comparing before to after3
(...)
[EntityRenamed "node1" "node2"]

```
