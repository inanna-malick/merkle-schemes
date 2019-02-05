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

TODO
- tests (property based, hedgehog)
- file search that lazily traverses some structure (filename, file body, exclusion filters..)
- server-client model, with server owning a filesystem store and local service using a network call one (json -> servant APIs)
-- command interface such that I can add things to store, get (print) them, get (to filesystem) them..
