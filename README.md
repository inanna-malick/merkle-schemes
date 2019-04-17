
merkle-schemes
----

This repo contains tools for working with any merkle-type data structure. This includes blockchains (merkle linked lists), directory trees as stored in git and mercurial, and merkle DAGs like those used by IPFS.

What all these technologies have in common is that they use some recursive data structure and that each layer of this structure is uniquely identified by its cryptographic hash. Let's take a look at a blockchain in this format:

```
data Txn = Txn { from :: String, to :: String, amt :: Int }
data BlockChainLayer a = Block [Txn] a | GenesisBlock
```

We can then represent a blockchain as the fixed point of `BlockChain` using `Fix`
```
newtype Fix = Fix { unFix :: f (Fix f) }

blockchain :: Fix BlockChain
blockchain = 
  let previousBlock = Fix GenesisBlock
   in Fix $ Block [Txn "alice" "bob" 5] previousBlock
```

If you're not familiar with this style, you may want to read this blog post https://blog.sumtypeofway.com/an-introduction-to-recursion-schemes. You don't really need to do so to finish reading this README, but it is interesting context that will help you understand how this library is implemented.


Because we've defined it with a type parameter `a` for the type of the pointer to the next we can also represent 'shallow' layers of the structure where we have a single block in a blockchain that contains not further recursive structure but just a hash pointer to 

```
type Hash = Int

shallowBlockChainLayer :: BlockChainLayer Hash
shallowBlockChainLayer = 
  let previousBlockHash = 12345
   in Block [Txn "alice" "bob" 5] previousBlockHash

hashlayer :: BlockChainLayer Hash -> Hash
hashlayer = undefined -- implemented in the repo
```

If you're familiar with recursion schemes, you'll recognize that `hashLayer` is an algebra as used by functions like `cata`. If you're not, writing our data structure using `Fix BlockChainLayer` lets us fold over the entire structure by providing a single function of type `BlockChainLayer Hash -> Hash` that's then used to collaps the entire chain of blocks down to a single hash value.

This library provides a set of tools for working with such structures, including tools to annotate each layer with its hash, a variety of hash-addressed stores that support uploading and downloading layers of such structure using the common abstraction `Store m f`. It also provides some tooling to handle lazily unfolding a structure from a hash, such that `Hash`-prefixed effectful fetch actions are interleaved with each layer of structure `f`. For example, a lazy blockchain would look like this:

```
type LazyBlockChain m = Fix ((,) Hash `Compose` m `Compose` BlockChainLayer)
```

There are some example block chains (including one very similar to the example above) in the tests directory of this project. This library was developed during the process of developing a git/mercurial clone designed to demonstrate some concepts (lazy diff and merge algorithms, seamless fallback from local to remote stores) https://github.com/pkinsky/hgit
