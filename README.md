bytestring-trie
===============
[![Hackage version](https://img.shields.io/hackage/v/bytestring-trie.svg?style=flat)](https://hackage.haskell.org/package/bytestring-trie) 
[![Build Status](https://github.com/wrengr/bytestring-trie/workflows/ci/badge.svg)](https://github.com/wrengr/bytestring-trie/actions?query=workflow%3Aci)
[![Dependencies](https://img.shields.io/hackage-deps/v/bytestring-trie.svg?style=flat)](http://packdeps.haskellers.com/specific?package=bytestring-trie)

The bytestring-trie package provides an efficient implementation
of tries mapping `ByteString` to values.  The implementation is
based on Okasaki's big-endian patricia trees, à la `IntMap`.  We
first trie on the elements of `ByteString` and then trie on the
big-endian bit representation of those elements.  Patricia trees
have efficient algorithms for union and other merging operations,
but they're also quick for lookups and insertions.

If you are only interested in being able to associate individual
`ByteString`s to values, then you may prefer the `hashmap` package
which is faster for those only needing a map-like structure.  This
package is intended for those who need the extra capabilities that
a trie-like structure can offer (e.g., structure sharing to reduce
memory costs for highly redundant keys, taking the submap of all
keys with a given prefix, contextual mapping, extracting the minimum
and maximum keys, etc.)


## Install

This is a simple package and should be easy to install.  You should
be able to use the standard:

    $> cabal install bytestring-trie


## Portability

The implementation is quite portable, relying only on a few basic
language extensions. The complete list of extensions used is:

* CPP
* MagicHash 
* NoImplicitPrelude

## Links

* [Website](http://wrengr.org/)
* [Blog](http://winterkoninkje.dreamwidth.org/)
* [Twitter](https://twitter.com/wrengr)
* [Hackage](http://hackage.haskell.org/package/bytestring-trie)
* [GitHub](https://github.com/wrengr/bytestring-trie)
