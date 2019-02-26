bytestring-trie
===============
[![Hackage version](https://img.shields.io/hackage/v/bytestring-trie.svg?style=flat)](https://hackage.haskell.org/package/bytestring-trie) 
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/bytestring-trie.svg?style=flat)](http://packdeps.haskellers.com/specific?package=bytestring-trie)
[![TravisCI Build Status](https://img.shields.io/travis/wrengr/bytestring-trie.svg?style=flat)](https://travis-ci.org/wrengr/bytestring-trie) 
[![CircleCI Build Status](https://circleci.com/gh/wrengr/bytestring-trie.svg?style=shield&circle-token=b57517657c556be6fd8fca92b843f9e4cffaf8d1)](https://circleci.com/gh/wrengr/bytestring-trie)

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
be able to use one of the following standard methods to install it.

    -- With cabal-install and without the source:
    $> cabal install bytestring-trie
    
    -- With cabal-install and with the source already:
    $> cd bytestring-trie
    $> cabal install
    
    -- Without cabal-install, but with the source already:
    $> cd bytestring-trie
    $> runhaskell Setup.hs configure --user
    $> runhaskell Setup.hs build
    $> runhaskell Setup.hs haddock --hyperlink-source
    $> runhaskell Setup.hs copy
    $> runhaskell Setup.hs register

The Haddock step is optional.


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
