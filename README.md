text-trie
===============
[![Hackage version](https://img.shields.io/hackage/v/bytestring-trie.svg?style=flat)](https://hackage.haskell.org/package/text-trie) 
[![TravisCI Build Status](https://img.shields.io/travis/michaeljklein/bytestring-trie.svg?style=flat)](https://travis-ci.org/michaeljklein/text-trie) 

The `text-trie` package is a lightweight adaptation of `bytestring-trie` to `Text`.

For the differences in performance, see below.


## bytestring-trie

The [bytestring-trie](https://github.com/wrengr/bytestring-trie) package provides an efficient implementation
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

```bash
    -- With stack and without the source:
    $> stack install text-trie
    
    -- With stack and with the source already:
    $> cd text-trie
    $> stack install
    
```


## Performance

Example performance compared to `bytestring-trie` from benchmarks run on 04/03/2019:

`fromListR` obverse:
| String Type | Per iteration | Per second |
|-------------|---------------|------------|
| ByteString  | 43.230ns      | 23131.93   |
| Text        | 45.256ns      | 22096.29   |

`fromListL` obverse:
| String Type | Per iteration | Per second |
|-------------|---------------|------------|
| ByteString  | 182.380ns     | 5483.07    |
| Text        | 274.391ns     | 3644.43    |

`fromListR` reverse:
| String Type | Per iteration | Per second |
|-------------|---------------|------------|
| ByteString  | 279.460ns     | 3578.33    |
| Text        | 356.662ns     | 2803.78    |

`fromListL` reverse:
| String Type | Per iteration | Per second |
|-------------|---------------|------------|
| ByteString  | 43.646ns      | 22911.79   |
| Text        | 45.778ns      | 21844.78   |

`fromListR` obverse sorted:
| String Type | Per iteration | Per second |
|-------------|---------------|------------|
| ByteString  | 45.714ns      | 21875.24   |
| Text        | 49.093ns      | 20369.65   |

`fromListL` obverse sorted:
| String Type | Per iteration | Per second |
|-------------|---------------|------------|
| ByteString  | 179.145ns     | 5582.08    |
| Text        | 281.516ns     | 3552.19    |

`fromListR` reverse sorted:
| String Type | Per iteration | Per second |
|-------------|---------------|------------|
| ByteString  | 45.055ns      | 22194.92   |
| Text        | 49.776ns      | 20089.87   |

`fromListL` reverse sorted:
| String Type | Per iteration | Per second |
|-------------|---------------|------------|
| ByteString  | 176.282ns     | 5672.73    |
| Text        | 282.495ns     | 3539.89    |


## Portability

The implementation only relies on a few basic
language extensions and `DeriveGeneric`. The complete list of extensions used is:

* `CPP`
* `MagicHash`
* `NoImplicitPrelude`
* `StandaloneDeriving`
* `DeriveGeneric`


## Links

- [Hackage](http://hackage.haskell.org/package/text-trie)
- [GitHub](https://github.com/michaeljklein/text-trie)

- `bytestring-trie`
  * [Website](http://wrengr.org/)
  * [Blog](http://winterkoninkje.dreamwidth.org/)
  * [Twitter](https://twitter.com/wrengr)
  * [Hackage](http://hackage.haskell.org/package/bytestring-trie)
  * [GitHub](https://github.com/wrengr/bytestring-trie)

