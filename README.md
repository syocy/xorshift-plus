# xorshift-plus

Simple implementation of xorshift+.

```haskell
import Random.XorshiftPlus
main = do
  s <- genXorshiftPlusInt 1
  i <- getInt s
  print i -- -274877775873
```

## Performance

```
> cabal new-run --enable-benchmarks micro -- compareWithOtherPRNGs --small
Up to date
compareWithOtherPRNGs/xorshift-plus_Int (THIS PACKAGE) mean 22.28 μs  ( +- 711.5 ns  )
compareWithOtherPRNGs/xorshift-plus_Word (THIS PACKAGE) mean 21.53 μs  ( +- 561.2 ns  )
compareWithOtherPRNGs/xorshift_Int32     mean 250.3 μs  ( +- 11.14 μs  )
compareWithOtherPRNGs/xorshift_Int64     mean 455.7 μs  ( +- 14.15 μs  )
compareWithOtherPRNGs/Xorshift128Plus_Word64 mean 25.36 μs  ( +- 1.004 μs  )
```
