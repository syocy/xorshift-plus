# xorshift-plus

Simple implementation of xorshift+.

```haskell
import Random.XorshiftPlus
main = do
  s <- genXorshiftPlusInt 1
  i <- getInt s
  print i -- 2455688531189531812
```

## Performance

```
> cabal new-run --enable-benchmarks micro -- compareWithOtherPRNGs --small
Up to date
compareWithOtherPRNGs/xorshift-plus_Int (THIS PACKAGE) mean 10.63 μs  ( +- 40.63 ns  )
compareWithOtherPRNGs/xorshift-plus_Word (THIS PACKAGE) mean 10.72 μs  ( +- 36.54 ns  )
compareWithOtherPRNGs/xorshift_Int32     mean 250.5 μs  ( +- 917.1 ns  )
compareWithOtherPRNGs/xorshift_Ina64     mean 457.1 μs  ( +- 1.070 μs  )
compareWithOtherPRNGs/Xorshift128Plus_Word64 mean 24.61 μs  ( +- 111.9 ns  )
```
