# xorshift-plus

Simple implementation of xorshift+.

```haskell
import Random.XorshiftPlus
main = do
  s <- genXorshiftPlusInt 1
  i <- getInt s
  print i                   -- 131076
```
