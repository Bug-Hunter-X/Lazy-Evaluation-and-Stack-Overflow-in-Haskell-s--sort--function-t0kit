```haskell
import Data.List (sort)

main :: IO ()
main = do
  let xs = [1, 2, 3, 4, 5]
  let ys = sort xs
  print ys
```
This code looks harmless, but it hides a subtle issue related to Haskell's laziness.  If `xs` were a very large list, `sort xs` might take a long time to compute, and in some cases, it might even cause a stack overflow if the list is deeply recursive.  The problem is that `sort` needs to traverse the entire list to perform its sorting, so even though `print ys` only prints the result, the entire sorting operation is still performed.

The fact that `sort` is a pure function doesn't necessarily prevent this;  it simply means the result is deterministic, not that the computation is efficient or memory-friendly for all inputs. 