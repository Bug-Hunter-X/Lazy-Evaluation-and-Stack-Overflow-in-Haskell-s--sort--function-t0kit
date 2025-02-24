```haskell
import Data.List (sort)
import qualified Data.Vector.Unboxed as UV

main :: IO ()
main = do
  let xs = [1, 2, 3, 4, 5]
  let ys = sort xs
  print ys

-- Solution using Data.Vector.Unboxed for improved performance with large lists
main' :: IO ()
main' = do
  let xs = UV.fromList [5, 2, 8, 1, 9, 4, 7, 3, 6]
  let ys = UV.toList $ UV.sort xs --Using Unboxed Vector for better efficiency
  print ys
```
The solution uses `Data.Vector.Unboxed` to perform the sorting.  Unboxed vectors provide significantly better performance compared to lists when dealing with large collections of primitive data types.  The use of `Data.Vector.Unboxed` and its efficient sorting algorithm avoids the stack overflow issue associated with using `Data.List.sort` on large lists.