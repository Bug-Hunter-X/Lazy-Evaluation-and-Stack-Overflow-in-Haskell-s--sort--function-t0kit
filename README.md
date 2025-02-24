# Haskell Lazy Evaluation Bug

This repository demonstrates a subtle bug in Haskell code that arises from the language's lazy evaluation.  The bug involves the `Data.List.sort` function and its potential for stack overflow when dealing with very large lists.  The issue isn't immediately apparent and showcases the need for careful consideration of how lazy evaluation impacts performance and memory usage.

## Bug Description

The provided Haskell code uses the `sort` function from `Data.List` to sort a list. While seemingly innocuous, this code can cause a stack overflow if the input list is very large or deeply nested in a recursive structure. This is because `sort` requires traversing the entire list, even though the output is only displayed.  The lazy nature of Haskell doesn't prevent this full traversal and potential for exceeding stack limits.

## Solution

The solution involves using a more efficient sorting algorithm or more carefully managing the list processing to avoid excessive recursion and resource consumption. The provided solution demonstrates one such approach.