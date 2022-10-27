{-  computes the nth element of a given sequence.
    It is bottom preserving.

   rules:

   `n`n:x ≡ n > length:x ⌽ -> ⊥;
            <x1, ..., xn> -> xn;
            ⊥;

   Examples:

   2n:<1,2,3> -- 3
   5n:<1,2,3> -- ⊥
-}

2n:<1,2,3>

5n:<1,2,3>

