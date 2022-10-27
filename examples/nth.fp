{-  computes the nth element of a given sequence.
    It is bottom preserving.

   rules:

   ~n:x ≡ n > length:x ⌽ -> ⊥;
          <x1, ..., xn> -> xn;
          ⊥;

   Examples:

   ~2:<1,2,3> -- 3
   ~5:<1,2,3> -- ⊥
-}

~2:<1,2,3>

~5:<1,2,3>

