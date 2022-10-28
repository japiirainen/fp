{-  computes the nth element of a given sequence.
    It is bottom preserving.

   rules:

   ~n:x ≡ n > length:x -> ⊥;
          <x1, ..., xn> -> xn;
          ⊥;

   0~:x ≡ x = <x0, ..., xn> -> xn-1;
          ⊥;

   Examples:

   ~2:<1,2,3> -- 3
   ~5:<1,2,3> -- ⊥

   0~:<1,2,3> -- 3
   1~:<1,2,3> -- 2
-}

~2:<1,2,3>

~5:<1,2,3>

 0~:<1,2,3>

 1~:<1,2,3>
