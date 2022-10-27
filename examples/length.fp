{- `length` computes the lenght of a given sequence.
    It is bottom preserving.

   rules:

   distl:x ≡ x = ⌽ -> 0;
             x = <x1, ..., xn> -> n;
             ⊥;

   Examples:

   length:<1,2,3> -- 3
   length:⌽       -- 0
   length:⊥       -- ⊥
-}

length:<1,2,3>

length:⌽

length:⊥
