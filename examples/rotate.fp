{- `rotl` and `rotr`

   rules:

   rotl:x ≡ x = ⌽ -> ⌽;
            x = <x1, ..., xn> -> <x2, ..., xn, x1>;
            ⊥;

   rotr:x ≡ x = ⌽ -> ⌽;
            x = <x1, ..., xn> -> <xn, x1, ..., xn-1>;
            ⊥;

   Examples:

   rotl:<1,2,3>       -- <2,3,1>

   rotr:<1,2,3>       -- <3,1,2>
-}

rotl:<1,2,3>

rotr:<1,2,3>

