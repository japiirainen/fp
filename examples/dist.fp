{- `distl` and `distr`

   rules:

   distl:x ≡ x = <y,⌽> -> ⌽;
             x = <y,<Z1 ..... Zn>> -> <<y, z1> .... , <y, Zn>>; ⊥"

   distr:x ≡ x = <⌽,y> -> ⌽;
             x = <<y1 ..... yn>, z> -> <<y1, z> .... , <yn, z>>; ⊥"


   Examples:

   distl:<1, <2, 2>> -- <<1, 2>, <1, 2>>
   distr:<<2, 2>, 1> -- <<2, 1>, <2, 1>>
-}

distl:<1, <2, 2>>
