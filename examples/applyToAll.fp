{- applyToAll (α) applies a function `f` to all elements in
   given a sequence. This functions is known as a `map` in
   many programming languages.

   rules:

   αf:x ≡ x = ⌽ -> ⌽;
          x = <x1, ..., xn> -> <f:x1, ..., f:xn>

   Examples:

   α+:<<1,2>, <3,4>> -- <3, 7>
   α+:⌽              -- ⌽
-}

α+:<<1,2>, <3,4>>
