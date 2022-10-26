{- rules:

   reverse:x ≡ x = ⌽ -> ⌽;
               x = <x1, ..., xn> -> <xn, ..., x1>
               ⊥

   Examples:

   reverse:⌽     -- ⌽
   reverse:<1,2> -- <2,1>
-}

-- for about `α` see `examples/applyToAll`.

α reverse:<<1,2>,<3,4>>
