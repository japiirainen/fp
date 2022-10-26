{- rules:

   null:x ≡ x = ⌽ -> T; x != ⊥ -> F; ⊥

   Examples:

   null:⌽     -- T
   null:<>    -- T
   null:<1,2> -- F
   null:A     -- F
   null:⊥     -- ⊥
-}

null:⌽
