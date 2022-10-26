{- rules:

   eq:x ≡ x=<y,z> & y = z -> T; x=<y,z> &y != z -> F; ⊥

   Examples:

   eq:<A, A> -- T
   eq:<1, 1> -- T
   eq:<<1,2>,<1,2>> -- T
   eq:<A, B> -- F
   eq:<<1,2>,<2,1>> -- F
-}

eq:<1, 1>
