{- transpose (⍉) swaps the given input matrices
   rows and columns.

   rules:

   trans:x ≡ x≡<<> ..... <>> -> <>;
      X≡<Xi, ... , Xn> "-> <yl, ... , yrn>; j"

   where
     xi≡<xil, ..., xim>
     yi≡<xil, ..., xnj>, 1 <= i <= n,
                         1 <= j <= m.

   note:

   xs ≡ ⍉∘⍉:xs

-}

⍉:< < 1, 2, 3 >, < 4, 5, 6 >, < 7, 8, 9 > >

-- this will yield
-- < < 1, 4, 7 >, < 2, 5, 8 >, < 3, 6, 9 > >
