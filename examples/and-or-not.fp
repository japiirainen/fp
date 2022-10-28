{- `and`, `or` and `not` boolean algebra

   rules:

   Def ip ≡ /+∘α*∘⍉

   ∧:x ≡ x = <T,T> -> T;
           x = <T,F> or <F,T> -> F;
           ⊥

   ∨:x ≡ x = <T,F> or <F,T> or <T,T> -> T;
          x = <F,F> -> F;
          ⊥

   ¬:x ≡ x = T -> F;
           x = F -> T;
           ⊥

-}

∧:<T,T>
∧:<F,T>

∨:<T,F>
∨:<F,T>
∨:<T,T>
∨:<F,F>

¬:T
¬:F
