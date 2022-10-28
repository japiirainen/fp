{- `while`

   rules:

   (while p f):x ≡ p:x = T -> (while p f):(f:x);
               p:x = F -> x;
               ⊥;
-}

Def eq0 ≡ eq ∘ [ id, ~0 ]

Def iota ≡ _2 ∘ while (¬ ∘ eq0 ∘ _1) [ sub1 ∘ _1, apndl ] ∘ [ id, ~<> ]

iota:10

