{- `while`

   rules:

   (while p f):x ≡ p:x = T -> (while p f):(f:x);
               p:x = F -> x;
               ⊥;
-}

Def eq0 ≡ eq ∘ [ id, _0 ]
Def sub1 ≡ - ∘ [ id, _1 ]

-- Def iiota = 2s o while (not o eq0 o 1s) [ sub1 o 1s, apndl ] o [ id, ~<> ]

Def iota ≡ _1 ∘ while (¬ ∘ eq0 ∘ _0) [ sub1 ∘ _0, apndl ] ∘ [ id, ~<> ]

iota:10

