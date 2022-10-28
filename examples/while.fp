{- `while`

   rules:

   (while p f):x ≡ p:x = T -> (while p f):(f:x);
               p:x = F -> x;
               ⊥;
-}

Def eq0 ≡ eq ∘ [ id, _0 ]
Def sub1 ≡ - ∘ [ id, _1 ]

Def iota ≡ ~1 ∘ while (¬ ∘ eq0 ∘ ~0) [ sub1 ∘ ~0, apndl ] ∘ [ id, @<> ]

iota:10

