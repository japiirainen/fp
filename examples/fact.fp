{- Here we calculate factorials. Yay!
-}

Def eq0 ≡ eq ∘ [ id, _0 ]
Def sub1 ≡ - ∘ [ id, _1 ]
Def hd ≡ (atom → id; ~0)
Def iota ≡ flatten ∘ while (not ∘ eq0 ∘ ~0) [sub1 ∘ hd, id]

Def fact ≡ /* ∘ tl ∘ iota

fact:10

