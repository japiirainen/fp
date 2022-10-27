{- Implementation of 'matrix multiplication'.
-}

Def ip ≡ /+∘α*∘⍉

Def mm ≡ α(α ip)∘(α distl)∘distr∘[~0, ⍉∘~1]

mm:< < <1,2>, <4,5> >,
     < <6,8>, <7,9>> >
