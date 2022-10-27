{-  rules:

   (p → f; g):x ≡ (p:x) = T → f:x;
                  (p:x) = F → g:x;
                  ⊥;

   Examples:

   (eq → +; id):<2,2> -- 4
   -- since
   -- (eq:<2,2> = T)
   -- -> +:<2,2>
-}

(eq → +; id):<2,2>

eq → +; id:<1,2>

