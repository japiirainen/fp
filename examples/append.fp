{- `apndl` and `apndr`

   rules:

   apndl:x ≡ x = <y,⌽> -> <y>;
             x = <y, <z1, ..., zn>> -> <y, z1, ..., zn>;
             ⊥;

   apndr:x ≡ x = <⌽,y> -> <y>;
             x = <<z1, ..., zn>, y> -> <z1, ..., zn, y>;
             ⊥;

   Examples:

   apndl:<6, ⌽>       -- <6>
   apndl:<6, <1,2,3>> -- <6,1,2,3>

   apndr:<⌽, 6>       -- <6>
   apndr:<<1,2,3>, 6> -- <1,2,3,6>
-}

apndl:<6, ⌽>
apndl:<6, <1,2,3>>

apndr:<⌽, 6>
apndr:<<1,2,3>, 6>

