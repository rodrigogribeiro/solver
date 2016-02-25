Utilities for pretty printing

> module Utils.Pretty (module Utils.Pretty,
>                      module Text.PrettyPrint.HughesPJ) where


> import Text.PrettyPrint.HughesPJ                            

> import Data.Constraints  
> import Data.Type

> class Pretty a where
>     pprint :: a -> Doc


> instance Pretty Name where
>     pprint = text . unName


> instance Pretty Field where
>     pprint (Field n t) = pprint t <+> pprint n <> semi


> instance Pretty Ty where
>     pprint (TyCon n) = pprint n
>     pprint (TyVar n) = pprint n
>     pprint (FunTy ret params) = parens (hcat $ punctuate comma
>                                                   (map pprint
>                                                        (params ++ [ret])))
>     pprint (Struct fs n) = text "struct" <+> pprint n <+>
>                            braces (hcat $ (map pprint fs))
>     pprint (Pointer t) = pprint t <> char '*'                              

                                  
> instance Pretty Constraint where
>     pprint (t :=: t') = pprint t <+> char '=' <+> pprint t'
>     pprint (n :<-: t) = text "typeof" <> (parens $ pprint n) <+>
>                         char '=' <+> pprint t
>     pprint (Has t f)  = text "has" <> parens (pprint t        <> comma <>
>                                               pprint (name f) <+> colon <+>
>                                               pprint (ty f))
>     pprint (TypeDef n t) = text "typedef" <+> pprint n <+> text "as"
>                                           <+> pprint t
>     pprint (Def n t ctr) = text "def" <+> pprint n <+> char ':' <+>
>                                           pprint t <+> text "in" <+>
>                                           pprint ctr
>     pprint (Exists n ctr) = text "exists" <+> pprint n <+> char '.'
>                                           <+> pprint ctr
>     pprint (ctr :&: ctr') = pprint ctr <+> char ',' <+> pprint ctr'
>     pprint Truth = text "Truth"                        


Name of a type
          
> nameOf :: Ty -> Name
> nameOf (TyCon n) = n
> nameOf (TyVar n) = n
> nameOf (Struct _ n) = n
> nameOf (Pointer t) = Name ("*" ++ (show $ pprint t))                      
> nameOf x@(FunTy t ts) = Name (show $ pprint x)
      
