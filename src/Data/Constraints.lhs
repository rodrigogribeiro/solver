Definition of constraints
======================

> {-# LANGUAGE DeriveDataTypeable #-}        

> module Data.Constraints where

> import Data.Type
> import Data.Generics
  
> data Constraint = Ty :=: Ty                 -- equality   
>                 | Name :<-: Ty              -- type ascription
>                 | Has Ty Field              -- field constraint
>                 | TypeDef Name Ty           -- type definition
>                 | Def Name Ty Constraint    -- symbol definition
>                 | Constraint :&: Constraint -- conjunction
>                 | Exists Name Constraint    -- fresh variable introduction
>                 | Truth  
>                 deriving (Eq, Ord, Show, Data, Typeable)  


> vars :: Constraint -> [Name]
> vars (Exists n c) = n : vars c
> vars (Def _ _ c) = vars c
> vars (c :&: c') = vars c ++ vars c'
> vars _ = []                  
