Definition of constraints
======================

> {-# LANGUAGE DeriveDataTypeable #-}        

> module Data.Constraints where

> import Data.Type
> import Data.Generics
  
> data Constraint = Ty :=: Ty                 -- equality   
>                 | Name :<-: Ty              -- type ascription
>                 | Has Ty Field              -- field constraint
>                 | TypeDef Ty Ty             -- type definition
>                 | Def Name Ty Constraint    -- symbol definition
>                 | Constraint :&: Constraint -- conjunction
>                 | Exists Name Constraint    -- fresh variable introduction
>                 | Truth  
>                 deriving (Eq, Ord, Show, Data, Typeable)  
