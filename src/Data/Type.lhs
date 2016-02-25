Definition of C type syntax
===========================

> {-# LANGUAGE DeriveDataTypeable #-}        

> module Data.Type where

> import Data.List (union)  
> import Data.Generics
  
Definition of names  

> newtype Name = Name { unName :: String }
>                deriving (Eq, Ord, Show, Data, Typeable)

Definition of fields
                 
> data Field = Field { name :: Name
>                    , ty   :: Ty   }
>              deriving (Eq, Ord, Show, Data, Typeable)

Definition of types

> data Ty = TyCon Name
>         | TyVar Name
>         | FunTy Ty [Ty]
>         | Struct [Field] Name
>         | Pointer Ty
>         deriving (Eq, Ord, Show, Data, Typeable)

          

