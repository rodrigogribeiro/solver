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

Free type variables
          
> fv :: Ty -> [Name]
> fv (TyCon _) = []
> fv (TyVar n) = [n]
> fv (FunTy t ts) = foldr (union . fv) [] ( t: ts)
> fv (Struct fs _) = foldr (union . fv . ty) [] fs
> fv (Pointer t) = fv t
          
Name of a type
          
> nameOf :: Ty -> Name
> nameOf (TyCon n) = n
> nameOf (TyVar n) = n
> nameOf (Struct _ n) = n
> nameOf _ = error "This is impossible! Data.Type.nameOf\n line 35"

