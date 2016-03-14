Built-in types, operators and contexts

> module Data.BuiltIn where

> import Data.Type
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Utils.Pretty (nameOf)
     

Initial typing definition     

> initialTyCtx :: Map Name Ty
> initialTyCtx
>     = Map.fromList (map (\t -> (nameOf t, t)) types)
>       where
>         types = [ bool, void, char, short, int, long
>                 , float, double, longDouble
>                 , signedChar, signedInt
>                 , signedShortInt, signedLongInt
>                 , unsignedChar, unsignedInt
>                 , unsignedShortInt, unsignedLongInt
>                 , unsignedLong ]


> initialVarCtx :: Map Name (Ty,Bool)
> initialVarCtx
>     = Map.fromList types
>       where
>          types = [ (Name "+", (FunTy int [int, int], True))
>                  , (Name "-", (FunTy int [int, int], True))
>                  , (Name "*", (FunTy int [int, int], True))
>                  , (Name "/", (FunTy double [double, double], True))
>                  , (Name "%", (FunTy int [int, int], True))
>                  , (Name "<", (FunTy bool [short, short], True))
>                  , (Name ">", (FunTy bool [short, short], True))
>                  , (Name "<=", (FunTy bool [short, short], True))
>                  , (Name ">=", (FunTy bool [short, short], True))
>                  , (Name "==", (FunTy bool [short, short], True))
>                  , (Name "!=", (FunTy bool [short, short], True))
>                  , (Name "&", (FunTy short [short, short], True))
>                  , (Name "|", (FunTy short [short, short], True))
>                  , (Name "^", (FunTy short [short, short], True))
>                  , (Name "||", (FunTy bool [bool, bool], True))
>                  , (Name "&&", (FunTy bool [bool, bool], True))
>                  , (Name "size_of", (FunTy int [TyVar (Name "alpha0")], True))
>                  , (Name "malloc", (FunTy (TyVar (Name "alpha0")) [int], True))]
>           
                    
basic types

> bool :: Ty
> bool = TyCon (Name "bool")
      
> void :: Ty
> void = TyCon (Name "void")        

> char :: Ty
> char = TyCon (Name "char")

> short :: Ty
> short = TyCon (Name "short")

> int :: Ty
> int = TyCon (Name "int")

> long :: Ty
> long = TyCon (Name "long")

> float :: Ty
> float = TyCon (Name "float")
  
> double :: Ty
> double = TyCon (Name "double")

> longDouble :: Ty
> longDouble = TyCon (Name "long double")
  
signed types
                     
> signedChar :: Ty
> signedChar = TyCon (Name "signed char")

> signedInt :: Ty
> signedInt = TyCon (Name "signed int")             

> signedShortInt :: Ty
> signedShortInt = TyCon (Name "signed short int")                  

> signedLongInt :: Ty
> signedLongInt = TyCon (Name "signed long int")

unsigned types 
  
> unsignedChar :: Ty
> unsignedChar = TyCon (Name "unsigned char")

> unsignedInt :: Ty
> unsignedInt = TyCon (Name "unsigned int")             

> unsignedShortInt :: Ty
> unsignedShortInt = TyCon (Name "unsigned short int")                  

> unsignedLongInt :: Ty
> unsignedLongInt = TyCon (Name "unsigned long int")
                   
> unsignedLong :: Ty
> unsignedLong = TyCon (Name "unsigned long")                
