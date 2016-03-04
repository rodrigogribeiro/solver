Specification of type conversion rules
======================================              
        
> module Solver.ConversionRules where

> import Data.Type
> import Data.BuiltIn    

> convertible :: Ty -> Ty -> Bool
> convertible t t' = or [ t == t'
>                       , and [t == short, t' == int]
>                       , and [t == int , t' == short]
>                       , and [t == int , isPointer t']
>                       , and [isPointer t, t' == int]
>                       , and [t == bool, t' == int]
>                       , and [t == int, t' == bool]
>                       , and [t == int, t' == double]
>                       , and [t == double, t' == int]
>                       , and [t == int, t' == float]
>                       , and [t == float, t' == int]
>                       , and [t == double, t' == float]
>                       , and [t == float, t' == double]  
>                       ]                              
                            
> isPointer :: Ty -> Bool
> isPointer (Pointer _) = True
> isPointer _ = False                        
