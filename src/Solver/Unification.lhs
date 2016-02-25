Unification algorithm
=====================

> module Solver.Unification where

> import Control.Monad.Except
> import Control.Monad.State

> import Data.List (union, sort)    
> import Data.Map (Map)
> import qualified Data.Map as Map
  
> import Data.Type
> import Data.Constraints    
> import Solver.SolverMonad
> import Solver.ConversionRules    
> import Utils.Pretty

A type class for the unification algorithm

> class Apply a where
>     apply :: Subst -> a -> a

> instance Apply a => Apply [a] where
>     apply s = map (apply s)

> (@@) :: Subst -> Subst -> Subst
> s1 @@ s2 = Subst $ (Map.map (apply s1) (subs s2))
>                    `Map.union`
>                    (subs s1)                

> class Apply a => Unifiable a where
>     fv    :: a -> [Name]
>     unify :: a -> a -> SolverM Subst

> instance Unifiable a => Unifiable [a] where
>     fv      = foldr (union . fv) []
>     unify [] [] = return nullSubst
>     unify _ [] = wrongArgumentNumberError
>     unify [] _ = wrongArgumentNumberError
>     unify (t:ts) (t':ts')
>         = do
>             s <- unify t t'
>             s' <- unify (apply s ts)
>                         (apply s ts')
>             return (s' @@ s)


> varBind :: Name -> Ty -> SolverM Subst
> varBind n t
>        | n `elem` fv t = occursCheckError n t
>        | otherwise = return $ Subst (Map.singleton n t)

> instance Apply Ty where
>     apply s t@(TyVar v) = maybe t id (Map.lookup v (subs s))
>     apply s t@(TyCon c) = t
>     apply s (Pointer t) = Pointer (apply s t)
>     apply s (FunTy t ts) = FunTy (apply s t) (apply s ts)
>     apply s (Struct fs n) = Struct (apply s fs) n


> instance Unifiable Ty where
>     fv (TyVar v) = [v]
>     fv (TyCon _) = []
>     fv (Pointer t) = fv t
>     fv (FunTy t ts) = fv t `union` fv ts
>     fv (Struct fs _) = fv fs
>
>     unify (TyVar v) t = varBind v t
>     unify t (TyVar v) = varBind v t
>     unify (TyCon c) (TyCon c')
>           | convertible(TyCon c) (TyCon c') = return nullSubst
>           | otherwise = differentTypeConstructorsError c c'
>     unify (Pointer t) (Pointer t')
>         = unify t t'
>     unify (FunTy t ts) (FunTy t' ts')
>         = do
>            s <- unify t t'
>            s' <- unify (apply s ts) (apply s ts')
>            return (s' @@ s)      
>     unify (Struct fs n) (Struct fs' n')
>         | n == n' = unify (sort fs) (sort fs')
>         | otherwise = differentTypeConstructorsError n n'            

> instance Apply Field where
>     apply s (Field n t) = Field n (apply s t)

> instance Unifiable Field where
>     fv = fv . ty
>     unify (Field n t) (Field n' t')
>           | n == n' = unify t t'
>           | otherwise = unifyDifferentFields n n'            


> instance Apply Constraint where
>     apply s (t :=: t') = (apply s t) :=: (apply s t')
>     apply s (n :<-: t) = n :<-: (apply s t)
>     apply s (Has t f) = Has (apply s t) (apply s f)
>     apply s (Def n t c) = Def n (apply s t) (apply s c)
>     apply s (c :&: c') = (apply s c) :&: (apply s c')
>     apply s (Exists n c) = Exists n (apply s c)
>     apply s (TypeDef t t') = TypeDef (apply s t) (apply s t')                       
>     apply s Truth = Truth                       

> wrongArgumentNumberError :: SolverM a
> wrongArgumentNumberError = throwError "Error! Wrong argument number."                            

> occursCheckError :: Name -> Ty -> SolverM a
> occursCheckError n t = throwError $ show $
>                           text "Cannot unify:\n" <>
>                           pprint n <+> text "with\n" <>
>                           pprint t  <> text "\noccurs check"     

> differentTypeConstructorsError :: Name -> Name -> SolverM a
> differentTypeConstructorsError v v'
>     = throwError $ show $
>          text "Cannot unify types with different constructors:\n" <>
>          pprint v <+> text "and" <+> pprint v'
                            
> unifyDifferentFields :: Name -> Name -> SolverM a
> unifyDifferentFields n n' = throwError $ show $
>                                 text "Cannot unify different fields:\n" <+>
>                                 pprint n <+> text "\nwith\n" <+> pprint n'
