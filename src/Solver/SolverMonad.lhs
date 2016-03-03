> module Solver.SolverMonad where

> import Data.Type
> import Data.BuiltIn

> import Data.Map (Map)
> import qualified Data.Map as Map
> import Data.Maybe (isJust, fromJust)
  
> import Data.Generics
> import Data.Constraints
> import Data.Type
> import Data.BuiltIn    

> import Control.Monad
> import Control.Monad.Trans    
> import Control.Monad.State
> import Control.Monad.Except    

> import Solver.ConversionRules  
> import Utils.Pretty


Definition of solver's state
============================

> type SolverM a = ExceptT String (StateT Int IO) a               

> runSolverM :: SolverM a -> Int -> IO (Either String a)
> runSolverM s v = do
>                    (e, n) <- runStateT (runExceptT s) v
>                    return e
                    
Context definitions
-------------------
        
> newtype TyCtx = TyCtx { tyctx :: Map Name Ty }
>                 deriving Eq

> instance Pretty TyCtx where
>    pprint = printer "is" . tyctx
                 
> newtype VarCtx = VarCtx { varctx :: Map Name (Ty,Bool) }
>                  deriving Eq

> instance Pretty VarCtx where
>    pprint = printer "::" . Map.map fst . varctx
          
Substitution definition
-----------------------
        
> newtype Subst = Subst { subs :: Map Name Ty }             

> nullSubst :: Subst
> nullSubst = Subst Map.empty
  
> instance Pretty Subst where
>     pprint = printer "+->" . subs

> (+->) :: Name -> Ty -> Subst
> n +-> t = Subst (Map.singleton n t)
      
Fresh variable generation
-------------------------

> fresh :: SolverM Ty
> fresh = do
>           n <- get
>           put (n + 1)
>           return (TyVar (Name ("alpha" ++ show n)))    

  
Auxiliar code
-------------         
      
> printer :: String -> Map Name Ty -> Doc  
> printer sep = hcat . punctuate comma . map (uncurry step) . Map.toList 
>               where
>                  step n t = pprint n <+> text sep <+> pprint t 
  
