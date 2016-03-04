> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE TupleSections #-}
          
> module Solver.ConstraintSolver where

> import Data.Map (Map)
> import qualified Data.Map as Map
> import Data.Maybe (isJust, fromJust)
  
> import Data.Constraints
> import Data.Type
> import Data.BuiltIn    

> import Control.Monad  
> import Control.Monad.Trans    
> import Control.Monad.State
> import Control.Monad.Except    -- malloc + e-mail.

> import Solver.SolverMonad
> import Solver.Unification    
> import Solver.ConversionRules  
> import Utils.Pretty


> solver :: Constraint -> IO (Either String (TyCtx, VarCtx))
> solver c = runSolverM (solve c) 1 --((length $ fv c) + 1)
  
> solve :: Constraint -> SolverM (TyCtx, VarCtx)
> solve c = do
>             -- step 1: expand type defs.
>             (tc0,c') <- stage1 (TyCtx initialTyCtx) c
>             -- step 2: expand variable types and create missing variables
>             (vcx,c'') <- stage2 (VarCtx initialVarCtx) c'
>             -- step 3: split constraints into equality and field acess. unify equalities
>             let (eqs, fds) = stage3 c''
>                 unifyList [] = return nullSubst
>                 unifyList ((t :=: t') : ts)
>                           = do
>                              --liftIO (print $ pprint (t :=: t'))
>                              s <- unify t t'     
>                              s' <- unifyList (apply s ts)
>                              return (s' @@ s)
>             --liftIO (mapM_ (print . pprint) eqs)                                          
>             s <- unifyList eqs
>             -- step 4: build record structures and remove built-ins
>             tcx1 <- stage4 tc0 fds s
>             let
>                 --isVar = (== "alpha") . take 5 . show . pprint
>                 tcx' = TyCtx $ (tyctx tcx1) Map.\\ initialTyCtx
>                 vcx' = VarCtx $ Map.filter (not . snd) $ (varctx vcx) Map.\\ initialVarCtx
>             --liftIO (print $ pprint s)
>             --liftIO (mapM_ (print . pprint) (apply s fds))           
>             --liftIO (print $ pprint tcx1)           
>             return (tcx', vcx')               

> stage1 :: TyCtx -> Constraint -> SolverM (TyCtx, Constraint)
> stage1 tctx (t :=: t')
>        = do
>            return (tctx, (replaceTy tctx t) :=:
>                          (replaceTy tctx t'))
> stage1 tctx (n :<-: t)
>        = return (tctx, n :<-: (replaceTy tctx t))
> stage1 tctx (Has n (Field n' t))
>        = return (tctx, Has n (Field n' (replaceTy tctx t)))
> stage1 tctx (TypeDef t t')
>        = defineTypeDef t t' tctx
> stage1 tctx (c :&: c')
>        = do
>            (tcx1, c1) <- stage1 tctx c
>            (tcx2, c1') <- stage1 tcx1 c'
>            return (tcx2, c1 :&: c1')
> stage1 tctx (Exists n c)
>        = do
>            v <- fresh
>            stage1 tctx (apply (n +-> v) c)
> stage1 tctx (Def n t c)
>        = do
>           (tcx, c')  <- stage1 tctx c
>           return (tcx, Def n (replaceTy tctx t) c')
> stage1 tctx Truth
>        = return (tctx, Truth)       
              

> defineTypeDef :: Ty -> Ty -> TyCtx -> SolverM (TyCtx, Constraint)
> defineTypeDef (Pointer t) (Pointer t') tctx
>     = do
>         (tcx,c) <- defineTypeDef t t' tctx
>         return (tcx, (t :=: t') :&: c)
> defineTypeDef t@(Pointer l) t'@(TyVar v) tctx
>     = do
>          v' <- fresh
>          --liftIO (print $ pprint t <+> pprint t')                
>          (tcx, c) <- defineTypeDef l v' tctx
>          return (tcx, (t' :=: (Pointer v')) :&: c)             
> defineTypeDef t t' tctx
>     = do
>          let
>              tctx' = TyCtx $ maybe (Map.insert (nameOf t) t' (tyctx tctx))
>                                    (const (tyctx tctx))
>                                    (Map.lookup (nameOf t) (tyctx tctx))
>          return (tctx' , Truth)   

> replaceTy :: TyCtx -> Ty -> Ty
> replaceTy tctx t@(TyCon n)
>     = maybe t id (Map.lookup n (tyctx tctx))
> replaceTy tctx t@(TyVar _)
>     = t
> replaceTy tctx (FunTy t ts)
>     = FunTy (replaceTy tctx t)
>             (map (replaceTy tctx) ts)
> replaceTy tctx (Pointer t)
>     = Pointer (replaceTy tctx t)
> replaceTy tctx (Struct fs n)
>     = Struct (map (\f -> f{ty = replaceTy tctx (ty f)}) fs)
>              n


> stage2 :: VarCtx -> Constraint -> SolverM (VarCtx, Constraint)
> stage2 vtx (n :<-: t)
>     = case Map.lookup n (varctx vtx) of
>         Just (t',_) -> return (vtx, t :=: t')
>         Nothing ->
>             do
>               v <- fresh
>               return (VarCtx $ Map.insert n (v,False) (varctx vtx)
>                      , Truth)  
> stage2 vtx (Def n t c)
>     = stage2 (VarCtx $ Map.insert n (t, True) (varctx vtx)) c
> stage2 vtx (c :&: c')
>     = do
>         (vtx1, c1) <- stage2 vtx c
>         (vtx2, c2) <- stage2 vtx1 c'
>         return (VarCtx $ (varctx vtx1) `Map.union` (varctx vtx2)
>                , c1 :&: c2)
> stage2 vtx c@(Has _ _)
>     = return (vtx, c)
> stage2 vtx c@(_ :=: _)
>     = return (vtx, c)
> stage2 vtx Truth
>     = return (vtx, Truth)
                          

> stage3 :: Constraint -> ([Constraint], [Constraint])
> stage3 (c :&: c') = (eq ++ eq', fs ++ fs')
>                     where
>                        (eq,fs) = stage3 c
>                        (eq',fs') = stage3 c'
> stage3 c@(Has _ _) = ([], [c])
> stage3 c@(_ :=: _) = ([c],[])
> stage3 Truth = ([],[])


> stage4 :: TyCtx -> [Constraint] -> Subst -> SolverM TyCtx
> stage4 tcx fs s
>     = do
>         let
>           -- build a map of fields using as key a type name  
>           fieldMap = foldr go Map.empty (apply s fs)
>           go (Has n t) ac = maybe (Map.insert (nameOf n) [apply s t] ac)
>                                   (\ts -> Map.insert (nameOf n) ((apply s t):ts) ac)
>                                   (Map.lookup (nameOf n) ac)
>           s' = Subst $ Map.mapWithKey (\v fs -> Struct fs v) fieldMap                         
>           -- apply substitution to types                        
>           tcx' = TyCtx $ Map.map (apply (s' @@ s)) (tyctx tcx)
>           -- combine structs with types
>           replace n (Struct fs v) = apply (v +-> (TyCon n)) $ Struct fs n
>           replace n (Pointer t) = Pointer (replace n t)                          
>           replace n t = t
>           isVar = (== "alpha") . take 5 . show . pprint                    
>           tcx'' = TyCtx $ Map.filter (not . isVar) $ Map.mapWithKey replace (tyctx tcx')
>         --liftIO (print fieldMap)
>         --liftIO (print $ pprint tcx')
>         --liftIO (print $ pprint tcx'')                
>         return tcx''
           
-- >           isVar = (== "alpha") . take 5 . show . pprint      
-- >           s' = Subst $ Map.foldrWithKey (\k v ac -> if isVar v then
-- >                                                         Map.insert (nameOf v) (TyCon k) ac
-- >                                                      else ac) Map.empty (tyctx tcx)
-- >           -- build record definitions using field map     
-- >           tctx'' = TyCtx $ Map.map (apply s) $ Map.foldrWithKey step (tyctx tcx)
-- >                                                                      (tyctx tcx)
-- >           step n t ac = maybe ac
-- >                               (\fs -> Map.insert n (Struct fs n) ac)
-- >                               (Map.lookup t fieldMap)
-- >         return (TyCtx $ Map.filterWithKey (\k v -> (TyCon k) /= v) $ Map.map (apply s') (tyctx tctx''))                       
           
                  
-- >     do
-- >             --liftIO (print $ length (fv c))
-- >             (tc0,c') <- stage0 (TyCtx initialTyCtx) c
-- >             --liftIO (print $ pprint c')                    
-- >             (tcx, vcx, cs, s) <- stage1 tc0
-- >                                         (VarCtx initialVarCtx)
-- >                                         c'
-- >             (tcx', vcx') <- stage2 tcx vcx cs s
-- >             liftIO (print $ pprint $ tcx')
-- >             --liftIO (print $ pprint s)
-- >             --liftIO (mapM_ print $ map pprint cs)
-- >             --liftIO (print $ pprint vcx')       
-- >             let (a,b) = removeBuiltIn tcx' vcx'
-- >                 (s',a') = fixRecursiveTypes tcx a       
-- >             --liftIO (print $ pprint s')
-- >             --liftIO (print $ pprint a)                                
-- >             return (a',b)            


-- > fixRecursiveTypes :: TyCtx -> TyCtx -> (Subst, TyCtx)
-- > fixRecursiveTypes otcx ntcx
-- >     = (s, TyCtx $ Map.map (apply s) ctx)
-- >       where
-- >          ctx = (tyctx ntcx) Map.\\ initialTyCtx
-- >          s = Subst $ Map.foldrWithKey step Map.empty ctx 
-- >          step k v ac = if not $ null $ fv v then
-- >                            foldr (\x ac -> Map.insert x (TyCon k) ac) Map.empty (fv v)
-- >                          else ac     
              
-- > removeBuiltIn :: TyCtx -> VarCtx -> (TyCtx, VarCtx)
-- > removeBuiltIn tcx vcx = (TyCtx t', VarCtx v')
-- >                         where
-- >                           t' =  Map.filter (not . isVar) ((tyctx tcx) Map.\\ initialTyCtx)
-- >                           v' = Map.filter (not . snd) ((varctx vcx) Map.\\ initialVarCtx)
-- >                           isVar = ((==) "alpha") . (take 5) . show . pprint      


-- > defineTypeDef :: Ty -> Ty -> TyCtx -> SolverM (TyCtx, Constraint)
-- > defineTypeDef (Pointer t) (Pointer t') tctx
-- >     = do
-- >         (tcx,c) <- defineTypeDef t t' tctx
-- >         return (tcx, (t :=: t') :&: c)
-- > defineTypeDef t@(Pointer l) t'@(TyVar v) tctx
-- >     = do
-- >          v' <- fresh
-- >          --liftIO (print $ pprint t <+> pprint t')                
-- >          (tcx, c) <- defineTypeDef l v' tctx
-- >          return (tcx, (t' :=: (Pointer v')) :&: c)             
-- > defineTypeDef t t' tctx
-- >     = do
-- >          let
-- >              tctx' = TyCtx $ maybe (Map.insert (nameOf t) t' (tyctx tctx))
-- >                                    (const (tyctx tctx))
-- >                                    (Map.lookup (nameOf t) (tyctx tctx))
-- >          return (tctx' , Truth)   

-- > stage0 :: TyCtx -> Constraint -> SolverM (TyCtx, Constraint)
-- > stage0 tctx (t :=: t')
-- >        = do
-- >            return (tctx, (replaceTy tctx t) :=:
-- >                          (replaceTy tctx t'))
-- > stage0 tctx (n :<-: t)
-- >        = return (tctx, n :<-: (replaceTy tctx t))
-- > stage0 tctx (Has n (Field n' t))
-- >        = return (tctx, Has n (Field n' (replaceTy tctx t)))
-- > stage0 tctx (TypeDef t t')
-- >        = defineTypeDef t t' tctx
-- > stage0 tctx (c :&: c')
-- >        = do
-- >            (tcx1, c1) <- stage0 tctx c
-- >            (tcx2, c1') <- stage0 tcx1 c'
-- >            return (tcx2, c1 :&: c1')
-- > stage0 tctx (Exists n c)
-- >        = do
-- >            v <- fresh
-- >            stage0 tctx (apply (n +-> v) c)
-- > stage0 tctx (Def n t c)
-- >        = do
-- >           (tcx, c')  <- stage0 tctx c
-- >           return (tcx, Def n (replaceTy tctx t) c')
-- > stage0 tctx Truth
-- >        = return (tctx, Truth)       

-- > replaceTy :: TyCtx -> Ty -> Ty
-- > replaceTy tctx t@(TyCon n)
-- >     = maybe t id (Map.lookup n (tyctx tctx))
-- > replaceTy tctx t@(TyVar _)
-- >     = t
-- > replaceTy tctx (FunTy t ts)
-- >     = FunTy (replaceTy tctx t)
-- >             (map (replaceTy tctx) ts)
-- > replaceTy tctx (Pointer t)
-- >     = Pointer (replaceTy tctx t)
-- > replaceTy tctx (Struct fs n)
-- >     = Struct (map (\f -> f{ty = replaceTy tctx (ty f)}) fs)
-- >              n
             
-- > stage1 :: TyCtx -> VarCtx -> Constraint -> SolverM (TyCtx
-- >                                                   , VarCtx
-- >                                                   , [Constraint]
-- >                                                   , Subst)
-- > stage1 tctx vctx (t :=: t')
-- >       = do
-- >           --liftIO (print $ pprint (t :=: t'))
-- >           s <- unify t t'
-- >           --liftIO (print $ pprint ((apply s t) :=: (apply s t')))     
-- >           return (tctx, vctx, [], s)
-- > stage1 tctx vctx (n :<-: t)
-- >       = case Map.lookup n (varctx vctx) of
-- >             Just (t',_) ->
-- >                 do
-- >                   --liftIO (print $ pprint (t :=: t'))
-- >                   s <- unify t t'
-- >                   --liftIO (print $ pprint ((apply s t) :=: (apply s t')))          
-- >                   return (tctx, vctx,[], s)
-- >             Nothing -> return (tctx
-- >                               , VarCtx $ Map.insert n (t, False) (varctx vctx)
-- >                               , []
-- >                               , nullSubst)
-- > stage1 tctx vctx c@(Has _ _)
-- >       = return (tctx, vctx, [c], nullSubst)
-- > stage1 tctx vctx (TypeDef t t')
-- >       = undefined
-- > stage1 tctx vctx (c :&: c')
-- >       = do
-- >           --liftIO (print $ pprint c)
-- >           (t1,v1,c1,s1) <- stage1 tctx vctx c
-- >           --liftIO (print $ pprint c')                 
-- >           (t2,v2,c2,s2) <- stage1 t1 v1 (apply s1 c')                  
-- >           return ( TyCtx (Map.union (tyctx t1) (tyctx t2))
-- >                  , VarCtx (Map.union (varctx v1) (varctx v2))
-- >                  , apply (s2 @@ s1) (c1 ++ c2)
-- >                  , s2 @@ s1)  
-- > stage1 tctx vctx (Exists n c)
-- >       = undefined --stage1 tctx vctx c
-- > stage1 tctx vctx (Def n t c)
-- >       = stage1 tctx (VarCtx $ Map.insert n (t,True) (varctx vctx)) c
-- > stage1 tctx vctx Truth
-- >       = return (tctx, vctx, [], nullSubst)


-- > stage2 :: TyCtx -> VarCtx -> [Constraint] -> Subst -> SolverM (TyCtx
-- >                                                               , VarCtx)
-- > stage2 tctx vctx cs s
-- >      = do
-- >          let tctx' = TyCtx $ Map.map (apply s) (tyctx tctx)
-- >              vctx' = VarCtx $ Map.map (\(a,b) -> (apply s a, b)) (varctx vctx)
-- >              cs' = apply s cs
-- >              fieldMap = foldr go Map.empty cs'
-- >              go (Has n t) ac = maybe (Map.insert n [t] ac)
-- >                                      (\ts -> Map.insert n (t:ts) ac)
-- >                                      (Map.lookup n ac)
-- >              tctx'' = TyCtx $ Map.foldrWithKey step (tyctx tctx') (tyctx tctx')
-- >              step n t ac = maybe ac
-- >                                  (\fs -> Map.insert n (Struct fs n) ac)
-- >                                  (Map.lookup t fieldMap)
-- >          --liftIO (mapM_ (print . pprint) (apply s cs))
-- >          liftIO (print $ pprint s)
-- >          liftIO (print fieldMap)                        
-- >          --liftIO (print $ pprint tctx'')       
-- >          return (tctx'', vctx')
