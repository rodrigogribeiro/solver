> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE TupleSections #-}
          
> module Solver.ConstraintSolver where

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

Simple context definition  

> type VarCtx = Map Name (Ty,Bool) -- True if is a defined thing
> type TyCtx =  Map Name Ty -- only type contructor names!

Monad definition
         
> data Info = Info {
>               varctx  :: VarCtx
>             , tyctx   :: TyCtx
>             , counter :: Int }
>             deriving (Eq, Ord)

> writeInfo :: Info -> String
> writeInfo i = defs ++ typs
>             where
>                show' :: (Pretty a) => a -> String
>                show' = show . pprint
>                defs = Map.foldrWithKey (\n t ac -> show' (fst t) ++ " " ++ show' n ++
>                                                    ";\n" ++ ac) []  (varctx i)
>                typs = Map.foldrWithKey (\n t ac -> "typedef " ++ show' t ++
>                                                    " " ++ show' n ++ ";\n" ++ ac) [] (tyctx i)
              
Showing type inference Information

> instance Show Info where
>    show i = "Var ctx:\n" ++ (show (varctx i)) ++
>             "\n\n" ++
>             "Ty ctx:\n" ++ (show (tyctx i))
              
Initial environment

> initialInfo :: Constraint -> Info
> initialInfo c = Info initialVarCtx
>                      initialTyCtx
>                      (length $ vars c)
              

> type SolverM a = ExceptT String (StateT Info IO) a              


> solver :: Constraint -> IO (Either String Info)
> solver c = do
>             (x, c') <- runStateT (runExceptT (solve c))
>                                  (initialInfo c)
>             print (removeBuiltIn c')                     
>             return $ either Left (const (Right $ removeBuiltIn c')) x
              
> solve :: Constraint -> SolverM ()
> solve c = do
>             c' <- build c
>             eqs <- simplify c'
>             eqs' <- mapM replace eqs
>             s <- unify eqs'
>             liftIO (mapM (\(n,t) -> putStrLn $ show $ pprint n <+> text "->>" <+> pprint t) (Map.toList s))      
>             updateEnv s

> removeBuiltIn :: Info -> Info
> removeBuiltIn i = i{ varctx = Map.filter (not . snd) $
>                                   varctx i Map.\\ initialVarCtx
>                    , tyctx = Map.filterWithKey notVar $
>                                  tyctx i Map.\\ initialTyCtx }
>                   where
>                     notVar v t = all ((/= "alpha") . take 5 . show) ([pprint v, pprint t])

Unification stuff

> type Subst = Map Name Ty

> nullSubst :: Subst
> nullSubst = Map.empty             

> unify :: [Constraint] -> SolverM Subst
> unify [] = return nullSubst
> unify ((t :=: t') : es)
>     = do
>        s <- unifyTy t t'
>        s' <- unify (map (apply' s) es)
>        return (s' @@ s)      

> apply' :: Subst -> Constraint -> Constraint
> apply' s (t :=: t') = (apply s t) :=: (apply s t')
         

> unifyTy :: Ty -> Ty -> SolverM Subst
> unifyTy t t' = if convertible t t' then return nullSubst
>                  else unifyTy' t t'
                   
> unifyTy' :: Ty -> Ty -> SolverM Subst
> unifyTy' (TyVar v) t
>     = varBind v t
> unifyTy' t (TyVar v)
>     = varBind v t
> unifyTy' t@(TyCon n) t'@(TyCon n')
>         | convertible t t' = return nullSubst            
>         | otherwise = unificationError t t'            
> unifyTy' (Pointer t) (Pointer t')
>     = unifyTy t t'
> unifyTy' (FunTy t ts) (FunTy t' ts')
>     = do
>         s  <- unifyTys ts ts'
>         s' <- unifyTy (apply s t)
>                       (apply s t')
>         return (s' @@ s)
> unifyTy' t@(Struct fs n) t'@(Struct fs' n')
>         | isVarName n || isVarName n' = fixRecordStructure t t'
>         | n /= n' = unificationError (TyCon n) (TyCon n')
>         | otherwise = unifyTys (map ty fs) (map ty fs')
> unifyTy' t t' = unificationError t t'
                

> unifyTys :: [Ty] -> [Ty] -> SolverM Subst
> unifyTys [] [] = return nullSubst            
> unifyTys [] _ = wrongArgumentNumberError
> unifyTys _ [] = wrongArgumentNumberError
> unifyTys (t : ts) (t' : ts')
>     = do
>          s <- unifyTy t t'       
>          s' <- unifyTys (map (apply s) ts)
>                         (map (apply s) ts')
>          return (s' @@ s)               
                
> varBind :: Name -> Ty -> SolverM Subst
> varBind n t
>     | n `elem` fv t = occursCheckError n t
>     | otherwise = return (Map.singleton n t)


        
> apply :: Subst -> Ty -> Ty
> apply s t@(TyVar v)
>     = maybe t id (Map.lookup v s)
> apply _ t@(TyCon _)
>     = t
> apply s (FunTy t ts)
>     = FunTy (apply s t)
>             (map (apply s) ts)
> apply s (Pointer t)
>     = Pointer (apply s t)
> apply s (Struct fs n)
>     = Struct (map (apply' s) fs) n
>       where
>         apply' s f = Field (name f)
>                            (apply s (ty f))

> fixRecordStructure :: Ty -> Ty -> SolverM Subst
> fixRecordStructure (Struct fs n) (Struct fs' n')
>                    | isVarName n && isVarName n'
>                      = do
>                          let s = (Map.insert n (Struct (fs ++ fs') n)
>                                                (Map.singleton n' (TyVar n')))
>                          --liftIO (mapM (\(n,t) -> putStrLn $ show $ pprint n <+> text "->>" <+> pprint t) (Map.toList s))
>                          return s
> fixRecordStructure _ _ = error ("Impossible this should not happen!" ++
>                                 "ConstraintSolver.fixRecordStructure")                       

                             
> unificationError :: Ty -> Ty -> SolverM a
> unificationError t t' = throwError ("Cannot unify\n"  ++
>                                     (show $ pprint t) ++
>                                     "\nwith\n"        ++
>                                     (show $ pprint t'))

> occursCheckError :: Name -> Ty -> SolverM a
> occursCheckError n t' = throwError ("Cannot unify\n"  ++
>                                     (show $ pprint n) ++
>                                     "\nwith\n"        ++
>                                     (show $ pprint t'))


> wrongArgumentNumberError :: SolverM a
> wrongArgumentNumberError
>     = throwError "Wrong argument number error in function call"
                                                                            
> (@@) :: Subst -> Subst -> Subst
> s1 @@ s2 = (Map.map (\t -> apply s1 t) s2) `Map.union` s1 
                                      
> updateEnv :: Subst -> SolverM ()
> updateEnv s
>     = do
>         modify (\i ->
>             i{
>                varctx = Map.map (\ (t,b) -> (apply s t , b)) (varctx i)
>              , tyctx  = fixStruct $ Map.map (apply s) (tyctx i)
>              })

> fixStruct :: Map Name Ty -> Map Name Ty
> fixStruct = Map.mapWithKey sub . Map.filterWithKey rem 
>               where
>                 rem k v = not (isVarName k)
>                           
>                 sub n (Struct fs m) = Struct fs n
>                 sub n t = t

                    
> fresh :: SolverM Ty
> fresh = do
>           i <- get
>           let n = counter i
>           put (i{counter = 1 + counter i})
>           return (TyVar (Name ("alpha" ++ show n)))    

Insert a declaration
            
> insertDef :: Name -> Ty -> Bool -> SolverM Constraint
> insertDef n t b = do
>                     i <- get
>                     let vctx = varctx i
>                     put (i{varctx = Map.insert n (t,b) vctx})
>                     return Truth

Insert a type definition
                    
> insertTypeDef :: Name -> Ty -> SolverM Constraint
> insertTypeDef n t = do
>                      i <- get
>                      let tctx = tyctx i
>                      maybe (put (i{tyctx = Map.insert n t tctx})
>                             >> return Truth)
>                            (return . ((:=:) t))
>                            (Map.lookup n tctx)

Insert field definition

> insField :: Ty -> Field -> SolverM Constraint
> insField t@(Struct fs n) f
>     = do
>         let fs' = [f' | f' <- fs, name f' == name f]
>         if null fs' then
>             modify (\i -> i{tyctx = Map.insert n t (tyctx i)}) >>
>             return Truth
>           else
>               return ((ty (head fs')) :=: ty f)
> insField (TyVar n) f
>     = do
>         let t = Struct [f] n
>         modify (\i -> i{tyctx = Map.insert n t (tyctx i)})
>         return ((TyVar n) :=: t)
> insField t f = error ("It should not happen!\ninsField:" ++ show (pprint t))

> insertField :: Name -> Field -> SolverM Constraint
> insertField n f = maybe (insField (TyVar n) f)
>                         (flip insField f)
>                         =<< (gets (Map.lookup n . tyctx))


> checkDef :: Name -> Ty -> SolverM Constraint
> checkDef n t = do
>                 i <- get
>                 let vctx = varctx i
>                 maybe (insertDef n t False >> return (t :=: t))
>                       (return . ((:=:) t) . fst)
>                       (Map.lookup n vctx)
     
Context construction from a constraint

> build :: Constraint -> SolverM Constraint
> build c@(_ :=: _) = return c
> build (n :<-: t) = checkDef n t
> build c@(Has t f) = insertField (nameOf t) f
> build (TypeDef n t) = insertTypeDef (nameOf n) t
> build (Def n t c) = insertDef n t True >> build c
> build (c :&: c') = liftM2 (:&:) (build c)
>                                 (build c')
> build (Exists n c) = build c
> build Truth = return Truth                     

Remove unnecessary Truth constraints
                          
> simplify :: Constraint -> SolverM [Constraint]
> simplify (Truth :&: c) = simplify c
> simplify (c :&: Truth) = simplify c
> simplify (c :&: c')
>     = do
>         c1 <- simplify c
>         c1' <- simplify c'
>         return (c1 ++ c1')
> simplify c = return [c]

  
Replace typedef       

> replace :: Constraint -> SolverM Constraint
> replace (t :=: t') = liftM2 (:=:) (replaceTy t)
>                                   (replaceTy t')
        

> replaceTy :: Ty -> SolverM Ty
> replaceTy t@(TyCon n)
>     = do
>         tctx <- gets tyctx
>         maybe (return t)
>               return
>               (Map.lookup n tctx)
> replaceTy (Pointer t)
>     = liftM Pointer (replaceTy t)
> replaceTy (FunTy t ts)
>     = liftM2 FunTy (replaceTy t)
>                    (mapM replaceTy ts)
> replaceTy (Struct fs n)
>     = liftM (flip Struct n)
>             (mapM go fs)
>       where
>         go f = liftM (Field (name f))
>                      (replaceTy (ty f))
> replaceTy t = return t                     


> isVarName :: Name -> Bool
> isVarName = (== "alpha") . take 5 . show . pprint             
