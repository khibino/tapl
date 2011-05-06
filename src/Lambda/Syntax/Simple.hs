
{-- Implementation of Chapter 10
 -- An ML Implementation of Simple Types
 --}

module Lambda.Syntax.Simple where

--import Debug.Trace (trace)

data Info = Info deriving (Show, Eq)
            
data Ty = TyBool
        | TyArr Ty Ty
        deriving (Show, Eq)

showTy :: Ty -> String
showTy =  f
    where f TyBool        = "bool"
          f (TyArr t0 t1) = uni t0 ++ " → " ++ f t1
          uni TyBool = f TyBool
          uni tm     = "(" ++ f tm ++ ")"

--instance Show Ty where
--  show = showTy

data Term = TmTrue  Info
          | TmFalse Info
          | TmIf  Info Term Term Term
          | TmVar Info Int Int
          | TmAbs Info String Ty Term
          | TmApp Info Term Term
          deriving (Show, Eq)

data Binding = NameBind
             | VarBind Ty

type Context = [(String, Binding)]

addBinding :: Context -> String -> Binding -> Context
addBinding ctx x bind =  (x, bind) : ctx

error' :: Info -> String -> a
error' _ = error

getTypeFromContext :: Info -> Context -> Int -> Ty
getTypeFromContext fi ctx i =
  case getBinding fi ctx i of
    VarBind ty -> ty
    _          -> error' fi
                  ("getTypeFromContext: Wrong kind of binding for variable "
                   ++ (index2name fi ctx i))

getBinding' :: Info -> Context -> Int -> (String, Binding)
getBinding' _ ctx = (ctx !!)

getBinding :: Info -> Context -> Int -> Binding
getBinding fi ctx = snd . getBinding' fi ctx

index2name :: Info -> Context -> Int -> String
index2name fi ctx = fst . getBinding' fi ctx

ctxLength :: Context -> Int
ctxLength =  length

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx hint =
  case lookup hint ctx of
    Nothing -> ((hint, NameBind) : ctx, hint)
    Just _  -> pickFreshName ctx ("_" ++ hint)


showTerm :: Context -> Term -> String
showTerm ctx = f
  where f (TmTrue _)  = "true"
        f (TmFalse _) = "false"
        f (TmIf _ pred' then' else') = "(if (" ++ f pred' ++ ")"
                                       ++ " (" ++ f then' ++ ")"
                                       ++ " (" ++ f else' ++ "))"
        f (TmAbs _  x ty t1) =
          let (ctx', x') = pickFreshName ctx x in
          "(\\ " ++ x' ++ ":" ++ showTy ty ++ ". " ++ showTerm ctx' t1 ++ ")"

        f (TmApp _  t1 t2) =
          "(" ++ f t1 ++ " " ++ f t2 ++ ")"
        f (TmVar fi x n) =
          if ctxLength ctx == n then index2name fi ctx x
          else error' fi "[bad index]"

printTerm :: Context -> Term -> IO ()
printTerm ctx = putStrLn . showTerm ctx

{--
printTm :: Context -> Term -> IO ()
printTm ctx = f
  where f (TmTrue _)  = putStr "true"
        f (TmFalse _) = putStr "false"
        f (TmIf _ pred' then' else') = putStr "(if (" >> f pred' >> putStr ")"
                                       >> putStr " (" >> f then' >> putStr ")"
                                       >> putStr " (" >> f else' >> putStr "))"
        f (TmAbs _  x ty t1) =
          let (ctx', x') = pickFreshName ctx x in
          putStr ("(\\ " ++ x' ++ ":" ++ show ty ++ ". ") >> printTm ctx' t1 >> putStr ")"

        f (TmApp _  t1 t2) =
          putStr "(" >> f t1 >> putStr " " >> f t2 >> putStr ")"
        f (TmVar fi x n) =
          if ctxLength ctx == n then putStr (index2name fi ctx x)
          else putStr "[bad index]"
--}

typeOf :: Context -> Term -> Ty
typeOf ctx t =
  case t of
    TmTrue  _ -> TyBool
    TmFalse _ -> TyBool

    TmIf fi pred' then' else' ->
      if typeOf ctx pred' == TyBool then
        let tyThen = typeOf ctx then' in
        if tyThen == typeOf ctx else' then tyThen
        else error' fi "arms of conditional have different types"
      else error' fi "guard of conditional not a boolean"

    TmVar fi i _ -> getTypeFromContext fi ctx i
    TmAbs _ x tyArg body ->
      let ctx' = addBinding ctx x (VarBind tyArg)
          tyBody = typeOf ctx' body in
      TyArr tyArg tyBody

    TmApp fi fun arg ->
      let tyFun = typeOf ctx fun
          tyArg = typeOf ctx arg in
      case tyFun of
        TyArr tyArg' tyBody ->
          if tyArg == tyArg' then tyBody
          else error' fi "parameter type mismatch"
        _ -> error' fi "arrow type expected"

termShift :: Int -> Term -> Term
termShift d t = walk 0 t
  where walk :: Int -> Term -> Term
        walk c = f
          where f bt@(TmTrue _)     = bt
                f bt@(TmFalse _)    = bt
                f (TmIf fi pred' then' else') = TmIf fi (f pred') (f then') (f else')
                f (TmVar fi x n)   = if x >= c then TmVar fi (x + d) (n + d)
                                     else           TmVar fi x (n + d)
                f (TmAbs fi x ty t1)  = TmAbs fi x ty (walk (c + 1) t1)
                f (TmApp fi t1 t2) = TmApp fi (f t1) (f t2)

termSubst :: Int -> Term -> Term -> Term
termSubst j s t = walk 0 t
  where walk :: Int -> Term -> Term
        walk c = f
          where f bt@(TmTrue _)       = bt
                f bt@(TmFalse _)      = bt
                f (TmIf fi pred' then' else') = TmIf fi (f pred') (f then') (f else')
                f (TmVar fi x n)   = if x == j + c then termShift c s
                                     else TmVar fi x n
                f (TmAbs fi x ty t1)  = TmAbs fi x ty (walk (c + 1) t1)
                f (TmApp fi t1 t2) = TmApp fi (f t1) (f t2)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

isVal :: Context -> Term -> Bool
isVal _ (TmTrue  _) = True
isVal _ (TmFalse _) = True
isVal _ (TmAbs _ _ _ _) = True
isVal _ _               = False

eval1 :: Context -> Term -> Maybe Term
eval1 ctx = f
  where f (TmIf fi cond' then' else') =
          case cond' of
            TmTrue  _ -> Just then'
            TmFalse _ -> Just else'
            _         -> 
              do cond'' <- f cond'
                 Just $ TmIf fi cond'' then' else'
        
        f t@(TmVar _ _ _) = error $ "Evaluation rule undefined: " ++ show t
        f (TmApp _  (TmAbs _ _ _ t12) v2) | isVal ctx v2 =
          Just $ termSubstTop v2 t12
        f (TmApp fi v1 t2) | isVal ctx v1 =
          do t2' <- eval1 ctx t2 
             Just $ TmApp fi v1 t2'
        f (TmApp fi t1 t2) =
          do t1' <- eval1 ctx t1
             Just $ TmApp fi t1' t2
        f t | isVal ctx t = Just t
            | otherwise   = Nothing

eval   :: Context -> Term -> Term
eval ctx t = case eval1 ctx t of
  Just t' | t' /= t   -> eval ctx t'
          | otherwise -> t
  Nothing             -> t

typeAndEval :: Context -> Term -> (Ty, Term)
typeAndEval ctx t = (typeOf ctx t, eval ctx t)

evalTop :: Term -> (Ty, Term)
evalTop t = typeAndEval [] t

showResult :: Context -> (Ty, Term) -> IO ()
showResult ctx (ty, tm) =
  do putStrLn $ "type: " ++ (showTy ty)
     putStrLn $ "result: " ++ showTerm ctx tm

eval' :: Term -> IO ()
eval' t = showResult [] (evalTop t)


tmTrue  = TmTrue  Info
tmFalse = TmFalse Info
tmTrue, tmFalse :: Term

tmIf :: Term -> Term -> Term -> Term
tmIf    = TmIf  Info

tmVar :: Int -> Int -> Term
tmVar   = TmVar Info

tmAbs :: String -> Ty -> Term -> Term
tmAbs   = TmAbs Info

tmApp :: Term -> Term -> Term
tmApp   = TmApp Info



ex0 :: Term
ex0 =  TmFalse Info

test0 :: IO ()
test0 =  eval' ex0

ex1 :: Term
ex1 =  tmIf
       tmFalse
       (tmAbs "x" (TyArr TyBool TyBool) (tmApp (tmVar 0 1) tmTrue))
       (tmAbs "y" (TyArr TyBool TyBool) (tmApp (tmVar 0 1) tmFalse))

test1 :: IO ()
test1 =  eval' ex1

ex2 :: Term
ex2 =  tmApp
       (tmAbs "y" (TyArr TyBool TyBool) (tmApp (tmVar 0 1) tmFalse))
       (tmAbs "z" TyBool (tmVar 0 1))

test2 :: IO ()
test2 =  eval' ex2

ex3 :: Term
ex3 =  tmApp
       (tmIf tmFalse
        (tmAbs "x" (TyArr TyBool TyBool) (tmApp (tmVar 0 1) tmTrue))
        (tmAbs "y" (TyArr TyBool TyBool) (tmApp (tmVar 0 1) tmFalse)))
       (tmAbs "z" TyBool (tmVar 0 1))

test3 :: IO ()
test3 =  eval' ex3
