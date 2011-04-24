
module Lambda.Symtax where

data Info = Info ()
            
data Term = TmVar Info Int Int
          | TmAbs Info String Term
          | TmApp Info Term Term

data NameBind = NameBind ()

type Binding = NameBind

type Context = [(String, Binding)]

index2name :: Info -> Context -> Int -> String
index2name _ ctx = fst . (ctx !!)

ctxLength :: Context -> Int
ctxLength =  length

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx hint =
  case lookup hint ctx of
    Nothing -> ((hint, NameBind ()) : ctx, hint)
    Just _  -> pickFreshName ctx ("_" ++ hint)

printTm :: Context -> Term -> IO ()
printTm ctx = f
  where f (TmAbs _  x t1) =
          let (ctx', x') = pickFreshName ctx x in
          putStr ("(lambda " ++ x' ++ ". ") >> printTm ctx' t1 >> putStr ")"

        f (TmApp _  t1 t2) =
          putStr "(" >> printTm ctx t1 >> putStr " " >> printTm ctx t2 >> putStr ")"
        f (TmVar fi x n) =
          if ctxLength ctx == n then putStr (index2name fi ctx x)
          else putStr "[bad index]"

termShift :: Int -> Term -> Term
termShift d t = walk 0 t
  where walk :: Int -> Term -> Term
        walk c = f
          where f (TmVar fi x n)   = if x >= c then TmVar fi (x + d) (n + d)
                                     else           TmVar fi x (n + d)
                f (TmAbs fi x t1)  = TmAbs fi x (walk (c + 1) t1)
                f (TmApp fi t1 t2) = TmApp fi (walk c t1) (walk c t2)

termSubst :: Int -> Term -> Term -> Term
termSubst j s t = walk 0 t
  where walk :: Int -> Term -> Term
        walk c = f
          where f (TmVar fi x n)   = if x == j + c then termShift c s
                                     else TmVar fi x n
                f (TmAbs fi x t1)  = TmAbs fi x (walk (c + 1) t1)
                f (TmApp fi t1 t2) = TmApp fi (walk c t1) (walk c t2)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

isVal :: Context -> Term -> Bool
isVal _ (TmAbs _ _ _) = True
isVal _ _             = False

eval1 :: Context -> Term -> Term
eval1 ctx = f
  where f (TmApp _  (TmAbs _ _ t12) v2) | isVal ctx v2 =
          termSubstTop v2 t12
        f (TmApp fi v1 t2) | isVal ctx v1 =
          let t2' = eval1 ctx t2 in TmApp fi v1 t2'
        f (TmApp fi t1 t2) =
          let t1' = eval1 ctx t1 in
          TmApp fi t1' t2
        f _                = error "No Rule Applies"

