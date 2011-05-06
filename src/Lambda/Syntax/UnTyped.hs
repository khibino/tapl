
{-- Implementation of Chapter 7
 -- An ML Implementation of Lambda Calculus
 --}

module Lambda.Syntax.UnTyped where

import Data.Char

data Info = Info deriving (Show, Eq)
            
data Term = TmVar Info Int Int
          | TmAbs Info String Term
          | TmApp Info Term Term
          deriving (Show, Eq)

data Binding = NameBind

type Context = [(String, Binding)]

index2name :: Info -> Context -> Int -> String
index2name _ ctx = fst . (ctx !!)

ctxLength :: Context -> Int
ctxLength =  length

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx hint@(h:hs) =
  case lookup hint ctx of
    Nothing -> ((hint, NameBind) : ctx, hint)
    Just _  -> pickFreshName ctx (if h < 'k' then chr (ord h + 1) : hs else 'a' : hint)
pickFreshName ctx "" = pickFreshName ctx "a"

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

eval1 :: Context -> Term -> Maybe Term
eval1 ctx = f
  where f (TmApp _  (TmAbs _ _ t12) v2) | isVal ctx v2 =
          Just $ termSubstTop v2 t12
        f (TmApp fi v1 t2) | isVal ctx v1 =
          do t2' <- eval1 ctx t2
             Just $ TmApp fi v1 t2'
        f (TmApp fi t1 t2) =
          do t1' <- eval1 ctx t1
             Just $ TmApp fi t1' t2
        f _                = Nothing

eval   :: Context -> Term -> Term
eval ctx t = case eval1 ctx t of
  Just t' | t' /= t   -> eval ctx t'
          | otherwise -> t
  Nothing             -> t


eval' :: Term -> IO ()
eval' =  (>> putChar '\n') . printTm [] . eval []

var :: Int -> Int -> Term
var   = TmVar Info

abs' :: String -> Term -> Term
abs'   = TmAbs Info

app :: Term -> Term -> Term
app   = TmApp Info

ex0 :: Term
ex0 =  app
       (abs' "u" (app (var 0 1) (var 0 1)))
       (app
        (abs' "x" (app (var 0 1) (var 0 1)))
        (app
         (abs' "y" (app (var 0 1) (var 0 1)))
         (abs' "z" (var 0 1))))

test0 :: IO ()
test0 =  eval' ex0

true  = abs' "t" (abs' "f" (var 1 2))
false = abs' "t" (abs' "f" (var 0 2))

ifTerm =
  abs' "l" (abs' "m" (abs' "n"
                      (app
                       (app
                        (var 2 3)
                        (var 1 3))
                       (var 0 3))))

if' :: Term -> Term -> Term -> Term
if' p t e = app (app (app ifTerm p) t) e
         
zero = abs' "s" (abs' "z" (var 0 2))

succTerm =
  abs' "n" (abs' "s" (abs' "z"
                      (app
                       (var 1 3)
                       (app
                        (app (var 2 3) (var 1 3))
                        (var 0 3)))))

succ' :: Term -> Term
succ' = app succTerm

true, false, zero :: Term
ifTerm, succTerm :: Term


exOne :: Term
exOne =  succ' zero

ex1 :: Term
ex1 =  if' false (succ' zero) (succ' (succ' zero))

test1 :: IO ()
test1 =  eval' ex1
