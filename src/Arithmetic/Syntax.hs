{-# LANGUAGE PatternGuards #-}

module Arithmetic.Syntax where

import Data.Maybe

type Info = ()

dummyInfo :: Info
dummyInfo =  ()

data Term = TmTrue  Info
          | TmFalse Info
          | TmIf Info Term Term Term
          | TmZero Info
          | TmSucc Info Term
          | TmPred Info Term
          | TmIsZero Info Term
            deriving Show
                     


isNumericVal :: Term -> Bool
isNumericVal =  p
  where p (TmZero _)    = True
        p (TmSucc _ t1) = p t1
        p _             = False

isVal :: Term -> Bool
isVal =  p
  where p (TmTrue _)         = True
        p (TmFalse _)        = True
        p t | isNumericVal t = True
        p _                  = False


eval1 :: Term -> Maybe Term
eval1 =  rec
  where rec (TmIf fi cond' then' else') | TmTrue  _ <- cond' = Just then'
                                        | TmFalse _ <- cond' = Just else'
                                        | True               =
                                          cond'' >>= (\ c' -> Just $ TmIf fi c' then' else')
                                           where cond'' = rec cond'

        rec (TmSucc fi t)          = fmap (TmSucc fi) t' where t' = rec t

        rec (TmPred _  (TmZero _)) = Just $ TmZero dummyInfo
        rec (TmPred _  (TmSucc _ v)) | isNumericVal v = Just v
        rec (TmPred fi t)          =  fmap (TmPred fi) t' where t' = rec t

        rec (TmIsZero _  (TmZero _)) = Just $ TmTrue dummyInfo
        rec (TmIsZero _  (TmSucc _ v)) | isNumericVal v = Just $ TmFalse dummyInfo
        rec (TmIsZero fi t)          = fmap (TmIsZero fi) t' where t' = rec t
        rec _                        = Nothing

eval   :: Term -> Term
eval t =  fromMaybe t (fmap eval t')
  where t' = eval1 t


true  = TmTrue  dummyInfo
false = TmFalse dummyInfo
if'   = TmIf dummyInfo
zero  = TmZero dummyInfo
succ'  = TmSucc dummyInfo
pred'  = TmPred dummyInfo
isZero = TmIsZero dummyInfo

true, false, zero  :: Term
if' :: Term -> Term -> Term -> Term
succ', pred', isZero :: Term -> Term


-- if true then true else (if false then false else false)
ex0 :: Term
ex0 = if' true true (if' false false false)

test0 :: Term
test0 =  eval ex0

-- if (iszero (pred (succ zero))) then true else (if false then false else false)
ex1 :: Term
ex1 = if' (isZero (pred' (succ' zero))) true (if' false false false)

test1 :: Term
test1 =  eval ex0



{--
eval1 :: Term -> Term
eval1 =  rec
  where rec (TmIf fi cond' then' else') | TmTrue  _ <- cond' = then'
                                        | TmFalse _ <- cond' = else'
                                        | True               =
                                          TmIf fi cond'' then' else'
                                            where cond'' = rec cond'

        rec (TmSucc fi t)          = TmSucc fi t' where t' = rec t

        rec (TmPred _  (TmZero _)) = TmZero dummyInfo
        rec (TmPred _  (TmSucc _ v)) | isNumericVal v = v
        rec (TmPred fi t)          = TmPred fi t' where t' = rec t

        rec (TmIsZero _  (TmZero _)) = TmTrue dummyInfo
        rec (TmIsZero _  (TmSucc _ v)) | isNumericVal v = TmFalse dummyInfo
        rec (TmIsZero fi t)          = TmIsZero fi t' where t' = rec t
        rec _                        = error "NoRuleApplies"
--}
