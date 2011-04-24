
module Lambda.Bruijn where

data Bruijn = U Int
            | Lambda Bruijn
            | App Bruijn Bruijn
            deriving Show
              
describe :: Bruijn -> String
describe =  f
  where f (U k)       = show k
        f (Lambda t)  = "\\." ++ f t
        f (App t1 t2) = concat ["(", f t1, " ", f t2, ")"]
        
bPrint :: Bruijn -> IO ()
bPrint = putStrLn . describe

uArrow :: Int -> Int -> Bruijn -> Bruijn
uArrow d c = f
    where f t@(U k) | k < c = t
                    | True  = U (k + d)
          f (Lambda t1) =  Lambda (uArrow d (c + 1) t1)
          f (App t1 t2) = App (f t1) (f t2)

infixl 3 |->

(|->) :: Int -> Bruijn -> Bruijn -> Bruijn
j |-> s = f
  where f :: Bruijn -> Bruijn
        f t@(U k) | k == j = s
                  | True   = t
        f (Lambda t1) =  Lambda ((j + 1 |-> u_0_1 s) t1)
        f (App t1 t2) = App ((j |-> s) t1) ((j |-> s) t2)
        
        u_0_1 :: Bruijn -> Bruijn
        u_0_1 =  uArrow 1 0
        
-- \ x -> \ y -> x (y z)
dEx6221 :: Bruijn
dEx6221 =  Lambda (Lambda (App (U 1) (App (U 0) (U 2))))

-- \ x -> ((x y) (\ y (( 
dEx6222 :: Bruijn
dEx6222 =  Lambda (App (App (U 0) (U 1)) (Lambda (App (App (U 0) (U 1)) (U 2))))

testEx6221 :: Bruijn
testEx6221 =  uArrow 2 0 dEx6221

testEx6222 :: Bruijn
testEx6222 =  uArrow 2 0 dEx6222

-- v_2 vf vb
