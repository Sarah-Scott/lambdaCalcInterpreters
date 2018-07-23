import Control.Applicative
import Control.Monad

data Reader e a = Reader (e -> a)

runR :: Reader e a -> e -> a
runR (Reader f) e = f e

  --makes the Reader a monad (copied from internet)
instance Functor (Reader e) where
  fmap = liftM

instance Applicative (Reader e) where
  pure = return
  (<*>) = ap

instance Monad (Reader e) where
  return x = Reader $ \e -> x
  g >>= f = Reader $ \e -> runR (f (runR g e)) e


  --just returns the current environment
ask :: Reader a a
ask = Reader $ \e -> e

  --returns the environment after a function has been applied to it
asks :: (e -> a) -> Reader e a
asks f = ask >>= \e -> (return (f e))

type Env = [(String,Term)]

lookupName :: String -> Env -> Term
lookupName s e = case (lookup s e) of Just x -> x
                                      _ -> error "name not found"

  --runs a Reader inside a temporary, local environment
local :: (e -> e') -> Reader e' t -> Reader e t
local f r = ask >>= \e -> return (runR r (f e))

data Term = Num Int |
            Plus Term Term |
            Minus Term Term |
            Bind String Term Term |
            Lambda String Term |
            Id String |
            App Term Term |        
            And Term Term |
            Leq Term Term |
            IsZero Term |
            If Term Term Term |
            Boolean Bool
          deriving (Show, Eq)

addVar :: String -> Term -> Env -> Env
addVar i t e = (i,t):e

eval :: Term -> Reader Env Term
eval t = case t of Num n -> return (Num n)
                   Plus t1 t2 -> do
                      Num n1 <- eval t1
                      Num n2 <- eval t2
                      return (Num ((+) n1 n2))
                   Minus t1 t2 -> do
                     Num n1 <- eval t1
                     Num n2 <- eval t2
                     return (Num ((-) n1 n2))
                   Lambda i b -> return (Lambda i b)
                   Id i -> do
                     env <- ask
                     return (lookupName i env)
                   Bind i v b -> do
                     v' <- eval v
                     local (addVar i v') (eval b)
                   App f a -> do
                     Lambda i b <- eval f
                     a' <- eval a
                     local (addVar i a') (eval b) 
                   And t1 t2 -> do
                     Boolean b1 <- eval t1
                     Boolean b2 <- eval t2
                     return (Boolean ((&&) b1 b2))
                   Leq t1 t2 -> do
                     Num n1 <- eval t1
                     Num n2 <- eval t2
                     return (Boolean ((<=) n1 n2))
                   IsZero t -> do
                     Num n <- eval t
                     return (Boolean ((==) n 0))
                   If t1 t2 t3 -> do
                     Boolean b <- eval t1
                     if b then (eval t2) else (eval t3)
                   Boolean b -> return (Boolean b)

evalStart :: Term -> Term
evalStart t = runR (eval t) []
         
--evalStart (If (And (Boolean True) (Boolean False)) (Num 6) (Minus (Num 4) (Num 1)))
         --Num 3
--evalStart (Bind "x" (Num 7) (Plus (Id "x") (Num 2)))
         --Num 9
--evalStart (App (Lambda "y" (And (Id "y") (Boolean True))) (Boolean False))
         --Boolean False
