import Control.Applicative
import Control.Monad

data Term = Num Int |
            Plus Term Term |
            Minus Term Term |
            Bind String Term Term |
            Lambda String TermType Term |
            App Term Term |
            Id String |
            Boolean Bool |
            And Term Term |
            Leq Term Term |
            IsZero Term |
            If Term Term Term
          deriving (Show, Eq)

---------------------------------------------------------------------------

data TermType = TNum |
                TBool |
                TFunc TermType TermType
              deriving (Show, Eq)

type Context = [(String,TermType)]

typeOfTerm :: Context -> Term -> (Maybe TermType)
typeOfTerm cont t = case t of Num n -> return TNum
                              Plus t1 t2 -> do
                                TNum <- (typeOfTerm cont t1)
                                TNum <- (typeOfTerm cont t2)
                                return TNum
                              Minus t1 t2 -> do
                                TNum <- (typeOfTerm cont t1)
                                TNum <- (typeOfTerm cont t2)
                                return TNum
                              Bind i v b -> do
                                v' <- (typeOfTerm cont v)
                                typeOfTerm ((i,v'):cont) b
                              Id i -> lookup i cont
                              Lambda i ty b -> do
                                bType <- typeOfTerm ((i,ty):cont) b
                                return (TFunc ty bType)
                              App f a -> do
                                TFunc domainType rangeType <- typeOfTerm cont f
                                aType <- typeOfTerm cont a
                                if (domainType == aType)
                                  then (return rangeType)
                                  else Nothing
                              Boolean b -> return TBool
                              And t1 t2 -> do
                                TBool <- (typeOfTerm cont t1)
                                TBool <- (typeOfTerm cont t2)
                                return TBool
                              Leq t1 t2 -> do
                                TNum <- (typeOfTerm cont t1)
                                TNum <- (typeOfTerm cont t2)
                                return TBool
                              IsZero n -> do
                                TNum <- (typeOfTerm cont n)
                                return TBool
                              If t1 t2 t3 -> do
                                TBool <- (typeOfTerm cont t1)
                                if ((evalStart t1) == (Boolean True))
                                  then (typeOfTerm cont t2)
                                  else (typeOfTerm cont t3)

getTypeOf :: Term -> (Maybe TermType)
getTypeOf t = typeOfTerm [] t

--------------------------------------------------------------------------

data Reader e a = Reader (e -> a)

runR :: Reader e a -> e -> a
runR (Reader f) e = f e

instance Functor (Reader e) where
  fmap = liftM

instance Applicative (Reader e) where
  pure = return
  (<*>) = ap

instance Monad (Reader e) where
  return x = Reader $ \e -> x
  g >>= f = Reader $ \e -> runR (f (runR g e)) e

ask :: Reader a a
ask = Reader $ \e -> e

asks :: (e -> a) -> Reader e a
asks f = ask >>= \e -> (return (f e))

type Env = [(String,Term)]

lookupName :: String -> Env -> Term
lookupName s e = case (lookup s e) of Just x -> x
                                      _ -> error "name not found"

local :: (e -> e') -> Reader e' t -> Reader e t
local f r = ask >>= \e -> return (runR r (f e))

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
                   Lambda i ty b -> return (Lambda i ty b)
                   Id i -> do
                     env <- ask
                     return (lookupName i env)
                   Bind i v b -> do
                     v' <- eval v
                     local (addVar i v') (eval b)
                   App f a -> do
                     Lambda i ty b <- eval f
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
                   Boolean b -> return (Boolean b)
                   If t1 t2 t3 -> do
                     Boolean b <- eval t1
                     if b then (eval t2) else (eval t3)

evalStart :: Term -> Term
evalStart t = if (getTypeOf t == Nothing) then (error "Does not type check") else (runR (eval t) [])

--------------------------------------------------------------------------

--evalStart (Bind "x" (Num 7) (Plus (Id "x") (Num 2)))
         --Num 9
--evalStart (Bind "x" (Num 7) (Plus (Id "x") (Boolean True)))
         --Exception

--evalStart (App (Lambda "y" (TBool) (And (Id "y") (Boolean True))) (Boolean False))
         --Boolean False
--evalStart (App (Lambda "y" (TBool) (And (Id "y") (Num 3))) (Boolean False))
         --Exception

--evalStart (If (And (Boolean True) (Boolean False)) (Num 6) (Minus (Num 4) (Num 1)))
         --Num 3
--evalStart (If (Minus (Num 4) (Num 1)) (Num 6) (Minus (Num 4) (Num 1)))
         --Exception

--evalStart (Plus (Minus (Num 3) (Num 1)) (If (IsZero (Num 4)) (Boolean True) (Plus (Num 7) (Num 2))))
         --Num 11
--evalStart (Plus (Minus (Num 3) (Num 1)) (If (IsZero (Num 4)) (Num 6) (Boolean False)))
         --Exception
