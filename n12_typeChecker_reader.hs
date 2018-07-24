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

data TermType = TNum |
                TBool |
                TFunc TermType TermType
              deriving (Show, Eq)

type Context = [(String,TermType)]


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

addVarType :: String -> TermType -> Context -> Context
addVarType i t e = (i,t):e

lookupVarType :: String -> Context -> TermType
lookupVarType s e = case (lookup s e) of Just x -> x
                                         _ -> error "Variable not found"

local :: (e -> e') -> Reader e' t -> Reader e t
local f r = ask >>= \e -> return (runR r (f e))

typeOfTerm :: Term -> Reader Context TermType
typeOfTerm t = case t of Num n -> return TNum
                         Plus t1 t2 -> do
                           t1' <- (typeOfTerm t1)
                           t2' <- (typeOfTerm t2)
                           return (if (t1' == TNum && t2' == TNum) then TNum else error "Type error in Plus")
                         Minus t1 t2 -> do
                           t1' <- (typeOfTerm t1)
                           t2' <- (typeOfTerm t2)
                           return (if (t1' == TNum && t2' == TNum) then TNum else error "Type error in Minus")
                         Bind i v b -> do
                           cont <- ask
                           v' <- typeOfTerm v
                           local (addVarType i v') (typeOfTerm b)
                         Lambda i ty b -> do
                           bType <- local (addVarType i ty) (typeOfTerm b)
                           return (TFunc ty bType)
                         App f a -> do
                           TFunc domainType rangeType <- typeOfTerm f
                           aType <- typeOfTerm a
                           return (if (domainType == aType) then rangeType else error "Type error in App")
                         Id i -> do
                           cont <- ask
                           return (lookupVarType i cont)
                         Boolean b -> return TBool
                         And t1 t2 -> do
                           t1' <- (typeOfTerm t1)
                           t2' <- (typeOfTerm t2)
                           return (if (t1' == TBool && t2' == TBool) then TBool else error "Type error in And")
                         Leq t1 t2 -> do
                           t1' <- (typeOfTerm t1)
                           t2' <- (typeOfTerm t2)
                           return (if (t1' == TNum && t2' == TNum) then TBool else error "Type error in Leq")
                         IsZero t -> do
                           t' <- (typeOfTerm t)
                           return (if (t' == TNum) then TBool else error "Type error in IsZero")

getTypeOf :: Term -> TermType
getTypeOf t = runR (typeOfTerm t) []


                           
