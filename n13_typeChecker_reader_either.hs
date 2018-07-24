import Control.Applicative
import Data.Either
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
            IsZero Term
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

lookupVarType :: String -> Context -> Either String TermType
lookupVarType s e = case (lookup s e) of Just x -> Right x
                                         _ -> Left "Variable not found"

local :: (e -> e') -> Reader e' t -> Reader e t
local f r = ask >>= \e -> return (runR r (f e))

typeOfTerm :: Term -> Reader Context (Either String TermType)
typeOfTerm t = case t of Num n -> return (Right TNum)
                         Plus t1 t2 -> do
                           t1' <- (typeOfTerm t1)
                           t2' <- (typeOfTerm t2)
                           case t1' of Right TNum -> case t2' of Right TNum -> return (Right TNum)
                                                                 Right _ -> return (Left "Type error in Plus")
                                                                 _ -> return t2'
                                       Right _ -> return (Left "Type error in Plus")
                                       _ -> return t1'
                         Boolean b -> return (Right TBool)
                         Bind i v b -> do
                           cont <- ask
                           v' <- typeOfTerm v
                           case v' of Right v'' -> local (addVarType i v'') (typeOfTerm b)
                                      _ -> (return v')
                         Id i -> do
                           cont <- ask
                           return (lookupVarType i cont)
                         Lambda i ty b -> do
                           b' <- local (addVarType i ty) (typeOfTerm b)
                           case b' of Right bType -> return (Right (TFunc ty bType))
                                      _ -> return b'
                         App f a -> do
                           f' <- typeOfTerm f
                           a' <- typeOfTerm a
                           case f' of (Right (TFunc domainType rangeType)) -> case a' of Right aType -> if (domainType == aType) then return (Right rangeType) else return (Left "Type error in App")
                                                                                         _ -> return a'
                                      _ -> return f'
                         Minus t1 t2 -> do
                           t1' <- (typeOfTerm t1)
                           t2' <- (typeOfTerm t2)
                           case t1' of Right TNum -> case t2' of Right TNum -> return (Right TNum)
                                                                 Right _ -> return (Left "Type error in Minus")
                                                                 _ -> return t2'
                                       Right _ -> return (Left "Type error in Minus")
                                       _ -> return t1'
                         And t1 t2 -> do
                           t1' <- typeOfTerm t1
                           t2' <- typeOfTerm t2
                           case t1' of Right TBool -> case t2' of Right TBool -> return (Right TBool)
                                                                  Right _ -> return (Left "Type error in And")
                                                                  _ -> return t2'
                                       Right _ -> return (Left "Type error in And")
                                       _ -> return t1'
                         Leq t1 t2 -> do
                           t1' <- typeOfTerm t1
                           t2' <- typeOfTerm t2
                           case t1' of Right TNum -> case t2' of Right TNum -> return (Right TBool)
                                                                 Right _ -> return (Left "Type error in Leq")
                                                                 _ -> return t2'
                                       Right _ -> return (Left "Type error in Leq")
                                       _ -> return t1'
                         IsZero t -> do
                           t' <- typeOfTerm t
                           case t' of Right TNum -> return (Right TBool)
                                      Right _ -> return (Left "Type error in IsZero")
                                      _ -> return t'


                             
getTypeOf :: Term -> Either String TermType
getTypeOf t = runR (typeOfTerm t) []


                           
