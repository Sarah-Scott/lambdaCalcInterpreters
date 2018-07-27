import Control.Monad
import Control.Applicative

-------------------------------------------------------------------------

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
  
-------------------------------------------------------------------------

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Monad m => Monad (ReaderT r m) where
  return = liftReaderT . return
  m >>= k = ReaderT $ \r -> do
    a <- runReaderT m r
    runReaderT (k a) r

instance Applicative m => Applicative (ReaderT r m) where
  pure = liftReaderT . pure
  f <*> v = ReaderT $ \r -> runReaderT f r <*> runReaderT v r

instance Functor m => Functor (ReaderT r m) where
  fmap f = mapReaderT (fmap f)

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f m = ReaderT $ f . runReaderT m

ask :: (Monad m) => ReaderT r m r
ask = ReaderT return

local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local = withReaderT

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f

-------------------------------------------------------------------------

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

addVarType :: String -> TermType -> Context -> Context
addVarType i t e = (i,t):e

lookupVarType :: String -> Context -> Either String TermType
lookupVarType i e = case (lookup i e) of Just x -> Right x
                                         _ -> Left "Variable not found"

typeOfTerm :: Term -> ReaderT Context (Either String) TermType
typeOfTerm t = case t of Num n -> return TNum
                         Plus t1 t2 -> do
                           t1' <- typeOfTerm t1
                           t2' <- typeOfTerm t2
                           if (t1'==TNum && t2'==TNum) then (return TNum) else (liftReaderT $ Left "Type error in Plus")
                         Minus t1 t2 -> do
                           t1' <- typeOfTerm t1
                           t2' <- typeOfTerm t2
                           if (t1'==TNum && t2'==TNum) then (return TNum) else (liftReaderT $ Left "Type error in Minus")
                         Boolean b -> return TBool
                         Bind i v b -> do
                           cont <- ask
                           v' <- typeOfTerm v
                           local (addVarType i v') (typeOfTerm b)
                         Id i -> do
                           cont <- ask
                           liftReaderT $ lookupVarType i cont
                         Lambda i ty b -> do
                           bType <- local (addVarType i ty) (typeOfTerm b)
                           return (TFunc ty bType)
                         App f a -> do
                           TFunc domainType rangeType <- typeOfTerm f
                           aType <- typeOfTerm a
                           if (domainType==aType) then (return rangeType) else (liftReaderT $ Left "Type error in App")
                         And t1 t2 -> do
                           t1' <- typeOfTerm t1
                           t2' <- typeOfTerm t2
                           if (t1'==TBool && t2'==TBool) then (return TBool) else (liftReaderT $ Left "Type error in And")
                         Leq t1 t2 -> do
                           t1' <- typeOfTerm t1
                           t2' <- typeOfTerm t2
                           if (t1'==TNum && t2'==TNum) then (return TBool) else (liftReaderT $ Left "Type error in Leq")
                         IsZero t -> do
                           t' <- typeOfTerm t
                           if (t'==TNum) then (return TBool) else (liftReaderT $ Left "Type error in IsZero")
                           
runRead :: ReaderT r m a -> r -> m a
runRead (ReaderT f) e = f e

getTypeOf :: Term -> Either String TermType
getTypeOf t = runRead (typeOfTerm t) []

-------------------------------------------------------------------------

{-
getTypeOf (Bind "x" (Num 7) (Plus (Id "x") (Num 2)))
     Right TNum
getTypeOf (Bind "x" (Num 7) (Plus (Id "x") (Boolean True)))
     Left "Type error in Plus"
getTypeOf (App (Lambda "y" (TBool) (And (Id "y") (Boolean True))) (Boolean False))
     Right TBool
getTypeOf (App (Lambda "y" (TBool) (And (Id "y") (Num 3))) (Boolean False))
     Left "Type error in And"
getTypeOf (App (Lambda "x" TNum (Plus (Num 3) (Id "x"))) (Boolean False))
     Left "Type error in App"
getTypeOf (App (Lambda "x" TBool (And (Id "y") (Boolean True))) (Boolean False))
     Left "Variable not found"
-}
