import Control.Applicative
import Control.Monad
import Test.QuickCheck

------------------------------------------------------------------------------------

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

local :: (e -> e') -> Reader e' t -> Reader e t
local f r = ask >>= \e -> return (runR r (f e))

------------------------------------------------------------------------------------

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

askT :: (Monad m) => ReaderT r m r
askT = ReaderT return

localT :: (r -> r) -> ReaderT r m a -> ReaderT r m a
localT = withReaderT

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f


------------------------------------------------------------------------------------



type Id = Char

type Place = Int

data APDT = VAR Id |
            LN APDT APDT | --Linear (Sequential)
            BR APDT APDT | --Branch (Parallel)
            AT Place APDT |
            SIG |
            KIM Place |
            USM |
            Lambda Id T APDT |
            App APDT APDT |
            V Val
          deriving (Show, Eq)

data Val =  Mt |
            Sig Val Place |
            Kim Place Place |
            Usm Place |
            Nonce Place |
            SeqV Val Val |
            ParV Val Val
         deriving (Show,Eq)

type Env = [(Id,Val)] 

isEvid :: APDT -> Bool
isEvid ev = case ev of V x -> True
                       _ -> False

getVal :: APDT -> Val
getVal t = case t of V v -> v

addVar :: Id -> Val -> Env -> Env
addVar i t env = (i,t):env

--lambdas will use debruijn indices for evaluation
--the type being included in the lambda seems like overkill
--does the first term in the App need to be a lambda term??
eval :: (APDT,Place) -> ReaderT Env (Either String) (APDT,Place)
eval (t,p) = case t of KIM q -> return (V (Kim q p), p)
                       USM -> return (V (Usm p), p)
                       LN t0 t1 -> if (isEvid t0)
                                      then (if (t1==SIG)
                                             then return (V (SeqV (getVal t0) (Sig (getVal t0) p)), p)
                                             else do
                                               (t1',p') <- eval (t1,p)
                                               return (V (SeqV (getVal t0) (getVal t1')), p))
                                   else do
                         (t0',p') <- eval (t0,p)
                         eval (LN t0' t1, p)
                       BR t0 t1 -> do
                         (t0',p0) <- eval (t0,p)
                         (t1',p1) <- eval (t1,p)
                         return (V (ParV (getVal t0') (getVal t1')), p)
                       AT q t0 -> do
                         (t0',q') <- eval (t0,q)
                         return (t0',p)
                       VAR i -> do
                         env <- askT
                         let m = lookup i env
                           in case m of Just v -> (return (V v,p))    
                                        _ -> liftReaderT $ Left "identifier does not exist in the environment"
                       Lambda i e t -> return (Lambda i e t, p)
                       App t0 t1 -> do
                         (Lambda i e t, p0') <- eval (t0,p)
                         (V t1', p1') <- eval (t1,p)
                         localT (addVar i t1') (eval (t,p))
                       SIG -> liftReaderT $ Left "cannot sign here"
                       V t -> return (V t, p)

                                              
                         

runRead :: ReaderT r m a -> r -> m a
runRead (ReaderT f) e = f e

evaluate :: (APDT,Place) -> Either String (APDT,Place)
evaluate (t,p) = runRead (eval (t,p)) []

------------------------------------------------------------------------------------

data T = SeqE T T |
         ParE T T |
         SigE T Place |
         K Place Place |
         U Place |
         N Place |
         Func T T
       deriving (Show,Eq)

-- type EnvE = [(Id,T)]

-- type Context = [Place]

-- addPlace :: Place -> (Context,EnvE) -> (Context,EnvE)
-- addPlace p (c,e) = (p:c,e)


-- addType :: Id -> T -> (Context,EnvE) -> (Context, EnvE)
-- addType i t (c,e) = (c,(i,t):e)

-- lookupType :: Id -> EnvE -> T
-- lookupType i e = case (lookup i e) of Just x -> x

-- --I think evidence and terms need differentiated
-- --should there be a type error if BR and LN have a combination of evidence and terms
-- --what the what are the type safety properties
-- typeOfTerm :: APDT -> ReaderT (Context,EnvE) (Either String) T
-- typeOfTerm t = case t of Usm p -> return (U p)
--                          Kim q p -> return (K q p)
--                          Nonce p -> return (N p)
--                          USM -> do
--                            (p:xs,e) <- askT
--                            return (U p)
--                          KIM q -> do
--                            (p:xs,e) <- askT
--                            return (K q p)  
--                          AT p t0 -> do
--                            (p0,e) <- askT
--                            localT (addPlace p) (typeOfTerm t0)
--                          LN t0 t1 ->
--                            if (t1==SIG)
--                              then do
--                               e0 <- typeOfTerm t0
--                               (p:xs,e) <- askT
--                               return (SigE e0 p)
--                            else do
--                              e0 <- typeOfTerm t0
--                              e1 <- typeOfTerm t1
--                              return (SeqE e0 e1)           
--                          BR t0 t1 -> do
--                            e0 <- typeOfTerm t0
--                            e1 <- typeOfTerm t1
--                            return (ParE e0 e1)
--                          VAR i -> do
--                            (c,e) <- askT
--                            let m = lookup i e
--                                in case m of Just v -> return v
--                          Lambda i e t -> do
--                            t' <- localT (addType i e) (typeOfTerm t)
--                            return (Func e t')
--                          App t0 t1 -> do
--                            Func d r <- typeOfTerm t0
--                            t1' <- typeOfTerm t1
--                            if (t1'==d) then (return r) else (liftReaderT $ Left "type error in App")
--                          SIG -> liftReaderT $ Left "type error in SIG"
                                  

-- getTypeOf :: APDT -> Either String T
-- getTypeOf t = runRead (typeOfTerm t) ([0],[])

-- ------------------------------------------------------------------------------------





instance Arbitrary APDT where
 arbitrary = sized $ \n -> genAPDT (rem n 10)


--Lambda, VAR, App are not here
genAPDT :: Int -> Gen APDT
genAPDT n = case n of 0 -> do
                        term <- oneof [genKIM, genUSM]
                        return term
                      _ -> do
                        term <- oneof [genLN (n-1), genBR (n-1), genAT (n-1), genSIG (n-1), genKIM, genUSM, genVal (n-1)]
                        return term


  


genKIM = do
  p <- choose (0,100)
  return (KIM p)

genBR n = do
  t0 <- genAPDT n
  t1 <- genAPDT n
  return (BR t0 t1)

genLN n = do
  t0 <- genAPDT n
  t1 <- genAPDT n
  return (LN t0 t1)

genSIG n = do
  t <- genAPDT n
  return (LN t SIG)

genAT n = do
  p <- choose (0,100)
  t <- genAPDT n
  return (AT p t)

genUSM :: Gen APDT
genUSM = do
  return (USM)


--SIG term can only be in the second spot of LN, so what's the scoop on Sig value??

genVal :: Int -> Gen APDT
genVal n = case n of 0 -> do
                       term <- oneof [genKim, genUsm, genMt, genNonce]
                       return term
                     _ -> do
                       term <- oneof [genKim, genUsm, genSeqV (n-1), genParV (n-1), genMt, genNonce]
                       return term


genUsm = do
  p <- choose (0,100)
  return (V (Usm p))

genKim = do
  p0 <- choose (0,100)
  p1 <- choose (0,100)
  return (V (Kim p0 p1))

genSeqV n = do
  V t0 <- genVal n
  V t1 <- genVal n
  return (V (SeqV t0 t1))

genParV n = do
  V t0 <- genVal n
  V t1 <- genVal n
  return (V (ParV t0 t1))

genMt :: Gen APDT
genMt = do
  return (V Mt)

genNonce = do
  p <- choose (0,100)
  return (V (Nonce p))





isRight :: Either String (APDT,Place) -> Bool
isRight x = case x of Right t -> True
                      _ -> False

 
{-


   compare small step and big step using quickcheck

quickCheck (\x -> isRight (evaluate (x,0)))
quickCheckWith stdArgs {maxSuccess=500} (\x -> isRight (evaluate (x,0)))

---

    don't know how to generate APDT with variables

genLambda n = do
  i <- choose ('a','z')
  t <- genAPDT n
  return (App (Lambda i (getTypeOf t) _____ ) t)

---

    causes quickcheck to fail (which it should)

genBAD n = do
  t <- genAPDT n
  return (BR t SIG)

oneof [genLN (n-1), genBR (n-1), genAT (n-1), genSIG (n-1), genKIM, genUSM, genBAD (n-1)]



------------------------------------------------------------------------------------

evaluate (LN (KIM 1) (USM), 0)
  Right (V (SeqV (Kim 1 0) (Usm 0)),0)
evaluate (LN (KIM 1) (SIG), 0)
  Right (V (SeqV (Kim 1 0) (Sig (Kim 1 0) 0)),0)
evaluate (BR USM (KIM 1), 0)
  Right (V (ParV (Usm 0) (Kim 1 0)),0)
evaluate (AT 1 USM, 0)
  Right (V (Usm 1),0)
evaluate (App (Lambda 'y' (K 1 0) (LN (VAR 'y') (USM))) (KIM 1), 0)
  Right (V (SeqV (Kim 1 0) (Usm 0)),0)
evaluate (LN (AT 1 USM) (USM), 0)
  Right (V (SeqV (Usm 1) (Usm 0)),0)
evaluate (LN (BR USM USM) USM,0)
  Right (V (SeqV (ParV (Usm 0) (Usm 0)) (Usm 0)),0)
evaluate (BR (V (Usm 0)) USM, 0)
  Right (V (ParV (Usm 0) (Usm 0)),0)
---
evaluate (App (Lambda 'y' (K 1 0) (LN (VAR 'b') (USM))) (KIM 1), 0)
  Left "identifier does not exist in the environment"
evaluate (BR (KIM 1) (SIG), 0)
  Left "cannot sign here"
evaluate (LN (SIG) (USM), 0)
  Left "cannot sign here"

------------------------------------------------------------------------------------
getTypeOf (LN (AT 1 USM) USM)
  Right SeqE (U 1) (U 0)
getTypeOf (LN (KIM 1) (SIG))
  Right SigE (K 1 0) 0
getTypeOf (App (Lambda 'y' (K 1 0) (LN (VAR 'y') (USM))) (KIM 1))
  Right SeqE (K 1 0) (U 0)
---
getTypeOf (BR USM SIG)
  Left "type error in SIG"
getTypeOf (App (Lambda 'y' (U 0) (BR (VAR 'y') (KIM 1))) (KIM 1))
  Left "type error in App"
-}
