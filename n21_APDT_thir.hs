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
            LN APDT APDT |
            BR APDT APDT |
            AT APDT APDT |
            SIG |
            KIM Place |
            USM |
            Lambda Id T APDT |
            App APDT APDT |
            V Val
          deriving (Show, Eq)

--added a closure (might be bad) ??????
data Val =  Mt |
            Sig Val Place |
            Kim Place Place |
            Usm Place |
            Nonce Place |
            SeqV Val Val |
            ParV Val Val |
            Pl Place |
            ClosureV Id APDT Env
         deriving (Show,Eq)

type Env = [(Id,Val)] 

isEvid :: APDT -> Bool
isEvid ev = case ev of V x -> True
                       _ -> False

getVal :: APDT -> Val
getVal t = case t of V v -> v

addVar :: Id -> Val -> Env -> Env
addVar i t env = (i,t):env


--change SIG (maybe if I feel like it)
--quickcheck generate lambdas within lambdas (list of identifiers that could be used maybe)
--debruijn converter needed (impossible)
--quickcheck the old debruijn indices
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
                         (V (Pl r),p') <- eval (q,p)
                         (t0',r') <- eval (t0,r)
                         return (t0',p)
                       VAR i -> do
                         env <- askT
                         let m = lookup i env
                           in case m of Just v -> (return (V v,p))    
                                        _ -> liftReaderT $ Left "identifier does not exist in the environment"
                       Lambda i e t0 -> do
                         env <- askT
                         return (V (ClosureV i t0 env), p)
                       App t0 t1 -> do
                         (V (ClosureV i t0 e), p0') <- eval (t0,p)
                         (V t1', p1') <- eval (t1,p)
                         localT (useClosure i t1' e) (eval (t0, p))
                       SIG -> liftReaderT $ Left "cannot sign here"
                       V x -> return (V x, p)

                                              
useClosure :: Id -> Val -> Env -> Env -> Env
useClosure i t e _ = (i,t):e

runRead :: ReaderT r m a -> r -> m a
runRead (ReaderT f) e = f e

evaluate :: (APDT,Place) -> Either String (APDT,Place)
evaluate (t,p) = runRead (eval (t,p)) []

------------------------------------------------------------------------------------

stepping :: (APDT,Place) -> (APDT,Place)
stepping a = let (t,p) = (step a) in (if (isEvid t) then (t,p) else (stepping (t,p)))

subst :: Id -> APDT -> APDT -> APDT
subst i v t = case t of V x -> V x
                        LN t0 t1 -> LN (subst i v t0) (subst i v t1)
                        USM -> USM
                        VAR i' -> if (i==i') then v else (VAR i')
                        App t0 t1 -> App (subst i v t0) (subst i v t1)
                        Lambda i' e t0 -> Lambda i' e (subst i v t0)
                        KIM q -> KIM q
                        BR t0 t1 -> BR (subst i v t0) (subst i v t1)
                        AT q t0 -> AT (subst i v q) (subst i v t0)
                        SIG -> SIG

step :: (APDT,Place) -> (APDT,Place)
step (t,p) = case t of Lambda i e t0 -> (Lambda i e t0,p)
                       App (Lambda i e t0) (V t1) -> (subst i (V t1) t0, p)
                       App (Lambda i e t0) t1 -> let (t1',p') = step (t1,p) in (App (Lambda i e t0) t1', p)
                       VAR i -> (VAR i, p)
                       USM -> (V (Usm p), p)
                       V x -> (V x, p)
                       LN (V x0) (V x1) -> (V (SeqV x0 x1), p)
                       LN (V x0) SIG -> (V (SeqV x0 (Sig x0 p)), p)
                       LN (V x0) t1 -> let (t1',p') = step (t1,p) in (LN (V x0) t1', p)
                       LN t0 t1 -> let (t0',p') = step (t0,p) in (LN t0' t1, p)
                       KIM q -> (V (Kim q p), p)
                       BR (V x0) (V x1) -> (V (ParV x0 x1), p)
                       BR (V x0) t1 -> let (t1',p') = (step (t1,p)) in (BR (V x0) t1', p)
                       BR t0 t1 -> let (t0',p') = (step (t0,p)) in (BR t0' t1, p)
                       AT q (V x) -> (V x, p)
                       AT (V (Pl q)) t0 -> let (t0',q') = (step (t0,q)) in (AT (V (Pl q)) t0', p)
                       AT q t0 -> let (r,p') = (step (q,p)) in (AT r t0, p)



------------------------------------------------------------------------------------

data T = MEAS |
         PLACE Place |
         Func T T
       deriving (Show,Eq)

type EnvE = [(Id,T)]

type Context = [Place]

-- addPlace :: Place -> (Context,EnvE) -> (Context,EnvE)
-- addPlace p (c,e) = (p:c,e)


addType :: Id -> T -> (Context,EnvE) -> (Context, EnvE)
addType i t (c,e) = (c,(i,t):e)

-- lookupType :: Id -> EnvE -> T
-- lookupType i e = case (lookup i e) of Just x -> x


--needs values and at
typeOfTerm :: APDT -> ReaderT (Context,EnvE) (Either String) T
typeOfTerm t = case t of USM -> return MEAS
                         KIM q -> return MEAS
                         VAR i -> do
                           (c,e) <- askT
                           let m = lookup i e
                               in case m of Just v -> return v
                         Lambda i e t0 -> do
                           t' <- localT (addType i e) (typeOfTerm t0)
                           return (Func e t')
                         App t0 t1 -> do
                           Func d r <- typeOfTerm t0
                           t1' <- typeOfTerm t1
                           if (t1'==d) then (return r) else (liftReaderT $ Left "type error in App")
 
                                  

getTypeOf :: APDT -> Either String T
getTypeOf t = runRead (typeOfTerm t) ([0],[])

-- ------------------------------------------------------------------------------------


--instance Arbitrary APDT where
-- arbitrary = sized $ \n -> genLambdaApp (rem n 10)



instance Arbitrary APDT where
 arbitrary = sized $ \n -> genAPDT (rem n 10)


genAPDT :: Int -> Gen APDT
genAPDT n = case n of 0 -> do
                        term <- oneof [genKIM, genUSM]
                        return term
                      _ -> do
                        term <- oneof [genLN (n-1), genBR (n-1), genAT (n-1), genSIG (n-1), genKIM, genUSM, genVal (n-1), genLambdaApp (n-1)]
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
  return (AT (V (Pl p)) t)

genUSM :: Gen APDT
genUSM = do
  return (USM)





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




genLambdaApp n = do
  i <- choose ('a','z')
  t0 <- genAPDTVAR n i
  t1 <- genAPDT n
  return (App (Lambda i MEAS t0) t1)

genVAR :: Char -> Gen APDT
genVAR i = return (VAR i)

genLNVAR n i = do
  t0 <- genAPDTVAR n i
  t1 <- genAPDTVAR n i
  return (LN t0 t1)

genBRVAR n i = do
  t0 <- genAPDTVAR n i
  t1 <- genAPDTVAR n i
  return (BR t0 t1)


genSIGVAR n i = do
  t <- genAPDTVAR n i
  return (LN t SIG)

genAPDTVAR :: Int -> Char -> Gen APDT
genAPDTVAR n i = case n of 0 -> do
                             term <- oneof [genKIM, genUSM, genVAR i]
                             return term
                           _ -> do
                             term <- oneof [genLNVAR (n-1) i, genBRVAR (n-1) i, genSIGVAR (n-1) i, genVAR i, genKIM, genUSM]
                             return term



removeRight :: Either String (APDT,Place) -> (APDT,Place)
removeRight x = case x of Right t -> t
                          _ -> error "you got a left in here"

removeRightAndPlace :: Either String (APDT,Place) -> APDT
removeRightAndPlace x = case x of Right (t,p) -> t
                                  _ -> error "you got a left in here"

isRight :: Either String (APDT,Place) -> Bool
isRight x = case x of Right t -> True
                      _ -> False

 
{-




   evaluate (big step) and stepping (small step) are the same
quickCheck (\x -> ((removeRight (evaluate (x,0))) == (stepping (x,0))))
quickCheckWith stdArgs {maxSuccess=5000} (\x -> ((removeRight (evaluate (x,0))) == (stepping (x,0))))

   evaluate results in a value
quickCheck (\x -> (isEvid (removeRightAndPlace (evaluate (x,0)))))
quickCheckWith stdArgs {maxSuccess=5000} (\x -> (isEvid (removeRightAndPlace (evaluate (x,0)))))

   evaluate does not result in any errors
quickCheck (\x -> isRight (evaluate (x,0)))
quickCheckWith stdArgs {maxSuccess=5000} (\x -> isRight (evaluate (x,0)))


---

    causes quickcheck to fail (which it should)
genBAD n = do
  t <- genAPDT n
  return (BR t SIG)
quickCheck (\x -> isRight (evaluate (x,0)))


------------------------------------------------------------------------------------

evaluate (LN (KIM 1) (USM), 0)
  Right (V (SeqV (Kim 1 0) (Usm 0)),0)
evaluate (LN (KIM 1) (SIG), 0)
  Right (V (SeqV (Kim 1 0) (Sig (Kim 1 0) 0)),0)
evaluate (BR USM (KIM 1), 0)
  Right (V (ParV (Usm 0) (Kim 1 0)),0)
evaluate (AT 1 USM, 0)
  Right (V (Usm 1),0)
evaluate (App (Lambda 'y' (MEAS) (LN (VAR 'y') (USM))) (KIM 1), 0)
  Right (V (SeqV (Kim 1 0) (Usm 0)),0)
evaluate (LN (AT 1 USM) (USM), 0)
  Right (V (SeqV (Usm 1) (Usm 0)),0)
evaluate (LN (BR USM USM) USM,0)
  Right (V (SeqV (ParV (Usm 0) (Usm 0)) (Usm 0)),0)
evaluate (BR (V (Usm 0)) USM, 0)
  Right (V (ParV (Usm 0) (Usm 0)),0)
evaluate (App (Lambda 'y' MEAS (App (Lambda 'x' MEAS (LN (VAR 'x') (VAR 'y'))) (BR USM (KIM 1)))) (KIM 1),0)
  Right (V (SeqV (ParV (Usm 0) (Kim 1 0)) (Kim 1 0)),0)
evaluate (App (Lambda 'y' MEAS (App (Lambda 'x' MEAS (LN (VAR 'x') (VAR 'y'))) (USM))) (KIM 1),0)
  Right (V (SeqV (Usm 0) (Kim 1 0)),0)
---
evaluate (App (Lambda 'y' (MEAS) (LN (VAR 'b') (USM))) (KIM 1), 0)
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
