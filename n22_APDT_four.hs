import Control.Applicative
import Control.Monad
import Test.QuickCheck
import Data.List

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

--should take the closure out
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

--add comments with source information

data Debruijn = VARD Int |
                LambdaD Debruijn |
                AppD Debruijn Debruijn |
                LND Debruijn Debruijn |
                BRD Debruijn Debruijn |
                ATD Debruijn Debruijn |
                SIGD |
                KIMD Place |
                USMD |
                VD Val
              deriving (Show,Eq)

isEvidD :: Debruijn -> Bool
isEvidD ev = case ev of VD x -> True
                        _ -> False
                        

evalD :: (Debruijn,Place) -> (Debruijn,Place)
evalD a = let (t,p) = (eval1D a) in (if (isEvidD t) then (t,p) else (evalD (t,p)))


eval1D :: (Debruijn,Place) -> (Debruijn,Place)
eval1D (t,p) = case t of AppD (LambdaD t12) (VD v2) -> (termSubstTop (VD v2) t12, p)
                         AppD (LambdaD t12) t2 -> let (t2',p') = eval1D (t2,p) in (AppD (LambdaD t12) t2', p)
                         AppD x1 t2 -> let (x1',p') = eval1D (x1,p) in (AppD x1' t2, p)
                         KIMD q -> (VD (Kim q p), p)
                         BRD (VD x0) (VD x1) -> (VD (ParV x0 x1), p)
                         BRD (VD x0) t1 -> let (t1',p') = (eval1D (t1,p)) in (BRD (VD x0) t1', p)
                         BRD t0 t1 -> let (t0',p') = (eval1D (t0,p)) in (BRD t0' t1, p)
                         USMD -> (VD (Usm p), p)
                         VD v -> (VD v, p)
                         LambdaD t12 -> (LambdaD t12, p)
                         LND (VD x0) (VD x1) -> (VD (SeqV x0 x1), p)
                         LND (VD x0) SIGD -> (VD (SeqV x0 (Sig x0 p)), p)
                         LND (VD x0) t1 -> let (t1',p') = eval1D (t1,p) in (LND (VD x0) t1', p)
                         LND t0 t1 -> let (t0',p') = eval1D (t0,p) in (LND t0' t1, p)
                         ATD q (VD x) -> (VD x, p)
                         ATD (VD (Pl q)) t0 -> let (t0',q') = (eval1D (t0,q)) in (ATD (VD (Pl q)) t0', p)
                         ATD q t0 -> let (r,p') = (eval1D (q,p)) in (ATD r t0, p)
                         

termShift :: Int -> Int -> Debruijn -> Debruijn
termShift d c t = case t of VARD k -> if (k>=c) then VARD(k+d) else VARD(k)
                            LambdaD t1 -> LambdaD (termShift d (c+1) t1)
                            AppD t1 t2 -> AppD (termShift d c t1) (termShift d c t2)
                            x -> x
                            
                           

termSubst :: Int -> Debruijn -> Debruijn -> Debruijn
termSubst j s t = case t of VARD k -> if (k==j) then s else (VARD k)
                            LambdaD t1 -> LambdaD (termSubst (j+1) (termShift 1 0 s) t1)
                            AppD t1 t2 -> AppD (termSubst j s t1) (termSubst j s t2)
                            BRD t1 t2 -> BRD (termSubst j s t1) (termSubst j s t2)
                            LND t1 t2 -> LND (termSubst j s t1) (termSubst j s t2)
                            ATD t1 t2 -> ATD (termSubst j s t1) (termSubst j s t2)
                            x -> x
                            
                            

termSubstTop :: Debruijn -> Debruijn -> Debruijn
termSubstTop s t = termShift (-1) 0 (termSubst 0 (termShift 1 0 s) t)

                

debruijnize t = db (free t) t


db indices t = case t of VAR s -> let Just x = (lookup s indices) in (VARD x)
                         Lambda i e t0 -> let l = map add_one (deleteAssoc (==) i indices) in LambdaD (db ((i,0):l) t0)
                         App t0 t1 -> AppD (db indices t0) (db indices t1)
                         USM -> USMD
                         KIM q -> KIMD q
                         LN t0 t1 -> LND (db indices t0) (db indices t1)
                         BR t0 t1 -> BRD (db indices t0) (db indices t1)
                         V v -> VD v
                         AT q t0 -> ATD  (db indices q) (db indices t0)
                         SIG -> SIGD
                         


generateD :: (Eq a, Num a) => a -> [a]
generateD n = if (n==0) then [] else n:(generateD (n-1))

free :: APDT -> [(Id,Int)]
free t = let f_t = free_vars t in (combine f_t (generateD (length f_t)))


add_one :: Num b => (a,b) -> (a,b)
add_one (s,n) = (s,n+1)


free_vars :: APDT -> [Id]
free_vars t = case t of  VAR s -> [s]
                         Lambda i e t0 ->  filter (\x -> x /= i) (free_vars t0)
                         App t0 t1 -> let f_t0 = free_vars t0 in (let f_t1 = free_vars t1 in (f_t0 ++ (filter (\x -> notElem x f_t0) f_t1)))
                         _ -> []




deleteAssoc :: (a -> a -> Bool) -> a -> [(a,b)] -> [(a,b)]
deleteAssoc f x l = case l of (t,z):xs -> if (f x t) then xs else (t,z):(deleteAssoc f x xs)
                              [] -> []
                          

combine :: [a] -> [b] -> [(a,b)]
combine as bs = case as of x:xs -> case bs of y:ys -> (x,y):(combine xs ys)
                                              [] -> []
                           [] -> []

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


addType :: Id -> T -> (Context,EnvE) -> (Context, EnvE)
addType i t (c,e) = (c,(i,t):e)

--missing several things
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
                         V (Pl p) -> return (PLACE p)
                         
 
                                  

getTypeOf :: APDT -> Either String T
getTypeOf t = runRead (typeOfTerm t) ([0],[])

-- ------------------------------------------------------------------------------------





{-
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
-}


--maybe make it more likely to choose a variable

instance Arbitrary APDT where
 arbitrary = sized $ \n -> genAPDTVAR (rem n 10) []

genKIM = do
  p <- choose (0,100)
  return (KIM p)

genUSM :: Gen APDT
genUSM = do
  return (USM)









genNewChar :: [APDT] -> Gen Char
genNewChar ls = do
  l <- choose ('a','z')
  if (elem (VAR l) ls) then (genNewChar ls) else (return l)

genLambdaApp :: Int -> [APDT] -> Gen APDT
genLambdaApp n ls = do
  l <- genNewChar ls
  t0 <- genAPDTVAR n ((VAR l):ls)
  t1 <- genAPDTVAR n ls
  return (App (Lambda l MEAS t0) t1)

genVAR :: [APDT] -> Gen APDT
genVAR ls = elements ls

genLNVAR :: Int -> [APDT] -> Gen APDT
genLNVAR n ls = do
  t0 <- genAPDTVAR n ls
  t1 <- genAPDTVAR n ls
  return (LN t0 t1)

genBRVAR :: Int -> [APDT] -> Gen APDT
genBRVAR n ls = do
  t0 <- genAPDTVAR n ls
  t1 <- genAPDTVAR n ls
  return (BR t0 t1)

genSIGVAR :: Int -> [APDT] -> Gen APDT
genSIGVAR n ls = do
  t <- genAPDTVAR n ls
  return (LN t SIG)

genAPDTVAR :: Int -> [APDT] -> Gen APDT
genAPDTVAR n ls = case n of 0 -> case ls of [] -> do
                                              term <- oneof [genKIM, genUSM]
                                              return term
                                            _ -> do
                                              --term <- oneof [genVAR ls]
                                              term <- oneof [genKIM, genUSM, genVAR ls]
                                              return term
                            _ -> case ls of [] -> do
                                              --term <- oneof [genLambdaApp (n-1) ls]
                                              term <- oneof [genLambdaApp (n-1) ls, genLNVAR (n-1) ls, genBRVAR (n-1) ls, genSIGVAR (n-1) ls, genUSM, genKIM]
                                              return term
                                            _ -> do
                                              --term <- oneof [genLambdaApp (n-1) ls]
                                              term <- oneof [genLambdaApp (n-1) ls, genLNVAR (n-1) ls, genBRVAR (n-1) ls, genSIGVAR (n-1) ls, genUSM, genKIM, genVAR ls]
                                              return term








vEqVD :: (APDT,Place) -> (Debruijn,Place) -> Bool
vEqVD a d = case a of (V a',pa) -> case d of (VD d',pd) -> if (a'==d') then True else False
                                             _ -> False
                      _ -> False

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

  evaluate (named) and evalD (numbered) are the same
quickCheckWith stdArgs {maxSuccess=5000} (\x -> (vEqVD (removeRight (evaluate (x,0))) (evalD (debruijnize x,0))))

   evaluate (big step) and stepping (small step) are the same
quickCheckWith stdArgs {maxSuccess=5000} (\x -> ((removeRight (evaluate (x,0))) == (stepping (x,0))))

   evaluate results in a value
quickCheckWith stdArgs {maxSuccess=5000} (\x -> (isEvid (removeRightAndPlace (evaluate (x,0)))))

   evaluate does not result in any errors
quickCheckWith stdArgs {maxSuccess=5000} (\x -> isRight (evaluate (x,0)))


---

    causes quickcheck to fail (which it should)
genBAD n = do
  t <- genAPDT n
  return (BR t SIG)
quickCheck (\x -> isRight (evaluate (x,0)))

changing eval so USM generates Kim causes failure


------------------------------------------------------------------------------------
                         
evalD (debruijnize (App (Lambda 'x' MEAS (VAR 'x')) (KIM 1)), 0)
  (VD (Kim 1 0),0)
evalD (debruijnize (App (Lambda 'x' MEAS (BR USM (VAR 'x'))) (KIM 1)), 0)
  (VD (ParV (Usm 0) (Kim 1 0)),0)
evalD (debruijnize (App (Lambda 'x' MEAS (App (Lambda 'y' MEAS (BR (VAR 'y') (VAR 'x'))) (KIM 1))) (USM)), 0)
  (VD (ParV (Kim 1 0) (Usm 0)),0)
evalD (debruijnize (App (Lambda 'x' MEAS (App (Lambda 'y' MEAS (BR (VAR 'y') (VAR 'x'))) (VAR 'x'))) (USM)), 0)
  (VD (ParV (Usm 0) (Usm 0)),0)

------------------------------------------------------------------------------------

evaluate (App (Lambda 'x' MEAS (App (Lambda 'y' MEAS (BR (VAR 'y') (VAR 'x'))) (KIM 1))) (USM),0)
  Right (V (ParV (Kim 1 0) (Usm 0)),0)
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
