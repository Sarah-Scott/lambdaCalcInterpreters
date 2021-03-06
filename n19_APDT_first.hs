import Control.Applicative
import Control.Monad

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

lookupName :: Id -> Env -> APDT
lookupName i e = case (lookup i e) of Just x -> x

type Id = String

type Place = Int

data APDT = VAR Id |
            LN APDT APDT | --Linear (Sequential)
            BR APDT APDT | --Branch (Parallel)
            AT Place APDT |
            SIG |
            KIM Place |
            USM |
            Mt |
            Sig APDT Place |
            Kim Place Place |
            Usm Place |
            Nonce Place |
            Lambda Id E APDT |
            App APDT APDT
          deriving (Show, Eq)

type Env = [(Id,APDT)] --the term must be evidence

isEvid :: APDT -> Bool
isEvid ev = case ev of LN e0 e1 -> if (isEvid e0 && isEvid e1) then True else False
                       BR e0 e1 -> if (isEvid e0 && isEvid e1) then True else False
                       Mt -> True
                       Sig e p -> if (isEvid e) then True else False
                       Kim p0 p1 -> True
                       Usm p -> True
                       Nonce p -> True
                       _ -> False

addVar :: Id -> APDT -> Env -> Env
addVar i t env = (i,t):env

--what would be a good example use of the lambda
--adding the type for what the variable should be seems like too much work

--comma place will be necessary later on
eval :: (APDT,Place) -> Reader Env (APDT,Place)
eval (t,p) = case t of KIM q -> return (Kim q p, p)
                       USM -> return (Usm p, p)
                       LN t0 t1 ->
                         if (isEvid t0)
                         then (if (t1==SIG)
                                then return (LN t0 (Sig t0 p), p)
                               else do
                                  (t1',p') <- eval (t1,p)
                                  return (LN t0 t1', p))
                         else do
                           (t0',p') <- eval (t0,p)
                           eval (LN t0' t1, p)                     
                       BR t0 t1 -> do
                         (t0',p0) <- eval (t0,p)
                         (t1',p1) <- eval (t1,p)
                         return (BR t0' t1', p)
                       AT q t0 -> do
                         (t0',q') <- eval (t0,q)
                         return (t0',p)
                       VAR i -> do
                         env <- ask
                         return (lookupName i env, p)
                       Lambda i e t -> return (Lambda i e t, p)
                       App t0 t1 -> do
                         (Lambda i e t, p0') <- eval (t0,p)
                         (t1', p1') <- eval (t1,p)
                         local (addVar i t1') (eval (t,p))
                         
evaluate :: (APDT,Place) -> (APDT,Place)
evaluate (t,p) = runR (eval (t,p)) []

------------------------------------------------------------------------------------

data E = SeqE E E |
         ParE E E |
         SigE E Place |
         K Place Place |
         U Place |
         N Place
       deriving (Show,Eq)

type Context = [Place]

addPlace :: Place -> Context -> Context
addPlace p c = p:c


typeOfTerm :: APDT -> Reader Context E
typeOfTerm t = case t of Usm p -> return (U p)
                         Kim q p -> return (K q p)
                         Nonce p -> return (N p)
                         USM -> do
                           p:xs <- ask
                           return (U p)
                         KIM q -> do
                           p:xs <- ask
                           return (K q p)  
                         AT p t0 -> do
                           local (addPlace p) (typeOfTerm t0)
                         LN t0 t1 ->
                           if (t1==SIG)
                             then do
                              e0 <- typeOfTerm t0
                              p:xs <- ask
                              return (SigE e0 p)
                           else do
                             e0 <- typeOfTerm t0
                             e1 <- typeOfTerm t1
                             return (SeqE e0 e1)         
                         BR t0 t1 -> do
                           e0 <- typeOfTerm t0
                           e1 <- typeOfTerm t1
                           return (ParE e0 e1)

--home is the number zero
getTypeOf :: APDT -> E
getTypeOf t = runR (typeOfTerm t) [0]

------------------------------------------------------------------------------------

{-

evaluate (LN (KIM 1) (USM), 0)
  (LN (Kim 1 0) (Usm 0),0)

evaluate (LN (KIM 1) (SIG), 0)
  (LN (Kim 1 0) (Sig (Kim 1 0) 0),0)

evaluate (BR USM (KIM 1), 0)
  (BR (Usm 0) (Kim 1 0),0)

evaluate (AT 1 USM, 0)
  (Usm 1,0)

evaluate (App (Lambda "y" (K 1 0) (LN (VAR "y") (USM))) (KIM 1), 0)
  (LN (Kim 1 0) (Usm 0),0)
-}
