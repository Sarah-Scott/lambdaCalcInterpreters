import Control.Applicative
import Control.Monad

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
            Nonce Place
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

--bindings will only be able to added to delta after lets or lambdas are in the language


--comma place will be necessary later on
eval1 :: (APDT,Place) -> (APDT,Place)
eval1 (t,p) = case t of KIM q -> (Kim q p, p)
                        USM -> (Usm p, p)
                        LN t0 t1 -> if (isEvid t0) then
                                        (if (t1==SIG) then
                                           (LN t0 (Sig t0 p), p)
                                         else (let (t1',p') = eval (t1,p) in (LN t0 t1', p)))
                                     else (let (t0',p') = eval (t0,p) in (LN t0' t1, p))
                        BR t0 t1 -> let (t0',p0) = eval (t0,p); (t1',p1) = eval (t1,p)
                                      in (BR t0' t1', p)
                        AT q t0 -> let (t0',q') = eval (t0,q) in (t0',p)

eval :: (APDT,Place) -> (APDT,Place)
eval (t,p) = if (isEvid t) then (t,p) else (let (t',p') = eval1 (t,p) in eval (t',p'))

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



--still have to add Var to eval

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

--will have home as the number zero
getTypeOf :: APDT -> E
getTypeOf t = runR (typeOfTerm t) [0]

{-

eval (LN (KIM 1) (USM), 0)
  (LN (Kim 1 0) (Usm 0),0)

eval (LN (KIM 1) (SIG), 0)
  (LN (Kim 1 0) (Sig (Kim 1 0) 0),0)

eval (BR USM (KIM 1), 0)
  (BR (Usm 0) (Kim 1 0),0)

eval (AT 1 USM, 0)
  (Usm 1,0)

-----------------------------------------------------------------

getTypeOf (LN (AT 1 USM) USM)
  SeqE (U 1) (U 0)

-}
