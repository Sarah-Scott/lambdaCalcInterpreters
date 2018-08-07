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

type Place = String

data Term = Var Id |
            Seq Term Term |
            Par Term Term |
            At Place Term |
            SIG |
            KIM Place |
            USM |
            MT |
            Sig Term Place |
            Kim Place Place |
            Usm Place |
            Nonce Place
          deriving (Show, Eq)

type Env = [(Id,Term)] --the term must be evidence

isEvid :: Term -> Bool
isEvid ev = case ev of Seq e0 e1 -> if (isEvid e0 && isEvid e1) then True else False
                       Par e0 e1 -> if (isEvid e0 && isEvid e1) then True else False
                       MT -> True
                       Sig e p -> if (isEvid e) then True else False
                       Kim p0 p1 -> True
                       Usm p -> True
                       Nonce p -> True
                       _ -> False

--is the triangle environment like bind and id??
--how do you add things to the environment??

--where does empty and nonce evidence come in???

--stuff will only be able to added to delta aka triangle after lets or lambdas are in the language

--can SIG only be used in sequential operation?
--does SIG just duplicate the evidence, sign that, and leave the unsigned and signed in sequence??
--can SIG have an At applied to it?

--should parallel have an option or just arbitrarily choose one everytime?

--adding stuff to gamma will come later

--are static semantics the same as typechecking?

--the comma place seems a little repetitive
--the comma place isnt so bad after doing At
eval1 :: (Term,Place) -> (Term,Place)
eval1 (t,p) = case t of KIM q -> (Kim q p, p)
                        USM -> (Usm p, p)
                        Seq t0 t1 -> if (isEvid t0) then
                                        (if (t1==SIG) then
                                           (Seq t0 (Sig t0 p), p)
                                         else (let (t1',p') = eval (t1,p) in (Seq t0 t1', p)))
                                     else (let (t0',p') = eval (t0,p) in (Seq t0' t1, p))
                        Par t0 t1 -> let (t0',p0) = eval (t0,p); (t1',p1) = eval (t1,p)
                                      in (Par t0' t1', p)
                        At q t0 -> let (t0',q') = eval (t0,q) in (t0',p)

eval :: (Term,Place) -> (Term,Place)
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

--places should be numbers

--still have to add Var to eval

--names should match the coq doc

--I think that the type for SIG is wrong in the document
--how does it know/add to the context??
--I'm not sure about At
--At adds to the context
--context starts with home
typeOfTerm :: Term -> Reader Context E
typeOfTerm t = case t of Usm p -> return (U p)
                         Kim q p -> return (K q p)
                         Nonce p -> return (N p)
                         USM -> do
                           p:xs <- ask
                           return (U p)
                         KIM q -> do
                           p:xs <- ask
                           return (K q p)  
                         At p t0 -> do
                           local (addPlace p) (typeOfTerm t0)
                         Seq t0 t1 ->
                           if (t1==SIG)
                             then do
                              e0 <- typeOfTerm t0
                              p:xs <- ask
                              return (SigE e0 p)
                           else do
                             e0 <- typeOfTerm t0
                             e1 <- typeOfTerm t1
                             return (SeqE e0 e1)
                                        
                         Par t0 t1 -> do
                           e0 <- typeOfTerm t0
                           e1 <- typeOfTerm t1
                           return (ParE e0 e1)

--will have home as the number zero
getTypeOf :: Term -> E
getTypeOf t = runR (typeOfTerm t) ["a"]

{-

eval (Seq (KIM "a") (USM), "x")
  (Seq (Kim "a" "x") (Usm "x"),"x")

eval (Seq (KIM "a") (SIG), "x")
  (Seq (Kim "a" "x") (Sig (Kim "a" "x") "x"),"x")

eval (Par USM (KIM "a"), "x")
  (Par (Usm "x") (Kim "a" "x"),"x")

eval (At "b" USM, "x")
  (Usm "b","x")

-----------------------------------------------------------------

getTypeOf (Seq (At "m" USM) USM)
  SeqE (U "m") (U "a")

-}
