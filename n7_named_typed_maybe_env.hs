import Data.Maybe
import Control.Monad


--data type for named variables and some simple types
data Term = Num Int |
            Plus Term Term |
            Minus Term Term |
            Bind String Term Term |
            Lambda String Term |
            Id String |
            App Term Term |
            And Term Term |
            Leq Term Term |
            IsZero Term |
            If Term Term Term |
            Boolean Bool
          deriving (Show, Eq)

type Env = [(String,Term)]

--single step evaluation
eval1 :: Env -> Term -> Maybe Term
eval1 env t = case t of Num n -> return (Num n)
                        Plus t1 t2 -> let v1 = evalMonadic env (return t1)
                                          v2 = evalMonadic env (return t2)
                                      in case v1 of Just (Num v1') -> case v2 of Just (Num v2') -> return (Num ((+) v1' v2'))
                                                                                 _ -> Nothing
                                                    _ -> Nothing              
                        Minus t1 t2 -> let v1 = evalMonadic env (return t1)
                                           v2 = evalMonadic env (return t2)
                                       in case v1 of Just (Num v1') -> case v2 of Just (Num v2') -> return (Num ((-) v1' v2'))
                                                                                  _ -> Nothing
                                                     _ -> Nothing
                        Id i -> lookup i env
                        Bind i v b -> let v' = evalMonadic env (return v) in case v' of Just v'' -> evalMonadic ((i,v''):env) (return b)
                                                                                        _ -> Nothing
                        Lambda i b -> return (Lambda i b)
                        App f a -> case (evalMonadic env (return f)) of Just (Lambda i b) -> let a' = evalMonadic env (return a) in case a' of Just a'' -> evalMonadic ((i,a''):env) (return b)
                                                                                                                                               _ -> Nothing
                                                                        _ -> return (App f a)
                        Boolean b -> return (Boolean b)
                        And t1 t2 -> let b1 = evalMonadic env (return t1)
                                         b2 = evalMonadic env (return t2)
                                     in case b1 of Just (Boolean b1') -> case b2 of Just (Boolean b2') -> return (Boolean ((&&) b1' b2'))
                                                                                    _ -> Nothing
                                                   _ -> Nothing
                        Leq t1 t2 -> let v1 = evalMonadic env (return t1)
                                         v2 = evalMonadic env (return t2)
                                     in case v1 of Just (Num v1') -> case v2 of Just (Num v2') -> return (Boolean ((<=) v1' v2'))
                                                                                _ -> Nothing
                                                   _ -> Nothing
                        IsZero t0 -> let v = evalMonadic env (return t0) in case v of Just (Num v') -> return (Boolean ((==) v' 0))
                                                                                      _ -> Nothing
                        If t1 t2 t3 -> let b = evalMonadic env (return t1) in case b of Just (Boolean b') -> return (if b' then t2 else t3)
                                                                                        _ -> Nothing
                        

--initial evaluation
eval :: Term -> Maybe Term
eval t = evalMonadic [("empty",Num 0)] (return t)


--complete evaluation
evalMonadic :: Env -> Maybe Term ->  Maybe Term
evalMonadic env t = if ((t >>= (eval1 env)) == t) then (t) else (let t' = (t >>= (eval1 env)) in (evalMonadic env t'))
