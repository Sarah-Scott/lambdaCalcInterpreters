import Data.Either
import Control.Monad


--data type for named variables and some simple types
data Term = Num Int |
            Plus Term Term |
            Minus Term Term |
            Boolean Bool |
            And Term Term |
            Leq Term Term |
            IsZero Term |
            If Term Term Term |
            Bind String Term Term |
            Lambda String Term |
            App Term Term |
            Id String
          deriving (Show, Eq)

--substitution operation
subst :: String -> Either String Term -> Term -> Term
subst i v t = case t of Num x -> Num x
                        Plus t1 t2 -> Plus (subst i v t1) (subst i v t2)
                        Minus t1 t2 -> Minus (subst i v t1) (subst i v t2)
                        Id i' -> if (i == i')
                                 then case v of Right t -> t
                                 else (Id i')
                        App t1 t2 -> App (subst i v t1) (subst i v t2)
                        Lambda i' b' -> Lambda i' (subst i v b')
                        Boolean x -> Boolean x
                        And t1 t2 -> And (subst i v t1) (subst i v t2)
                        Leq t1 t2 -> Leq (subst i v t1) (subst i v t2)
                        IsZero x -> IsZero (subst i v x)
                        If t1 t2 t3 -> If (subst i v t1) (subst i v t2) (subst i v t3)

--single step evaluation
eval1 :: Term -> Either String Term
eval1 t = case t of Num n -> return (Num n)
                    Plus t1 t2 -> let v1 = evalMonadic (return t1)
                                      v2 = evalMonadic (return t2)
                                  in case v1 of Right (Num v1') -> case v2 of Right (Num v2') -> return (Num ((+) v1' v2'))
                                                                              _ -> Left "Naw fam"
                                                _ -> Left "Naw fam"
                    Boolean b -> return (Boolean b)                
                    Minus t1 t2 -> let v1 = evalMonadic (return t1)
                                       v2 = evalMonadic (return t2)
                                   in case v1 of Right (Num v1') -> case v2 of Right (Num v2') -> return (Num ((-) v1' v2'))
                                                                               _ -> Left "Naw fam"
                                                 _ -> Left "Naw fam"
                    And t1 t2 -> let b1 = eval t1
                                     b2 = eval t2
                                 in case b1 of Right (Boolean b1') -> case b2 of Right (Boolean b2') -> return (Boolean ((&&) b1' b2'))
                                                                                 _ -> Left "Naw fam"
                                               _ -> Left "Naw fam"
                    Leq t1 t2 -> let v1 = evalMonadic (return t1)
                                     v2 = evalMonadic (return t2)
                                 in case v1 of Right (Num v1') -> case v2 of Right (Num v2') -> return (Boolean ((<=) v1' v2'))
                                                                             _ -> Left "Naw fam"
                                               _ -> Left "Naw fam"
                    IsZero t0 -> let v = evalMonadic (return t0) in case v of Right (Num v') -> return (Boolean ((==) v' 0))
                                                                              _ -> Left "Naw fam"
                    If t1 t2 t3 -> let b = evalMonadic (return t1) in case b of Right (Boolean b') -> return (if b' then t2 else t3)
                                                                                _ -> Left "Naw fam"
                    Id i -> return (Id i)
                    Lambda i b -> return (Lambda i b)
                    App f a -> case (evalMonadic (return f)) of Right (Lambda i b) -> let a' = evalMonadic (return a) in return (subst i a' b)
                                                                _ -> return (App f a)
                      
--initial evaluation
eval :: Term -> Either String Term
eval t = evalMonadic (return t)

--complete evaluation
evalMonadic :: Either String Term ->  Either String Term
evalMonadic t = if ((t >>= eval1) == t) then (t) else (let t' = (t >>= eval1) in evalMonadic t')

