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
subst :: String -> Term -> Term -> Term
subst i v t = case t of Num x -> Num x
                        Plus t1 t2 -> Plus (subst i v t1) (subst i v t2)
                        Minus t1 t2 -> Minus (subst i v t1) (subst i v t2)
                        Bind i' v' b' -> if (i == i')
                                         then Bind i' (subst i v v') b'
                                         else Bind i' (subst i v v') (subst i v b')
                        Id i' -> if (i == i') then v else (Id i')
                        App t1 t2 -> App (subst i v t1) (subst i v t2)
                        Lambda i' b' -> Lambda i' (subst i v b')
                        Boolean x -> Boolean x
                        And t1 t2 -> And (subst i v t1) (subst i v t2)
                        Leq t1 t2 -> Leq (subst i v t1) (subst i v t2)
                        IsZero x -> IsZero (subst i v x)
                        If t1 t2 t3 -> If (subst i v t1) (subst i v t2) (subst i v t3)
                        

--single step evaluation
eval1 :: Term -> Term
eval1 t = case t of Num n -> Num n

                    Plus t1 t2 -> let (Num v1) = eval t1
                                      (Num v2) = eval t2
                                  in Num ((+) v1 v2)
                                     
                    Minus t1 t2 -> let (Num v1) = eval t1
                                       (Num v2) = eval t2
                                   in Num ((-) v1 v2)
                                      
                    Bind i v b -> let v' = eval v in (eval (subst i v' b))
                    
                    Lambda i b -> Lambda i b
                    
                    App f a -> case (eval f) of (Lambda i b) -> let a' = eval a in (subst i a' b)
                                                _ -> App f a
                                                
                    Id i -> Id i
                    
                    Boolean b -> Boolean b
                    
                    And t1 t2 -> let (Boolean b1) = eval t1
                                     (Boolean b2) = eval t2
                                 in Boolean ((&&) b1 b2)
                                    
                    Leq t1 t2 -> let (Num v1) = eval t1
                                     (Num v2) = eval t2
                                 in Boolean ((<=) v1 v2)
                                    
                    IsZero t0 -> let (Num v) = eval t0 in Boolean ((==) v 0)
                    
                    If t1 t2 t3 -> let (Boolean b) = eval t1 in (if b then t2 else t3)




                      
--complete evaluation
eval :: Term -> Term
eval t = if (eval1 t == t) then t else (let t' = eval1 t in eval t')


