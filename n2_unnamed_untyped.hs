--data type for unnamed variables
data Term = TmVar Int | TmAbs Term | TmApp Term Term | NoRuleApplies deriving (Show, Eq)

--filters values
isVal :: Term -> Bool
isVal t = case t of TmVar _ -> True
                    TmAbs _ -> True
                    _ -> False
                    
--single step evaluation
eval1 :: Term -> Term
eval1 t = case t of TmApp (TmAbs t12) v2 -> termSubstTop v2 t12
                    TmApp x1 t2 -> if (isVal x1 || (eval1 x1 == NoRuleApplies)) then (if (isVal t2 || (eval1 t2 == NoRuleApplies)) then NoRuleApplies else (let t2' = eval1 t2 in TmApp x1 t2')) else (let x1' = eval1 x1 in TmApp x1' t2)
                    _ -> NoRuleApplies                    

--helper function for termSubst
termShift :: Int -> Int -> Term -> Term
termShift d c t = case t of TmVar k -> if (k>=c) then TmVar(k+d) else TmVar(k)
                            TmAbs t1 -> TmAbs (termShift d (c+1) t1)
                            TmApp t1 t2 -> TmApp (termShift d c t1) (termShift d c t2)
                           
--alpha conversion
termSubst :: Int -> Term -> Term -> Term
termSubst j s t = case t of TmVar k -> if (k==j) then s else (TmVar k)
                            TmAbs t1 -> TmAbs (termSubst (j+1) (termShift 1 0 s) t1)
                            TmApp t1 t2 -> TmApp (termSubst j s t1) (termSubst j s t2)
                            
--beta reduction
termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) 0 (termSubst 0 (termShift 1 0 s) t)

--complete evaluation
eval :: Term -> Term                   
eval t = if (eval1 t == NoRuleApplies) then t else (let t' = eval1 t in eval t')




{-

correct:

   eval (TmApp (TmApp (TmAbs (TmAbs (TmVar 0))) (TmVar 2)) (TmVar 0))
                        => TmVar 0
                                                   
   eval (TmApp (TmAbs (TmAbs (TmVar 0))) (TmAbs (TmAbs (TmVar 0))))
                        => TmAbs (TmVar 0)
                                                   
   eval (TmApp (TmAbs (TmAbs (TmVar 1))) (TmAbs (TmAbs (TmVar 1))))
                        => TmAbs (TmAbs (TmAbs (TmVar 1)))
                                                   
   eval (TmApp (TmAbs (TmAbs (TmVar 1))) (TmAbs (TmVar 0)))
                        => TmAbs (TmAbs (TmVar 0))
                                                   
   eval (TmApp (TmAbs (TmVar 0)) (TmVar 1))
                        => TmVar 1
                                                   
   eval (TmApp (TmAbs (TmApp (TmVar 0) (TmVar 2))) (TmAbs (TmVar 0)))
                        => TmVar 1
                                                   
   eval (TmApp (TmVar 4) (TmVar 3))
                        => TmApp (TmVar 4) (TmVar 3)
                                                   
   eval (TmApp (TmVar 1) (TmAbs (TmVar 0)))
                        => TmApp (TmVar 1) (TmAbs (TmVar 0))
                                                   
   eval (TmApp (TmAbs (TmApp (TmVar 0) (TmVar 4))) (TmAbs (TmVar 0)))
                        => TmVar 3
                                                   
   eval (TmApp (TmApp (TmVar 2) (TmVar 1)) (TmVar 0))
                        => TmApp (TmApp (TmVar 2) (TmVar 1)) (TmVar 0)
                                                   
   eval (TmApp (TmAbs (TmApp (TmVar 0) (TmVar 0))) (TmAbs (TmApp (TmVar 0) (TmVar 0))))
                        => *endless loop*
                                                   
   eval (TmApp (TmAbs (TmApp (TmApp (TmVar 1) (TmVar 0)) (TmVar 2))) (TmAbs (TmVar 0)))
                        => TmApp (TmApp (TmVar 0) (TmAbs (TmVar 0))) (TmVar 1)
                                                   
   eval (TmApp (TmAbs (TmApp (TmVar 0) (TmVar 3))) (TmAbs (TmApp (TmVar 0) (TmVar 2))))
                        => TmApp (TmVar 2) (TmVar 1)
                                                   
   eval (TmApp (TmAbs (TmApp (TmAbs (TmApp (TmApp (TmVar 1) (TmVar 1)) (TmVar 0))) (TmAbs (TmApp (TmVar 3) (TmVar 0))))) (TmAbs (TmAbs (TmVar 0))))
                        => TmAbs (TmApp (TmVar 2) (TmVar 0))
                                                   
   eval (TmApp (TmApp (TmAbs (TmAbs (TmVar 1))) (TmVar 2)) (TmVar 0))
                        => TmVar 2

   eval (TmApp (TmApp (TmAbs (TmVar 0)) (TmAbs (TmApp (TmVar 0) (TmVar 0)))) (TmAbs (TmApp (TmVar 0) (TmVar 1))))
                        => TmApp (TmVar 0) (TmVar 0)

   eval (TmApp (TmApp (TmApp (TmAbs (TmAbs (TmAbs (TmApp (TmApp (TmVar 2) (TmVar 1)) (TmVar 0))))) (TmAbs (TmApp (TmVar 0) (TmVar 0)))) (TmAbs (TmVar 0))) (TmVar 4))
                        => TmVar 4


               
crazy stuff (incorrect):

   eval (TmApp (TmAbs (TmAbs (TmApp (TmApp (TmVar 1) (TmVar 0)) (TmVar 1)))) (TmAbs (TmVar 5)))
               f  TmAbs (TmApp (TmApp (TmAbs (TmVar 4)) (TmVar 0)) (TmAbs (TmVar 6)))
               t  TmVar 4
                     https://www.cs.nmsu.edu/~rth/cs/cs571/LC%20Practice%20Answers.pdf

-}

