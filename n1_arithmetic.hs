data Term = TmTrue | TmFalse | TmIf Term Term Term | TmZero | TmSucc Term | TmPred Term | TmIsZero Term | NoRuleApplies deriving (Show, Eq)

isNumericVal :: Term -> Bool
isNumericVal t = case (eval t) of TmZero -> True
                                  TmSucc t1 -> if (isNumericVal t1) then True else error "NOT VALID"
                                  _ -> False

isBoolVal :: Term -> Bool
isBoolVal t = case t of TmTrue -> True
                        TmFalse -> True
                        _ -> False
                    
eval1 :: Term -> Term
eval1 t = case t of TmIf TmTrue t2 t3 -> t2
                    TmIf TmFalse t2 t3 -> t3
                    TmIf t1 t2 t3 -> let t1' = eval t1 in (if (isBoolVal t1') then (TmIf t1' t2 t3) else error "NOT VALID")
                    TmPred TmZero -> TmZero
                    TmPred (TmSucc nv1) -> if (isNumericVal nv1) then nv1 else error "NOT VALID"
                    TmPred t1 -> if (isNumericVal t1) then let t1' = eval1 t1 in TmPred t1' else error "NOT VALID"
                    TmIsZero TmZero -> TmTrue
                    TmIsZero (TmSucc nv1) -> if (isNumericVal nv1) then TmFalse else error "NOT VALID"
                    TmIsZero t1 -> let t1' = eval1 t1 in TmIsZero t1'
                    _ -> NoRuleApplies
                    
eval :: Term -> Term
eval t = if(eval1 t == NoRuleApplies) then t else (let t' = eval1 t in eval t')

formatNums :: Num a => Term -> a
formatNums t = case t of TmSucc t1 -> if (isNumericVal t1) then  1 + (formatNums (eval t1)) else error "NOT VALID"
                         TmZero -> 0
                         TmPred t1 -> if (isNumericVal t1) then (formatNums t1) - 1 else error "NOT VALID"
                        
                         
run :: Num a => Term -> Either a Term
run t = if (isNumericVal(eval t)) then Left (formatNums(eval t)) else Right (eval t)

              
                                      

