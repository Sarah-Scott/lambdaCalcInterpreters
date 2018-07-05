--data type for named variables
data Term = Bind String Term Term |
            Lambda String Term |
            App Term Term |
            Id String
          deriving (Show, Eq)

--substitution operation
subst :: String -> Term -> Term -> Term
subst i v t = case t of Bind i' v' b' -> if (i == i')
                                         then Bind i' (subst i v v') b'
                                         else Bind i' (subst i v v') (subst i v b')
                        Id i' -> if (i == i') then v else (Id i')
                        App t1 t2 -> App (subst i v t1) (subst i v t2)
                        Lambda i' b' -> Lambda i' (subst i v b')

--single step evaluation
eval1 :: Term -> Term
eval1 t = case t of Bind i v b -> let v' = eval v in (eval (subst i v' b))
                    Lambda i b -> Lambda i b
                    App f a -> case (eval f) of (Lambda i b) -> let a' = eval a in (subst i a' b)
                                                _ -> App f a
                    Id i -> Id i 

--complete evaluation
eval :: Term -> Term
eval t = if (eval1 t == t) then t else (let t' = eval1 t in eval t')




{-

 eval (App (App (Lambda "x" (Lambda "y" (Id "y"))) (Id "z")) (Id "b"))
                => Id "b"

 eval (App (Lambda "x" (Lambda "y" (Id "y"))) (Lambda "x" (Lambda "y" (Id "y"))))
                => Lambda "y" (Id "y")

 eval (App (Lambda "x" (Lambda "y" (Id "x"))) (Lambda "x" (Lambda "y" (Id "x"))))
                => Lambda "y" (Lambda "x" (Lambda "y" (Id "x")))

 eval (App (Lambda "x" (Lambda "y" (Id "x"))) (Lambda "z" (Id "z")))
                => Lambda "y" (Lambda "z" (Id "z"))

 eval (App (Lambda "x" (Id "x")) (Id "a"))
                => Id "a"

 eval (App (Lambda "x" (App (Id "x") (Id "a"))) (Lambda "z" (Id "z")))
                => Id "a"

 eval (App (Id "x") (Id "y"))
                => App (Id "x") (Id "y")

 eval (App (Id "a") (Lambda "x" (Id "x")))
                => App (Id "a") (Lambda "x" (Id "x"))

 eval (App (Lambda "x" (App (Id "x") (Id "y"))) (Lambda "z" (Id "z")))
                => Id "y"

 eval (App (App (Id "z") (Id "a")) (Id "b"))
                => App (App (Id "z") (Id "a")) (Id "b")

 eval (App (Lambda "x" (App (Id "x") (Id "x"))) (Lambda "x" (App (Id "x") (Id "x"))))
                => App (Lambda "x" (App (Id "x") (Id "x"))) (Lambda "x" (App (Id "x") (Id "x")))

 eval (App (Lambda "x" (App (App (Id "b") (Id "x")) (Id "a"))) (Lambda "y" (Id "y")))
                => App (App (Id "b") (Lambda "y" (Id "y"))) (Id "a")

 eval (App (Lambda "x" (App (Id "x") (Id "z")))  (Lambda "x" (App (Id "x") (Id "a"))))
                => App (Id "z") (Id "a")

 eval (App (Lambda "x" (App (Lambda "y" (App (App (Id "x") (Id "x")) (Id "y"))) (Lambda "z" (App (Id "a") (Id "z"))))) (Lambda "x" (Lambda "y" (Id "y"))))
                => Lambda "z" (App (Id "a") (Id "z"))

 eval (App (App (Lambda "x" (Lambda "y" (Id "x"))) (Id "z")) (Id "b")) 
                => Id "z"

 eval (App (App (Lambda "x" (Id "x")) (Lambda "y" (App (Id "y") (Id "y")))) (Lambda "z" (App (Id "z") (Id "b"))))
                => App (Id "b") (Id "b")

 eval (App (App (App (Lambda "x" (Lambda "y" (Lambda "z" (App (App (Id "x") (Id "y")) (Id "z"))))) (Lambda "x" (App (Id "x") (Id "x")))) (Lambda "x" (Id "x"))) (Id "x"))
                => Id "x"


-}
