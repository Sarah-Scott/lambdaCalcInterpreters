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

--can SIG only be used in sequential operation?
--does SIG just duplicate the evidence, sign that, and leave the unsigned and signed in sequence??
--can SIG have an At applied to it?

--should parallel have an option or just arbitrarily choose one everytime?

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


{-

eval (Seq (KIM "a") (USM), "x")
  (Seq (Kim "a" "x") (Usm "x"),"x")

eval (Seq (KIM "a") (SIG), "x")
  (Seq (Kim "a" "x") (Sig (Kim "a" "x") "x"),"x")

eval (Par USM (KIM "a"), "x")
  (Par (Usm "x") (Kim "a" "x"),"x")

eval (At "b" USM, "x")
  (Usm "b","x")

-}
