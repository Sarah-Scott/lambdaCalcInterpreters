data Term = Num Int |
            Plus Term Term |
            Minus Term Term |
            Bind String Term Term |
            Lambda String TermType Term |
            App Term Term |
            Id String |
            Boolean Bool |
            And Term Term |
            Leq Term Term |
            IsZero Term
          deriving (Show, Eq)

data TermType = TNum |
                TBool |
                TFunc TermType TermType
              deriving (Show, Eq)

type Context = [(String,TermType)]

typeOfTerm :: Context -> Term -> (Maybe TermType)
typeOfTerm cont t = case t of Num n -> return TNum
                              Plus t1 t2 -> do
                                TNum <- (typeOfTerm cont t1)
                                TNum <- (typeOfTerm cont t2)
                                return TNum
                              Minus t1 t2 -> do
                                TNum <- (typeOfTerm cont t1)
                                TNum <- (typeOfTerm cont t2)
                                return TNum
                              Bind i v b -> do
                                v' <- (typeOfTerm cont v)
                                typeOfTerm ((i,v'):cont) b
                              Id i -> lookup i cont
                              Lambda i t b -> do
                                bType <- typeOfTerm ((i,t):cont) b
                                return (TFunc t bType)
                              App f a -> do
                                TFunc domainType rangeType <- typeOfTerm cont f
                                aType <- typeOfTerm cont a
                                if (domainType == aType)
                                  then (return rangeType)
                                  else Nothing
                              Boolean b -> return TBool
                              And t1 t2 -> do
                                TBool <- (typeOfTerm cont t1)
                                TBool <- (typeOfTerm cont t2)
                                return TBool
                              Leq t1 t2 -> do
                                TNum <- (typeOfTerm cont t1)
                                TNum <- (typeOfTerm cont t2)
                                return TBool
                              IsZero n -> do
                                TNum <- (typeOfTerm cont n)
                                return TBool

