main :: IO ()
main = print "hello world"

data Natural = Zero | Succ Natural

instance Eq Natural where
  Zero == Zero = True
  (Succ n) == (Succ m) = n == m
  _ == _ = False

instance Ord Natural where
  Zero <= _ = True
  (Succ _) <= Zero = False
  (Succ n) <= (Succ m) = n <= m

instance Show Natural where
  show Zero = "0"
  show (Succ n) = show n ++ "+"

data Expression = Variable Natural
                | Lambda Expression
                | Application Expression Expression
                | Let Expression [(Pattern,Expression)] Expression
                | ArrowType Expression Expression
                | Sigma Expression Expression
                | Pair Expression Expression Expression

data Pattern
