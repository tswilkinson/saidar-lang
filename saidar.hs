main :: IO ()
main = print "done"

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
                | Lambda Type Expression
                | Application Expression Expression
                | Let Type Expression Expression
                | ForAll Expression

data Type = TypeVariable Natural
          | TypeLambda Type Type
          | TypeApplication Expression Type
          | TypeArrow Type Type

expression_type :: Expression -> Maybe Type
expression_type = go []
  where go :: [Type] -> Expression -> Maybe Type
        go c (Variable n) = deeper c n
          where deeper :: [Type] -> Natural -> Maybe Type
                deeper [] _ = Nothing
                deeper (t:_) Zero = Just t
                deeper (_:ts) (Succ b) = deeper ts b
        go c (Lambda t x) = fmap (TypeArrow t) (go (t:c) x)
        go c (Application x f) = do
          tx <- go c x
          tf <- go c f
          case tf of
            TypeArrow a b -> Nothing
