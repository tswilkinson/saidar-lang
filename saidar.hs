main :: IO ()
main = print "Hello world!"

data Natural = Zero | Succ Natural

instance Eq Natural where
  Zero == Zero = True
  (Succ n) == (Succ m) = n == m
  _ == _ = False

instance Ord Natural where
  Zero <= _ = True
  (Succ _) <= Zero = False
  (Succ n) <= (Succ m) = n <= m

data Expression = Variable Natural
                | Lambda Expression Expression
                | Application Expression Expression
                | ArrowType Expression Expression
                | Universe Natural

instance Eq Expression where
  (Variable n) == (Variable m) = n == m
  (Lambda l r) == (Lambda s t) = l == s && r == t
  (Application l r) == (Application x f) = l == x && r == f
  (ArrowType t s) == (ArrowType x y) = t == x && s == y
  (Universe n) == (Universe m) = n == m
  _ == _ = False

simplification_and_type :: Expression -> Maybe (Expression,Expression)
simplification_and_type = go []
    where go :: [Expression] -> Expression -> Maybe (Expression,Expression)
          go c (Variable n) = do
            t <- further c n
            return (Variable n,t)
          go c (Lambda t y) = do
            (t',k) <- go c t
            case k of
              (Universe _) -> do
                (y',ty) <- go (t':c) y
                return (Lambda t' y',ArrowType t' ty)
              _ -> Nothing
          go c (Application x f) = do
            (x',tx) <- go c x
            (f',tf) <- go c f
            case (f',tf) of
              (Lambda l r,ArrowType _ tr) -> if tx == l then Just (apply x' Zero r,apply x' Zero tr)
                                                        else Nothing
              _ -> Nothing
          go c (ArrowType l r) = do
            (l',tl) <- go c l
            case tl of
              Universe n -> do
                (r',tr) <- go (tl:c) r
                case tr of
                  Universe m -> Just (ArrowType l' r',Universe (max n m))
                  _ -> Nothing
              _ -> Nothing
          go _ (Universe n) = Just (Universe n,Universe (Succ n))

          further :: [Expression] -> Natural -> Maybe Expression
          further [] _ = Nothing
          further (t:_) Zero = Just t
          further (_:ts) (Succ n) = further ts n

          apply :: Expression -> Natural -> Expression -> Expression
          apply x n (Variable m) = if n == m then x else Variable m
          apply x n (Lambda l r) = Lambda (apply x n l) (apply x (Succ n) r)
          apply x n (Application l r) = case apply x (Succ n) r of
            Lambda t x -> Universe Zero
