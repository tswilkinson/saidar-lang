main :: IO ()
main = let identity :: Expression
           identity = Lambda (Universe Zero) (Lambda (Variable Zero) (Variable Zero))
--        in print $ simplification_and_type (Lambda (Universe Zero) (Lambda (Variable Zero) (Application (Variable Zero) (Application (Variable (Succ Zero)) identity))))
        in print $ simplification_and_type (Lambda (Universe Zero) (Lambda (Variable Zero) identity))

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

instance Show Expression where
  show (Variable n) = "@" ++ show n
  show (Lambda l r) = "(" ++ show l ++ " |-> " ++ show r ++ ")"
  show (Application l r) = show l ++ " " ++ show r
  show (ArrowType t s) = "(" ++ show t ++ " -> " ++ show s ++ ")"
  show (Universe n) = "U_" ++ show n

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
          further (t:_) Zero = Just (increment t)
          further (_:ts) (Succ n) = further ts n

          increment :: Expression -> Expression
          increment (Variable n) = Variable (Succ n)
          increment (Lambda t y) = Lambda (increment t) (increment y)
          increment (Application x f) = Application (increment x) (increment f)
          increment (ArrowType l r) = ArrowType (increment l) (increment r)
          increment (Universe n) = Universe n

          apply :: Expression -> Natural -> Expression -> Expression
          apply x n (Variable m) = deeper n m
            where deeper :: Natural -> Natural -> Expression
                  deeper Zero Zero = x
                  deeper Zero (Succ b) = Variable b
                  deeper (Succ _) Zero = Variable Zero
                  deeper (Succ a) (Succ b) = deeper a b
          apply x n (Lambda l r) = Lambda (apply x n l) (apply x (Succ n) r)
          apply x n (Application l r) = case apply x n r of
            Lambda _ b -> apply (apply x n l) Zero b
            a -> Application (apply x n l) a
          apply x n (ArrowType a b) = ArrowType (apply x n a) (apply x n b)
          apply _ _ (Universe m) = Universe m
