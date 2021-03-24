module Block2.Task1
  ( -- * Types
    ArithmeticError(..)
  , Expr(..)
    -- * Functions
  , eval
  ) where

-- | Section describes priority and associativity of 'Expr'
-- operator-like constructors.
infixl 5 :+:
infixl 5 :-:
infixl 6 :*:
infixl 6 :/:
infixl 7 :^:

-- | 'Expr' data type represents 'Int' expressions
-- supporting the bases described below.
data Expr
  = Val Int        -- ^ Terminal constructor represents 'Int' constant
  | Expr :+: Expr  -- ^ 'Expr' equivalent for summing operator
  | Expr :-: Expr  -- ^ 'Expr' equivalent for subtracting operator
  | Expr :*: Expr  -- ^ 'Expr' equivalent for multiplying operator
  | Expr :/: Expr  -- ^ 'Expr' equivalent for dividing operator
  | Expr :^: Expr  -- ^ 'Expr' equivalent for exponentiation operator
  deriving (Show, Eq)

-- | 'ArithmeticError' data type represents different errors which
-- may be produced during 'Expr' evaluation.
data ArithmeticError
  = DivisionByZero  -- ^ Constructor represents division by zero error
  | NegativePow     -- ^ Constructor represents negative exponentiation error
  deriving (Show, Eq)

-- | Evaluate arithmetic expression and return result as 'Int' or
-- 'ArithmeticError' if evaluation failed.
eval :: Expr -> Either ArithmeticError Int
eval (Val value)    = Right value
eval (left :+: right) = eval left >>= \l -> eval right >>= \r -> Right (l + r)
eval (left :-: right) = eval left >>= \l -> eval right >>= \r -> Right (l - r)
eval (left :*: right) = eval left >>= \l -> eval right >>= \r -> Right (l * r)
eval (left :/: right) = eval left >>= \l ->
  eval right >>= \r -> if r == 0 then Left DivisionByZero else Right (l `div` r)
eval (left :^: right) = eval left >>= \l ->
  eval right >>= \r -> if r < 0 then Left NegativePow else Right (l ^ r)
