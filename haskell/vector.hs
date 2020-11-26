module Vector where

type Vector = [Integer]

data Expr
  = V Vector
  | VO VectorOp Expr Expr
  | SO ScalarOp IntExpr Expr

data IntExpr
  = I Integer
  | NO NormOp Expr

data VectorOp = Add | Sub | Dot

data ScalarOp = Mul | Div

data NormOp = NormOne | NormInf

instance Show Expr where
  show (V v) = show v
  show (VO op l r) =  "{" ++ show op ++ "," ++ show l ++ "," ++ show r ++ "}"
  show (SO op l r) =  "{" ++ show op ++ "," ++ show l ++ "," ++ show r ++ "}"


instance Show IntExpr where
  show (I num) = show num
  show (NO op exp) = "{" ++ show op ++ "," ++ show exp ++ "}"

instance Show NormOp where
  show NormOne = "norm_one"
  show NormInf = "norm_inf"

instance Show ScalarOp where
  show Mul = "mul"
  show Div = "div"

instance Show VectorOp where
  show Add = "'add"
  show Sub = "sub"
  show Dot = "dot"
