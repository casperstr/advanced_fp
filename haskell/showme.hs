
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
  show Add = "add"
  show Sub = "sub"
  show Dot = "dot"
