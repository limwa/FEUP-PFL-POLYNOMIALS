
parser :: [Token] -> Polynomial
parser (t:tk)
    | t == LiteralToken = 