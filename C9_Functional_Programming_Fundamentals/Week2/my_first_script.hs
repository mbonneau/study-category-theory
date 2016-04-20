double x = x + x

-- quadruple x = double (double x)
-- quadruple x = double $ double x

quadruple = double . double

factorial n = product [1..n]

average xs = sum xs `div` length xs