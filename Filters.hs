import Types

filterPolynomial :: Polynomial -> Polynomial
filterPolynomial (Polynomial terms) = Polynomial (sanitizeTerms terms)
    where sanitizeTerms = filterTerms . map filterTermPowers
          

filterTerm :: Term -> Bool
filterTerm (Term coef _) | coef == 0 = False
                         | otherwise = True

filterTerms :: [Term] -> [Term]
filterTerms = filter filterTerm

filterPower :: Power -> Bool
filterPower (Power _ exp) | exp == 0 = False
                          | otherwise = True

filterPowers :: [Power] -> [Power]
filterPowers = filter filterPower

filterTermPowers :: Term -> Term
filterTermPowers (Term coef powers) = Term coef (filterPowers powers) 

-- main = print (filterPolynomial (Polynomial [ Term 0 [ Power 'x' 3], Term 5 [ Power 'y' 2, Power 'x' 0 ], Term 4 [ Power 'x' 3 ] ]))