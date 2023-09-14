{-# LANGUAGE FlexibleInstances #-}

module PE3 where

import Data.List (sort, sortBy)
import Text.Printf (printf)

data Term = Const Integer | Pw Integer Power | Trig Integer Power Trigonometric | Exp Integer Power Exponential

data Power = Power Integer
data Polynomial = Polynomial [(Integer, Power)]
data Exponential = Exponential Polynomial
data Trigonometric = Sin Polynomial | Cos Polynomial

class Evaluable a where
    function :: a -> (Integer -> Double)

class Differentiable a where
    derivative :: a -> [Term]

-- You can use this as is
getRounded :: Double -> Double 
getRounded x = read s :: Double
               where s = printf "%.2f" x

-- You don't have to follow the order the functions appear in the file
-- For example, you could first define all Show instances, then all Eq instances etc.
-- if that implementation order is more convenient for you.



-- INSTANCES FOR POWER
instance Show Power where
    show (Power 0) = "1"
    show (Power 1) = "x"
    show (Power n) = "x^" ++ show n
instance Eq Power where
    (Power a) == (Power b) = a == b
instance Ord Power where
    compare (Power a) (Power b) = compare a b

instance Evaluable Power where
    function (Power n) = pev 
        where pev x= fromIntegral x ** fromIntegral n

instance Differentiable Power where
    derivative (Power 0) = []
    derivative (Power n) = [Pw (n) (Power (n - 1))]



-- INSTANCES FOR POLYNOMIAL
instance Show Polynomial where
  show (Polynomial []) = ""
  show (Polynomial [t]) = showTerm t
    where
      showTerm (c, Power n)
        | n == 0    = show c
        | c == 1    = show (Power n)
        | c == -1   = "-" ++ show (Power n)
        | otherwise = show c ++ show (Power n)
  show (Polynomial (t:rest)) = showTerm t ++ showTerms rest
    where
      showTerm (c, Power n)
        | n == 0    = show c
        | c == 1    = show (Power n)
        | c == -1   = "-" ++ show (Power n)
        | otherwise = show c ++ show (Power n)
      showTerms [] = ""
      showTerms ((c, Power n):rest)
        | c < 0     = " - " ++ showTerm (negate c, Power n) ++ showTerms rest
        | otherwise = " + " ++ showTerm (c, Power n) ++ showTerms rest

instance Evaluable Polynomial where
    function (Polynomial terms) = evalPoly terms
        where
            evalPoly ts x = sum (map (termFunction x) ts)
            termFunction x (c, p) = fromIntegral c * function p x


instance Differentiable Polynomial where
    derivative (Polynomial terms) = map termDerivative terms
        where
            termDerivative (c, p) = Pw (c * fromIntegral (getPower p)) (Power (getPower p - 1))
            getPower (Power n) = n


-- INSTANCES FOR TRIGONOMETRIC
instance Show Trigonometric where
    show (Sin poly) =
        case poly of
            Polynomial [(_, Power 1)] -> "sin" ++ show poly
            _                         -> "sin(" ++ show poly ++ ")"
    show (Cos poly) =
        case poly of
            Polynomial [(_, Power 1)] -> "cos" ++ show poly
            _                         -> "cos(" ++ show poly ++ ")"

instance Evaluable Trigonometric where
    function trig = evalTrig trig
        where
            evalTrig (Sin p) = sinFunction p
            evalTrig (Cos p) = cosFunction p
            sinFunction p x = getRounded (sin (function p x))
            cosFunction p x = getRounded (cos (function p x))

instance Differentiable Trigonometric where
    derivative (Sin p) = [Trig 3 (Power 0) (Cos p)]
    derivative (Cos p) = [Trig (-1) (Power 0) (Sin p)]



-- INSTANCES FOR EXPONENTIAL
termsToPolynomial :: [Term] -> Polynomial
termsToPolynomial terms =
  let filteredTerms = filter isPower terms
      powers = [(c, p) | Pw c p <- filteredTerms]
  in Polynomial powers
  where
    isPower (Pw _ _) = True
    isPower _        = False

instance Show Exponential where
    show (Exponential poly) =
        case poly of
            Polynomial [(_, Power 1)] -> "e^" ++ show poly
            _                         -> "e^(" ++ show poly ++ ")"

instance Evaluable Exponential where
    function (Exponential poly) = evalExp poly
evalExp :: Evaluable a => a -> Integer -> Double
evalExp poly x = getRounded (exp (function poly (fromIntegral x)))

instance Differentiable Exponential where
    derivative (Exponential e) = [Exp 1 (Power 0) (Exponential (e))]



-- INSTANCES FOR TERM

instance Show Term where
    show (Const c) = if c /= 0 then show c else ""
    show (Pw c p) = show (Polynomial [(c, p)])
    show (Trig c p t) = showC c ++ show p ++ show t
        where showC 1 = ""
              showC x = show x
    show (Exp c p e) = showC c ++ show p ++ show e
        where showC 1 = ""
              showC x = show x
instance Evaluable Term where
    function (Const c) = constFunction (fromIntegral c)
    function (Pw c p) = pwFunction (fromIntegral c) p
    function (Trig c p t) = trigFunction (fromIntegral c) p t
    function (Exp c p e) = expFunction (fromIntegral c) p e

constFunction :: Double -> Integer -> Double
constFunction c _ = c

pwFunction :: Double -> Power -> Integer -> Double
pwFunction c p x = c * function p x

trigFunction :: Double -> Power -> Trigonometric -> Integer -> Double
trigFunction c p t x = c * function p x * function t x

expFunction :: Double -> Power -> Exponential -> Integer -> Double
expFunction c p e x = c * function p x * function e x

instance Differentiable Term where
    derivative (Const _) = []
    derivative (Pw c p) = [Pw (c * fromIntegral (getPower p)) (Power (getPower p - 1))]
        where
            getPower (Power n) = n
    
-- INSTANCES FOR [TERM]

instance Evaluable [Term] where
  function terms x = getRounded (sum (map (\t -> function t x) terms))
  
  
instance Differentiable [Term] where
    derivative terms = concatMap derivative terms

