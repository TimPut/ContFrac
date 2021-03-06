{-# LANGUAGE BangPatterns #-}

module Numeric.Fraction (convergents, toContinued, upperBounds, lowerBounds, approximate, approximateContinued, strict) where

import           Data.Ratio
import           GHC.Real   (Ratio (..))

-- We really do want a zero denominator here, so we use the dumb constructor from GHC.Real.
convergents :: Integral a => [a] -> [Ratio a]
convergents coeffs = (\(_,res,_) -> res) <$> iterate conv (0 :% 1, 1 :% 0, coeffs)
  where conv (!h_2 , !h_1, (a:as)) = ( h_1
                                   , (a * numerator   h_1 + numerator h_2)
                                   % (a * denominator h_1 + denominator h_2)
                                   , as)
        conv (!h_2, !h_1, [])          = ( h_2, h_1, [])

toContinued :: (RealFrac t, Integral a) => t -> [a]
toContinued x = if b /= 0 then a : toContinued (recip b) else [a]
    where (a, b) = properFraction x

upperBounds :: Integral a => [a] -> [Ratio a]
upperBounds = evens . convergents
  where evens (x:_:xs) = x : evens xs
        evens x        = x
lowerBounds :: Integral a => [a] -> [Ratio a]
lowerBounds = odds . convergents
  where odds (_:y:xs) = y : odds xs
        odds _        = []

-- computes the best rational approximation of the second kind with
-- bounded denominator, using Khinchin's terminology. In a case of
-- convergent design with Khinchin, we set the -1th convergent to the
-- (merely) formal rational 1%0.
approximate :: (Integral a, RealFrac t) => a -> t -> Ratio a
approximate denom = approximateContinued denom  . toContinued

approximateContinued :: Integral a => a -> [a] -> Ratio a
approximateContinued denom = last . (++) [1 :% 0] . takeWhile (\q -> denominator q <= denom) . strict . convergents

-- when strict is applied to a monotone list, a strictly monotone list is produced.
strict :: Eq a => [a] -> [a]
strict (x:y:xs) = if x == y then [x] else x : strict (y:xs)
strict x        = x
