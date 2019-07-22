module Main (main) where

import           GHC.Real         (Ratio (..), (%))
import           Numeric.Fraction
import           Test.HUnit
main :: IO ()
main = do
  _ <- runTestTT . TestList $ [
        TestCase $ assertEqual "sqrt 2" sqrtTwoOeis
                   (take (length sqrtTwoOeis) . convergents $ [1] ++ [2,2..])
       -- Some existing libraries fail to terminate when the
       -- convergents have all denominators bounded above by the
       -- requested denominator
       , TestCase $ assertEqual "Whole Number" (2 % 1) (approximate 100 2)
       ]
  return ()

sqrtTwoOeis :: [(Ratio Integer)]
sqrtTwoOeis = zipWith (:%) sqrtTwoNums sqrtTwoDenoms

-- https://oeis.org/A001333
sqrtTwoNums :: [Integer]
sqrtTwoNums =
  [ 1
  , 1
  , 3
  , 7
  , 17
  , 41
  , 99
  , 239
  , 577
  , 1393
  , 3363
  , 8119
  , 19601
  , 47321
  , 114243
  , 275807
  , 665857
  , 1607521
  , 3880899
  , 9369319
  , 22619537
  , 54608393
  , 131836323
  , 318281039
  , 768398401
  , 1855077841
  , 4478554083
  , 10812186007
  , 26102926097
  , 63018038201
  , 152139002499
  , 367296043199
  ]

-- https://oeis.org/A000129
sqrtTwoDenoms :: [Integer]
sqrtTwoDenoms =
  [ 0
  , 1
  , 2
  , 5
  , 12
  , 29
  , 70
  , 169
  , 408
  , 985
  , 2378
  , 5741
  , 13860
  , 33461
  , 80782
  , 195025
  , 470832
  , 1136689
  , 2744210
  , 6625109
  , 15994428
  , 38613965
  , 93222358
  , 225058681
  , 543339720
  , 1311738121
  , 3166815962
  , 7645370045
  , 18457556052
  , 44560482149
  , 107578520350
  , 259717522849
  ]
