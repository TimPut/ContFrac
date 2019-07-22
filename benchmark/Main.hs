module Main (main) where

import           Gauge.Main
import           Numeric.Fraction

main :: IO ()
main = defaultMain [
       bgroup "phi" [ bench "10" $ nf (approximate (10 :: Integer)) phi
                    , bench "10^10" $ nf (approximate (10^10 :: Integer)) phi
                    , bench "10^100" $ nf (approximate (10^100 :: Integer)) phi
                    ]
       ,
       bgroup "phi cf" [ bench "10" $ nf (approximateContinued (10 :: Integer)) phiCF
                       , bench "10^10" $ nf (approximateContinued (10^10 :: Integer)) phiCF
                       , bench "10^100" $ nf (approximateContinued (10^100 :: Integer)) phiCF
                       ]

                   ]

phi :: Double
phi = (1 + sqrt 5) / 2

phiCF :: [Integer]
phiCF = [1,1..]
