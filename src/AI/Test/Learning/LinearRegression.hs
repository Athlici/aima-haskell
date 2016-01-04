module AI.Test.Learning.LinearRegression (runAllTests) where

import           Data.Packed.Matrix
import           Data.Packed.Vector
import           Numeric.Container
import           Test.QuickCheck

import           AI.Learning.LinearRegression
import           AI.Test.Util
import           AI.Util.Matrix

-- |Regressing against a column of zeros should return a zero result vector.
testRegressionAgainstZeros :: Gen Bool
testRegressionAgainstZeros = do
    m <- choose (1,10)
    n <- choose (m,100)
    x <- arbitraryGaussianMatrix (n,m) :: Gen (Matrix Double)
    let y       = constant 0 n
        b       = constant 0 m
        bSample = regress x y
    return (bSample == b)


allTests =
    [ testRegressionAgainstZeros ]

runAllTests = mapM_ quickCheck allTests
