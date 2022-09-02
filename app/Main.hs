module Main where
import           Matrix

main :: IO ()
main = do print $ a + b
          print $ matrix [[1.0, 2.0]]

a = matrix [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]
b = matrix [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]

inputs = matrix [[1.00, 2.00, 3.00, 2.50],
                 [2.00, 5.00, -1.00, 2.00],
                 [-1.50, 2.70, 3.30, -0.80]]


weights = matrix [[0.20, 0.80, -0.50, 1.00],
                  [0.50, -0.91, 0.26, -0.50],
                  [-0.26, -0.27, 0.17, 0.87]]

weights2 = matrix [[0.10, -0.14, 0.50],
                   [-0.50, 0.12, -0.33],
                   [-0.44, 0.73, -0.13]]

biases = matrix [[2.00, 3.00, 0.50]]

biases2 = matrix [[-1.00, 2.00, -0.50]]


-- TODO: Implement Strassen's algorithm
-- TODO: Fix naive implementation
--matrixMultiply :: [[Float]] -> [[Float]] -> [[Float]]
{-
matrixMultiply :: Matrix -> Matrix -> Matrix
matrixMultiply (Matrix as shape_a) (Matrix bs shape_b)
  | snd shape_a == fst shape_b = [[dot row column | column <- transpose b] | row <- a]
  | otherwise = error
-}

--layer1Outputs = map (zipWith (+) biases) $ matrixMultiply inputs (transpose weights)
--layer2Outputs = map (zipWith (+) biases2) $ matrixMultiply layer1Outputs (transpose weights2)

-- layerOutputs = zipWith (+) biases $ map (dot inputs) weights

test = matrix [[1.0, 2.0, 3.0],
               [4.0, 5.0, 6.0],
               [7.0, 8.0, 9.0],
               [10.0, 11.0, 12.0],
               [13.0, 14.0, 15.0]]

{-
Tests
test `at` Both (Range(1, 4), Range(1, 3)) = [[5.0, 6.0],
                                             [8.0, 9.0],
                                             [11.0, 12.0]]
test `at` Both (Singular 3, Range (1, 3)) = [[11.0, 12.0]]
test `at` Both (Range (2, 4), Singular 1) = [[8.0], [11.0]]
test `at` Row 2 = [[7.0, 8.0, 9.0]]
test `at` Column 1 = [[2.0], [5.0], [8.0], [11.0], [14.0]]

-}