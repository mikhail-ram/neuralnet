module Matrix
( matrix,
  at,
  values,
  shape,
  transpose,
  dot,
  pad,
  Index (Row, Column, Both),
  Slice (Range, Singular)
) where

import           Data.Foldable (toList)
import qualified Data.Sequence as Sequence

type Start = Int
type End = Start
data Slice = Range (Start, End) | Singular Int
data Index = Both (Slice, Slice) | Row Int | Column Int

data Matrix = Matrix { values :: Sequence.Seq Float, shape :: (Int, Int) }

instance Show Matrix where
  show (Matrix values (1, 1)) = show $ values `Sequence.index` 0
  show (Matrix values (1, _)) = show $ toList values
  show (Matrix values (rows, columns)) = show $ toList $ toList <$> Sequence.chunksOf columns values

matrix :: [[Float]] -> Matrix
matrix xs = if all(== head lengths) (tail lengths) 
  then
    Matrix {
      values = Sequence.fromList $ concat xs,
      shape = (length xs, length (head xs))
    }
  else error "Dimensions of matrix must match"
  where lengths = map length xs

-- test `at` Both (Range (0, 2), Range (0, 1))
-- Zero Indexed
-- Negative Indexing is allowed to index from end of matrix
-- Ranges are non-inclusive
-- test `at` Row 1
-- (0,3),(0,3)
-- 0, (0,3)
-- (0,3), 0
-- 0, 0

takeNSepByS :: Int -> Int -> Sequence.Seq Float -> Sequence.Seq Float
takeNSepByS _ _ Sequence.Empty = Sequence.Empty
takeNSepByS n s xs = chunk Sequence.>< takeNSepByS n s (Sequence.drop s remainder)
  where (chunk, remainder) = Sequence.splitAt n xs 

-- For now, with ranges assume only positive integers work for slices, no negative indexing for counting backwards
at :: Matrix -> Index -> Matrix
at matrix@(Matrix values shape) (Both (Range (row_start, row_end), Range (column_start, column_end)))
  | row_start >= 0 && row_end <= fst shape && row_start < row_end && column_start >= 0 && column_end <= snd shape && column_start < column_end = Matrix {
      values = takeNSepByS (column_end - column_start) (snd shape - (column_end - column_start)) (Sequence.drop (column_start + row_start * snd shape) (Sequence.take (row_end * snd shape) values)),
      shape = (row_end - row_start, column_end - column_start)
    }
  | otherwise = error "Row or column slice is out of range."

at matrix@(Matrix values shape) (Both (Singular row, Range (column_start, column_end)))
  | row >= 0 && row < fst shape && column_start >= 0 && column_end <= snd shape && column_start < column_end = Matrix {
      values = takeNSepByS (column_end - column_start) (snd shape - column_end) (Sequence.drop (column_start + row * snd shape) (Sequence.take ((row + 1) * snd shape) values)),
      shape = (1, column_end - column_start)
    }
  | otherwise = error "Row slice is out of range."

at matrix@(Matrix values shape) (Both (Range (row_start, row_end), Singular column))
  | row_start >= 0 && row_end <= fst shape && row_start < row_end && column >= 0 && column < snd shape = Matrix {
      values = takeNSepByS 1 (snd shape - 1) (Sequence.drop (column + row_start * snd shape) (Sequence.take (row_end * snd shape) values)),
      shape = (row_end - row_start, 1)
    }
  | otherwise = error "Column slice is out of range."

at matrix@(Matrix values shape) (Both (Singular row, Singular column))
  | (row >= 0 && row < fst shape) && (column >= 0 && column < snd shape) = case Sequence.lookup (row * snd shape + column) values of
    Just value -> Matrix { values = Sequence.singleton value, shape = (1, 1) }
    Nothing    -> error "Sequence does not exist."
  | (row < 0 && row >= -(fst shape)) && (column < 0 && column >= -(snd shape)) = matrix `at` Both (Singular (fst shape + row), Singular (snd shape + column))
  | (row < 0 && row >= -(fst shape)) && (column >= 0 && column < snd shape) = matrix `at` Both (Singular (fst shape + row), Singular column)
  | (row >= 0 && row < fst shape) && (column < 0 && column >= -(snd shape)) = matrix `at` Both (Singular row, Singular (snd shape + column))
  | otherwise = error "Row or column index is out of bounds."

at matrix@(Matrix values shape) (Row row)
  | row < fst shape && row >= 0 = Matrix { values = Sequence.drop (row * snd shape) (Sequence.take ((row + 1) * snd shape) values), shape = (1, snd shape) }
  | row < 0 && row >= -(fst shape) = matrix `at` Row (fst shape + row)
  | otherwise = error "Row index is out of bounds."

at matrix@(Matrix values shape) (Column column)
  | column < snd shape && column >= 0 = Matrix {
      values = takeNSepByS 1 (snd shape - 1) (Sequence.drop column values),
      shape = (fst shape, 1)
    }
  | column < 0 && column >= -(snd shape) = matrix `at` Column (snd shape + column)
  | otherwise = error "Column index is out of bounds."

pad :: Matrix -> Float -> Int -> Int -> Matrix
pad (Matrix values shape) pad_value row_pad column_pad = Matrix {
    values = padded_values,
    shape = (fst shape + row_pad, snd shape + column_pad)
  } where padded_values = padded_column_values Sequence.>< padded_row_values
          padded_row_values = Sequence.replicate (row_pad * (snd shape + column_pad)) pad_value
          padded_column_values = foldr1 (Sequence.><) (Sequence.intersperse column_pad_slice (Sequence.chunksOf (snd shape) values)) Sequence.>< column_pad_slice
          column_pad_slice = Sequence.replicate column_pad pad_value

instance Num Matrix where
  (Matrix as shape_a) + (Matrix bs shape_b)
    | shape_a == shape_b = Matrix {
        values = (Sequence.zipWith (+) as bs),
        shape = shape_a
      }
    | otherwise = error "Matrices a and b must have the same shape."
  (Matrix as shape_a) - (Matrix bs shape_b)
    | shape_a == shape_b = Matrix {
        values = (Sequence.zipWith (-) as bs),
        shape = shape_a
      }
    | otherwise = error "Matrices a and b must have the same shape."
  {-
  (Matrix as shape_a) * b@(Matrix bs shape_b)
    | snd shape_a == fst shape_b = Matrix {
        values = Sequence.zipWith (dot) [Sequence.chunksOf (snd shape_a) as] [Sequence.chunksOf (fst shape_b) (values (transpose b))],
        shape = (fst shape_a, snd shape_b)
      }
    | otherwise = error "Matrices a and b must have the same shape."
  -}
transpose :: Matrix -> Matrix
transpose matrix@(Matrix values shape) = Matrix {
    values = transposed_values,
    shape = (snd shape, fst shape)
  } where transposed_values = getColumns matrix (snd shape - 1)
          getColumns :: Matrix -> Int -> Sequence.Seq Float
          getColumns (Matrix values shape) 0 = takeNSepByS 1 (snd shape - 1) values
          getColumns matrix@(Matrix values shape) column_index = getColumns matrix (column_index - 1) Sequence.>< takeNSepByS 1 (snd shape - 1) (Sequence.drop column_index values)

dot :: Matrix -> Matrix -> Float
dot (Matrix as shape_a) (Matrix bs shape_b)
  | shape_a == shape_b && fst shape_a  == 1 = sum $ Sequence.zipWith (*) as bs
  | otherwise = error "The length of vector a and vector b must be the same."

{-
naiveMatrixMultiply :: Matrix -> Matrix -> Matrix
naiveMatrixMultiply (Matrix as shape_a) (Matrix bs shape_b)
  | snd shape_a == fst shape_b =
  | otherwise = error "Matrix A must have shape (m x n) and matrix B must have shape (n x k) to qualify for matrix multiplication."
-}

multiply :: (Int, Int) -> Int -> Int -> [[Int]]
multiply shape 0 0 = [[0, 0]]
multiply shape row_index 0 = [row_index, 0]:multiply shape (row_index - 1) (snd shape - 1)
multiply shape row_index column_index = [row_index,column_index]:multiply shape row_index (column_index - 1)

{-
matrixMultiply :: Matrix -> Matrix -> Int -> Int -> Sequence.Seq Float
matrixMultiply a@(Matrix as shape_a) b@(Matrix bs shape_b) row_index column_index
  | row_index == 0 && column_index == 0 = Sequence.singleton $ (a `at` Row 0) `dot` (b `at` Column 0)
  | column_index == 0 = (a `at` Row row_index) `dot` (b `at` Column column_index) Sequence.<| matrixMultiply a b (row_index - 1) (snd shape_b)
  | otherwise = ((a `at` Row row_index) `dot` (b `at` Column column_index)) Sequence.<| matrixMultiply a b row_index (column_index - 1)
-}
{-
matrixMultiply a@(Matrix as shape_a) b@(Matrix bs shape_b) row_index column_index
  | column_index == 0 = (a `at` Row row_index) `dot` (b `at` Column column_index)

-}
{-

a = matrix [[1.0, 2.0],
            [3.0, 4.0],
            [5.0, 6.0],
            [7.0, 8.0]]

b = matrix [[1.0, 2.0, 3.0],
            [4.0, 5.0, 6.0]]

a * b = [[9.0, 12.0, 15.0],
         [19.0, 26.0, 33.0],
         [29.0, 40.0, 51.0]]
-}
