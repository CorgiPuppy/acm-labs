module Main where
import Text.Printf
import Formatting
import Formatting.Formatters(center)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n - 1)

coefs, limits, nSplitExtra :: [Double]
namesCoefs, namesLimits :: [[Char]]
nSplit :: Double
namesCoefs = ["a", "b", "c", "d"]
namesLimits = ["x1", "x2"]
coefs = [-1.28, 6.5, 1.44, 0.14]
limits = [-1.4, 4.3]
nSplit = 25.0
nSplitExtra = [25.0, 100.0, 150.0, 200.0, 250.0] 
-- coefs = [-0.88, 3.98, -0.03, 0.24]
-- limits = [-3.4, 4.0]
-- nSplit = 20.0
-- nSplitExtra = [20.0, 100.0, 150.0, 200.0, 250.0] 
-- coefs = [-3.8, 3.56, 1.12, 0.19]
-- limits = [-2.7, 2.9]
-- nSplit = 30.0
-- nSplitExtra = [30.0, 100.0, 150.0, 200.0, 250.0] 

showData :: String
showData = 
   "\n+-----------------------+\n" ++
   "|\tКоэффициенты\t|\n" ++ 
   "+-----------------------+\n" ++
   showArgs 4 ++ 
   "+-----------------------+\n" ++
   "|\tПределы\t\t|\n" ++ 
   "|\tинтегрирования\t|\n" ++ 
   "+-----------------------+\n" ++
   showLimits 2 ++
   "+-----------------------+\n"
   where 
      showArgs 1 = showIA 0
      showArgs n = showArgs (n-1) ++ showIA (n-1)

      showLimits 1 = showIL 0
      showLimits n = showLimits (n-1) ++ showIL (n-1)

      showIL i = formater $ (namesLimits !! i) ++ " = " ++ show (limits !! i)
      
      showIA i = formater $ (namesCoefs !! i) ++ " = " ++ show (coefs !! i)
      formater a = "|\t" ++ a ++ "\t|\n"

showIntegralStep :: String
showIntegralStep = 
   "\n+-----------------------+\n" ++
   "|\tШаг\t\t|\n" ++
   "|\tинтегрирования\t|\n" ++  
   "+-----------------------+\n" ++
   showArgs 1 ++ 
   "+-----------------------+\n"
   where 
      showArgs 1 = showIA 0
      showArgs n = showArgs (n-1) ++ showIA (n-1)
      
      showIA i = formater $ formatToString ("dx" % " = " % (right 10 ' ' %. fixed 3)) (((last limits) - (head limits)) / nSplit);
      formater a = "|\t" ++ a ++ "\t|\n"

outputDots :: (String -> IO ()) -> [(Double, Double)] -> IO()
outputDots output [] = output ""
outputDots output ((x, y):tail) = do
   output $ formatToString ((right 10 ' ' %. fixed 3) % (right 6 ' ' %. fixed 3) % "\n") x y;
   outputDots output tail

showMethods :: String
showMethods =
   "\n+---------------------------------------------------------------+\n" ++
   "|\tРешение методами прямоугольников, трапеций и парабол\t|\n" ++  
   "+---------------+---------------+---------------+---------------+\n" ++ 
   (formatToString (rxf % rxf % rfxf % rsfxf % rsf % rff % rsf % rff % rsf % "\n") 
                    "|" "x" "|" "f(x)" "|" "f(x+0,5dx)" "|" "f(x+dx)" "|") ++
   "+---------------+---------------+---------------+---------------+\n"
   where
      rff = right 11 ' ' %. string
      rsf = right 5 ' ' %. string
      rxf = right 8 ' ' %. string
      rfxf = right 6 ' ' %. string
      rsfxf = right 10 ' ' %.string  

showMethodKotes :: String
showMethodKotes =
   "\n+-----------------------------------------------------------------------------------------------------------+\n" ++
   (formatToString ((right 37 ' ' %. string) % (right 0 ' ' %. string) % (left 37 ' ' %. string) % "\n") 
                     "|" "Решение методом Котеса 4-го порядка" "|") ++  
   "+-----------------+-----------------+-----------------+-----------------+-----------------+-----------------+\n" ++ 
   (formatToString (rxf % rxf % rfxf % rsfxf % rfxf % rsfxf % rfxf % rsfxf % rsf % rff % rsf % rff % rsf % "\n") 
                    "|" "x" "|" "f(x)" "|" "f(x+0,25dx)" "|" "f(x+0,5dx)" "|" "f(x+0,75dx)" "|" "f(x+dx)" "|") ++
   "+-----------------+-----------------+-----------------+-----------------+-----------------+-----------------+\n"
   where
      rff = right 12 ' ' %. string
      rsf = right 6 ' ' %. string
      rxf = right 9 ' ' %. string
      rfxf = right 7 ' ' %. string
      rsfxf = right 11 ' ' %.string  

results :: [Double] -> IO ()
results [] = putStr "+---------------+---------------+---------------+---------------+\n"
results (x:tail) = do
   putStr (formatToString (rsf % rxf % rsf % rxf % rsf % rxf % rsf % rxf % rsf % "\n") 
                           '|' x '|' (f x) '|' (f (x + h / 2)) '|' (f (x + h)) '|')
   results tail
   where 
      h = ((last limits) - (head limits)) / nSplit
      rsf = right 6 ' '
      rxf = right 10 ' ' %. fixed 3 

resultsKotes :: [Double] -> IO ()
resultsKotes [] = putStr "+-----------------+-----------------+-----------------+-----------------+-----------------+-----------------+\n"
resultsKotes (x:tail) = do
   putStr (formatToString (rsf % rxf % rsf % rxf % rsf % rxf % rsf % rxf % rsf % rxf % rsf % rxf % rsf % "\n")
                           '|' x '|' (f x) '|' (f (x + h / 4)) '|' (f (x + h / 2)) '|' (f (x + h * 3 / 4)) '|' (f (x + h)) '|')
   resultsKotes tail
   where 
      h = ((last limits) - (head limits)) / nSplit
      rsf = right 7 ' '
      rxf = right 11 ' ' %. fixed 3

f :: Double -> Double
f x = (coefs !! 0) * (exp ((-(coefs !! 3) * ((x - (coefs !! 2)) ** 2)))) + (coefs !! 1) 

xs :: Double -> [Double]
xs n = [(limits !! 0), (limits !! 0 + ((last limits - head limits) / n))..((limits !! 1) - ((last limits - head limits) / n))]

ys :: [Double]
ys = [f x | x <- (xs nSplit)]

dots :: [(Double, Double)]
dots = zip (xs nSplit) ys 

leftRectangles:: (Double -> Double) -> Double -> Double -> Double -> Double
leftRectangles f a b n = h * sum [f x | x <- (xs n)]
   where h = (b - a) / n

rightRectangles :: (Double -> Double) -> Double -> Double -> Double -> Double
rightRectangles f a b n = h * sum [f (x + h) | x <- (xs n)]
   where h = (b - a) / n

middleRectangles :: (Double -> Double) -> Double -> Double -> Double -> Double
middleRectangles f a b n = h * sum [f (x + h / 2) | x <- (xs n)]
   where h = (b - a) / n

trapezMethod :: (Double -> Double) -> Double -> Double -> Double -> Double
trapezMethod f a b n = h / 2 * (sum [f x | x <- (xs n)] + sum [f (x + h) | x <- (xs n)])
   where h = (b - a) / n

parabolaMethod :: (Double -> Double) -> Double -> Double -> Double -> Double
parabolaMethod f a b n = h / 6 * (sum [f x | x <- (xs n)] + 4 * sum [f (x + h / 2) | x <- (xs n)] + sum [f (x + h) | x <- (xs n)]) 
   where h = (b - a) / n

cotesMethod :: (Double -> Double) -> Double -> Double -> Double -> Double
cotesMethod f a b n = h / 90 * (sum [7 * f x | x <- (xs n)] + 
                                 sum [32 * f (x + 0.25 * h) | x <- (xs n)] + 
                                 sum [12 * f (x + 0.5 * h) | x <- (xs n)] +
                                 sum [32 * f (x + 0.75 * h) | x <- (xs n)] +
                                 sum [7 * f (x + h) | x <- (xs n)])
   where h = (b - a) / n 

namesMethods = ["Метод левых прямоугольников", "Метод средних прямоугольников", "Метод трапеций\t\t", "Метод парабол\t\t", "Метод Котеса 4-го порядка"]
resultsMethods = (leftRectangles f (head limits) (last limits) nSplit) :
                 (middleRectangles f (head limits) (last limits) nSplit) : 
                 (trapezMethod f (head limits) (last limits) nSplit) :
                 (parabolaMethod f (head limits) (last limits) nSplit) :
                 (cotesMethod f (head limits) (last limits) nSplit) : []
showResults :: String
showResults =
   "\n+---------------------------------------------------------------------------------------+\n" ++
   "|\t\t\t\t   Результаты\t\t\t\t\t\t|\n" ++  
   "+---------------------------------------+-----------------------------------------------+\n" ++ 
   "|\tНазвание метода\t\t\t|\tЗначение определенного интеграла\t|\n" ++
   "+---------------------------------------+-----------------------------------------------+\n" ++
   showResultsMethods 5 ++
   "+---------------------------------------+-----------------------------------------------+\n"
   where
      showResultsMethods 1 = showIRM 0
      showResultsMethods n = showResultsMethods (n-1) ++ showIRM (n-1)

      showIRM i = formater $ (formatToString ((right 0 ' ' %. string) % "\t|\t\t\t" % (right 0 ' ' %. fixed 4)) (namesMethods !! i) (resultsMethods !! i))
      formater a = "|\t" ++ a ++ "\t\t\t|\n"

newNamesMethods = ["Левых прямоугольников", "Средних прямоугольников", "Трапеций\t", "Парабол\t\t"]
errorMethods = (abs ((leftRectangles f (head limits) (last limits) nSplit) - (cotesMethod f (head limits) (last limits) nSplit))) :
               (abs ((middleRectangles f (head limits) (last limits) nSplit) - (cotesMethod f (head limits) (last limits) nSplit))) : 
               (abs ((trapezMethod f (head limits) (last limits) nSplit) - (cotesMethod f (head limits) (last limits) nSplit))) :
               (abs ((parabolaMethod f (head limits) (last limits) nSplit) - (cotesMethod f (head limits) (last limits) nSplit))) : [] 
showError :: String
showError = 
   "\n+---------------------------------------+-------------------------------+\n" ++ 
   "|\tМетод решения\t\t\t|\tАбсолютная ошибка\t|\n" ++
   "+---------------------------------------+-------------------------------+\n" ++
   showError 4 ++
   "+---------------------------------------+-------------------------------+\n"
   where
      showError 1 = showIRM 0
      showError n = showError (n-1) ++ showIRM (n-1)

      showIRM i = formater $ (formatToString ((right 0 ' ' %. string) % "\t\t|\t" % (right 0 ' ' %. fixed 8)) (newNamesMethods !! i) (errorMethods !! i))
      formater a = "|\t" ++ a ++ "\t\t|\n"

engNamesMethods = ["LeftRectangles", "MiddleRectangles", "Trapezoid", "Parabola"]
outputError :: (String -> IO ()) -> IO ()
outputError output = do
   output $ formatToString (rsf % "\n") (showArgs 4)
   where
      rsf = right 30 ' ' %.string
      ref = right 30 ' ' %. fixed 2

      showArgs 1 = showIA 0
      showArgs n = showArgs (n-1) ++ showIA (n-1)

      showIA i = formatToString ((engNamesMethods !! i) % (left 11 ' ' %. fixed 8) % "\n") (errorMethods !! i)

namesRightMethod = ["Метод правых прямоугольников, n = 30", "Метод правых прямоугольников, n = 100", "Метод правых прямоугольников, n = 150", "Метод правых прямоугольников, n = 200", "Метод правых прямоугольников, n = 250"]
resultsRightMethod = (rightRectangles f (head limits) (last limits) (nSplitExtra !! 0)) :
                     (rightRectangles f (head limits) (last limits) (nSplitExtra !! 1)) : 
                     (rightRectangles f (head limits) (last limits) (nSplitExtra !! 2)) :
                     (rightRectangles f (head limits) (last limits) (nSplitExtra !! 3)) :
                     (rightRectangles f (head limits) (last limits) (nSplitExtra !! 4)) : []
showRightResults :: String
showRightResults =
   "\n+-----------------------------------------------------------------------------------------------+\n" ++
   "|\t\t\t\t\t   Результаты\t\t\t\t\t\t|\n" ++  
   "+-----------------------------------------------+-----------------------------------------------+\n" ++ 
   "|\t\tНазвание метода\t\t\t|\tЗначение определенного интеграла\t|\n" ++
   "+-----------------------------------------------+-----------------------------------------------+\n" ++
   showRightResults 5 ++
   "+-----------------------------------------------+-----------------------------------------------+\n"
   where
      showRightResults 1 = showIRM 0
      showRightResults n = showRightResults(n-1) ++ showIRM (n-1)

      showIRM i = formater $ (formatToString ((right 0 ' ' %. string) % "\t|\t\t\t" % (right 0 ' ' %. fixed 4)) (namesRightMethod !! i) (resultsRightMethod !! i))
      formater a = "|\t" ++ a ++ "\t\t\t|\n"

errorRightMethod = (abs (resultsRightMethod !! 0) - (cotesMethod f (head limits) (last limits) (nSplitExtra !! 0))) :
                   (abs (resultsRightMethod !! 1) - (cotesMethod f (head limits) (last limits) (nSplitExtra !! 1))) : 
                   (abs (resultsRightMethod !! 2) - (cotesMethod f (head limits) (last limits) (nSplitExtra !! 2))) :
                   (abs (resultsRightMethod !! 3) - (cotesMethod f (head limits) (last limits) (nSplitExtra !! 3))) :
                   (abs (resultsRightMethod !! 4) - (cotesMethod f (head limits) (last limits) (nSplitExtra !! 4))) : [] 
outputAbsoluteError :: (String -> IO ()) -> IO ()
outputAbsoluteError output = do
   output $ formatToString (rsf % "\n") (showArgs 5)
   where
      rsf = right 30 ' ' %.string
      ref = right 30 ' ' %. fixed 2

      showArgs 1 = showIA 0
      showArgs n = showArgs (n-1) ++ showIA (n-1)

      showIA i = formatToString ((right 10 ' ' %.fixed 0) % (right 10 ' ' %. fixed 8) % "\n") (nSplitExtra !! i) (errorRightMethod !! i)

main :: IO ()
main = do
   writeFile "test.txt" "";
   outputDots (appendFile "test.txt") dots;
   putStr showData 
   putStr showIntegralStep
   putStr showMethods
   results (xs nSplit)
   putStr showMethodKotes
   resultsKotes (xs nSplit) 
   putStr showResults
   putStr showError
   writeFile "test1.txt" "";
   outputError (appendFile "test1.txt")
   putStr showRightResults
   writeFile "test2.txt" "";
   outputAbsoluteError (appendFile "test2.txt")