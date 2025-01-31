module Mandelbrot where

-- Complex number representation
data Complex = Complex Double Double
  deriving (Show)

-- Addition of complex numbers
(+.) :: Complex -> Complex -> Complex
(Complex a b) +. (Complex c d) = Complex (a + c) (b + d)

-- Multiplication of complex numbers
(*.) :: Complex -> Complex -> Complex
(Complex a b) *. (Complex c d) = Complex (a*c - b*d) (a*d + b*c)

-- Magnitude squared of a complex number
magSquared :: Complex -> Double
magSquared (Complex x y) = x*x + y*y

-- Mandelbrot set iteration function
mandelbrotIter :: Int -> Complex -> Complex -> Maybe Int
mandelbrotIter maxIter c z
  | magSquared z > 4   = Nothing  -- Escapes
  | maxIter == 0       = Just 0   -- Reached max iterations
  | otherwise          = mandelbrotIter (maxIter - 1) c (z *. z +. c)

-- Check if a point is in the Mandelbrot set
isInMandelbrot :: Int -> Complex -> Bool
isInMandelbrot maxIter c = case mandelbrotIter maxIter c (Complex 0 0) of
  Just _  -> True
  Nothing -> False

-- Generate Mandelbrot set points
generateMandelbrot :: Int -> Int -> Int -> [[Char]]
generateMandelbrot width height maxIter = 
  [ [ mandelbrotPixel x y | x <- [0..width-1] ]
    | y <- [0..height-1]
  ]
  where
    mandelbrotPixel x y = 
      let 
        -- Map pixel coordinates to complex plane
        real = fromIntegral x / fromIntegral width * 4 - 2
        imag = fromIntegral y / fromIntegral height * 4 - 2
        c = Complex real imag
      in 
        if isInMandelbrot maxIter c 
          then '*'  -- Point in set
          else ' '  -- Point outside set

-- Print Mandelbrot set
printMandelbrot :: Int -> Int -> Int -> IO ()
printMandelbrot width height maxIter = 
  mapM_ putStrLn (generateMandelbrot width height maxIter)

-- Main function to demonstrate
main :: IO ()
main = printMandelbrot 80 40 50