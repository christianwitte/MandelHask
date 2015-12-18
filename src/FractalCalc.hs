--import Data.complex


module FractalCalc (
    Context(Context),
    Point(Point),
    calculation
    ) where

import Data.Complex
import Control.Monad.Reader

data Context = Context { width :: Int,
                         height :: Int,
                         minX :: Double,
                         maxX :: Double,
                         minY :: Double,
                         maxY :: Double
                         } deriving (Show)

data Point = Point { x :: Int,
                     y :: Int
                   } deriving (Show)


calculate :: Int -> Int -> Complex Double -> (Complex Double, Double)
--calculate c = magnitude c
calculate maxIter limit c = cal' c 1 where
    cal' z maxIter = (z, maxIter)
    cal'

getField :: Reader Context [Point]
getField = do
  w <- asks width
  h <- asks height
  return [ Point x y | x <- [0..w], y <- [0..h] ]

transformX :: Reader Context (Int -> Double)
transformX = do
  ctx <- ask
  return $ (\x -> (minX ctx) + ((fromIntegral x) / (fromIntegral $ width ctx)) * (maxX ctx - minX ctx))

transformY :: Reader Context (Int -> Double)
transformY = do
  ctx <- ask
  return $ (\y -> (minY ctx) + ((fromIntegral y) / (fromIntegral $ height ctx)) * (maxY ctx - minY ctx))

transformPoint :: Reader Context (Point -> (Complex Double))
transformPoint = do
  cx <- transformX
  cy <- transformY
  return $ \Point { x = x, y = y } -> (cx x) :+ (cy y)




transformPoints :: [Point] -> Reader Context [Complex Double]
transformPoints ps = do
  tp <- transformPoint
  return $ map tp ps

calculation :: Double -> Reader Context [(Point, Double)]
calculation limit = do
    field <- getField
    tp <- transformPoints field
    let v = map calculate tp
    return (field `zip` v)

