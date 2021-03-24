{-# LANGUAGE BangPatterns #-}

module Task1
  ( -- * Types
    Point(..)
    -- * Functions
  , doubleArea
  , doubleAreaSlow
  , perimeter
  , perimeterSlow
  ) where

-- | Type describing a 2-D point.
data Point = Point
  { x :: Int  -- ^ x coordinate
  , y :: Int  -- ^ y coordinate
  }

-- | Adds the coordinates of two 'Point'.
plus :: Point -> Point -> Point
plus (Point xa ya) (Point xb yb) = Point (xa + xb) (ya + yb)

-- | Subtract the coordinates of two 'Point'.
minus :: Point -> Point -> Point
minus (Point xa ya) (Point xb yb) = Point (xa - xb) (ya - yb)

-- | Scalar product of two 'Point'.
scalarProduct :: Point -> Point -> Int
scalarProduct (Point xa ya) (Point xb yb) = xa * xb + ya * yb

-- | Cross product of two 'Point'.
crossProduct :: Point -> Point -> Int
crossProduct (Point xa ya) (Point xb yb) = xa * yb - xb * ya

-- | Distance between two points.
dist :: Point -> Point -> Double
dist (Point xa ya) (Point xb yb) = (sqrt . fromIntegral) ((xa - xb) ^ 2 + (ya - yb) ^ 2)

-- | Calculates the perimeter of a polygon without self-intersections.
perimeter :: [Point] -> Double
perimeter [] = 0
perimeter points@(firstPt : _) = perimeter' points 0
  where
    perimeter' :: [Point] -> Double -> Double
    perimeter' []                    !acc = acc
    perimeter' (pt : [])             !acc = acc + (dist pt firstPt)
    perimeter' (pt1 : pts@(pt2 : _)) !acc = perimeter' pts (acc + (dist pt1 pt2))

-- | Naive implementation of 'perimeter'.
perimeterSlow :: [Point] -> Double
perimeterSlow []           = 0
perimeterSlow pts@(pt : _) = sum $ zipWith dist pts ((tail pts) ++ [pt])

-- | Calculates the double area of a polygon without self-intersections.
doubleArea :: [Point] -> Int
doubleArea [] = 0
doubleArea points@(firstPt : _) = doubleArea' points 0
  where
    doubleArea' :: [Point] -> Int -> Int
    doubleArea' []                    !acc = acc
    doubleArea' (pt : [])             !acc = acc + (crossProduct pt firstPt)
    doubleArea' (pt1 : pts@(pt2 : _)) !acc = doubleArea' pts (acc + crossProduct pt1 pt2)

-- | Naive implementation of 'doubleArea'.
doubleAreaSlow :: [Point] -> Int
doubleAreaSlow pts@(pt : _) = sum $ zipWith crossProduct pts ((tail pts) ++ [pt])
