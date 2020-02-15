module Lib.Main where

import Data.Fixed (mod')
import Data.List (sort, intersect)
import Data.List.Unique (unique)


-- | `rotate i0 as` returns `as` with the first `i0` things
-- moved to the end. `i0` should be positive.
rotate :: Int -> [a] -> [a]
rotate i0 as = let
  i = mod i0 (length as)
  in
  drop i as ++ take i as

-- | `transpose z shift x` moves `x` up by `shift`, mod `et`.
transpose :: Real a => a -> a -> a -> a
transpose et i = flip mod' et . (+i)

-- | all the transposes of a list
all_transpositions :: (Real a, Enum a)
                   => a -> [a] -> [[a]]
all_transpositions et x = [ sort $ map (transpose et i) x
                          | i <- [0..11] ]

-- | all the modes of a scale
modes :: Real a => a -> [a] -> [[a]]
modes z as = [ sort $ map (transpose z (-a)) as
             | a <- as ]


-- | = For analyzing pairs of scales.
-- PITFALL: The below might be broken. I started the git repo
-- only after extending the above to work in arbitrary temperaments.
-- It's backed up on my hard drives, though.

type Scale = [Int]

data ScalePair = SP { sP1         :: Scale
                    , sP2         :: Scale
                    , sPIntersect :: Scale
                    , sPOverlap   :: Int
                    }
               deriving (Show, Eq, Ord)

sp :: Scale -> Scale -> ScalePair
sp as bs = let
  i = intersect as bs
  in
  SP { sP1 = as
     , sP2 = bs
     , sPIntersect = i
     , sPOverlap = length i }

sps :: Int -> Scale -> Scale -> [ScalePair]
sps et as bs = map (sp as) $ all_transpositions et bs

showSp :: ScalePair -> String
showSp p = "SP { "
  ++ show (sP1 p)         ++ "\n   , "
  ++ show (sP2 p)         ++ "\n   , "
  ++ show (sPIntersect p) ++ "\n   , "
  ++ show (sPOverlap p)
  ++ " }"

maj :: Scale
maj = [0,2,4,5,7,9,11]

analyze :: Int -> Scale -> Scale -> IO ()
analyze z x y =
  mapM_ (putStrLn . showSp) $
  sps z x y
