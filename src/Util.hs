module Util where
import RIO


slice :: Int -> Int -> [a] -> [a]
slice iFrom iTo xs = take (iTo - iFrom + 1) (drop iFrom xs)
