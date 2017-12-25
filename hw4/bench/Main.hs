module Main where

import           Criterion.Main    (bench, bgroup, defaultMain, nf)

import           Algo.MapNub       (mNub)
import           Algo.MergeSortNub (sNub)
import           Data.List         (nub)

main :: IO ()
main = defaultMain $
  let testList300 = [(1::Int)..(300::Int)] ++ [(300::Int),(299::Int)..(1::Int)] 
      testList2002 = [(1::Int)..(500::Int)] ++ [(500::Int),(499::Int)..(1::Int)] ++ 
                     [(-1000::Int),(-998::Int)..(0::Int)] ++ [(0::Int),(-2::Int)..(-1000::Int)]
      in
  [ bgroup "is500"
    [ bench "nub" $ nf nub          testList300
    , bench "mNub" $ nf mNub        testList300
    , bench "sNub" $ nf sNub        testList300
    ]
  , bgroup "is2002"
    [ bench "nub" $ nf nub          testList2002
    , bench "mNub" $ nf mNub        testList2002
    , bench "sNub" $ nf sNub        testList2002
    ]
  ]
