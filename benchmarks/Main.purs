
module Benchmarks.Main where

import Prelude
import Data.Array ((..))
import Data.Int as I
import Data.DataFrame as DF
import Test.QuickCheck.Gen (Gen(), vectorOf)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

import Benchotron.Core
import Benchotron.UI.Console

sizeScale = (_ * 100)

randomArray :: Int -> Gen (Array Number)
randomArray = flip vectorOf arbitrary

benchFilter :: Benchmark
benchFilter = mkBenchmark
  { slug: "filter"
  , title: "Filter the dataframe"
  , sizes: (1..50) <#> sizeScale
  , sizeInterpretation: "Number of rows in the dataset"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn' "Array DataFrame" (DF.runQuery (DF.filter f)) DF.init
               -- TODO: benchmark different versions of the code if possible...
               ]
  }
  where
  f = (_ > 0.0)

benchGroup :: Benchmark
benchGroup = mkBenchmark
  { slug: "group"
  , title: "Group the dataframe"
  , sizes: (1..50) <#> sizeScale
  , sizeInterpretation: "Number of rows in the dataset"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn' "Array DataFrame" (DF.runQuery (DF.group f)) DF.init
               -- TODO: benchmark different versions of the code if possible...
               ]
  }
  where
  f x = mod (I.floor x) 10

benchCount :: Benchmark
benchCount = mkBenchmark
  { slug: "count"
  , title: "Size of groups in the dataframe"
  , sizes: (1..50) <#> sizeScale
  , sizeInterpretation: "Number of rows in the dataset"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn' "Array DataFrame" (DF.runQuery (DF.count f)) DF.init
               -- TODO: benchmark different versions of the code if possible...
               ]
  }
  where
  f x = mod (I.floor x) 10

benchSummarize :: Benchmark
benchSummarize = mkBenchmark
  { slug: "summarize"
  , title: "Convert the dataframe to an array"
  , sizes: (1..50) <#> sizeScale
  , sizeInterpretation: "Number of rows in the dataset"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn' "Array DataFrame" (DF.runQuery (DF.summarize id)) DF.init
               -- TODO: benchmark different versions of the code if possible...
               ]
  }

benchTrim :: Benchmark
benchTrim = mkBenchmark
  { slug: "trim"
  , title: "Truncate the dataframe"
  , sizes: (1..50) <#> sizeScale
  , sizeInterpretation: "Number of rows in the dataset"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn' "Array DataFrame" (DF.runQuery (DF.trim 10)) DF.init
               -- TODO: benchmark different versions of the code if possible...
               ]
  }

main = runSuite
  [ benchFilter
  , benchGroup
  , benchCount
  , benchSummarize
  , benchTrim
  ]

