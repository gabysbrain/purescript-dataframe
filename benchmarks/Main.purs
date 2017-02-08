
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
--defaultSize = (1..50) <#> sizeScale
defaultSize = [1, 10, 20, 30, 40, 50] <#> sizeScale

randomArray :: Int -> Gen (Array Number)
randomArray = flip vectorOf arbitrary

benchFilter :: Benchmark
benchFilter = mkBenchmark
  { slug: "filter"
  , title: "Filter the dataframe"
  , sizes: defaultSize
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
  , sizes: defaultSize
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
  , sizes: defaultSize
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
  , sizes: defaultSize
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
  , sizes: defaultSize
  , sizeInterpretation: "Number of rows in the dataset"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn' "Array DataFrame" (DF.runQuery (DF.trim 10)) DF.init
               -- TODO: benchmark different versions of the code if possible...
               ]
  }

bench_groups :: Benchmark
bench_groups = mkBenchmark
  { slug: "_group"
  , title: "Group the dataframe"
  , sizes: defaultSize
  , sizeInterpretation: "Number of rows in the dataset"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "Array Grouping" (DF._groups f)
               , benchFn "Seq Grouping" (DF._groups' f)
               , benchFn "List Grouping" (DF._groups'' f)
               -- TODO: benchmark different versions of the code if possible...
               ]
  }
  where
  f x = mod (I.floor x) 10

main = runSuite
  [ benchFilter
  , benchGroup
  , benchCount
  , benchSummarize
  , benchTrim
  , bench_groups
  ]

