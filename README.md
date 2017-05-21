
# purescript-dataframe

[![Latest release](http://img.shields.io/github/release/gabysbrain/purescript-dataframe.svg)](https://github.com/gabysbrain/purescript-dataframe/releases)

A datastructure designed to be used with queries as well as a type for
queries. There is also semantics for combining the queries.

# Example

```purescript
main = do
  let df = init [1, 2, 3, 4, 5, 6, 7]
      q = filter (\x -> x > 3) `chain`
          mutate show `chain`
          trim 3
  putStrLn $ runQuery q df
```

# Getting started

## Installation

```
bower install purescript-dataframe
```

## Queries

The idea of a Query type is that we want to have a type-safe way to chain
operations on dataframes and we want to maintain the original dataset
throughout the query. In other data processing languages this is a common
source of error, especially when mutating rows. 

The set of dataframe operations are based on what's offered by the
[dplyr](https://github.com/tidyverse/dplyr) R package.

* `filter :: forall r. (r -> Boolean) -> Query (DataFrame r) (Dataframe r)` 
  filter rows of the DataFrame
* `group :: forall r g. Ord g => (r -> g) -> Query (DataFrame r) (DataFrame {group :: g, data :: Dataframe r})` 
  group the rows of the dataframe by some grouping method
* `count :: forall r g. Ord g => (r -> g) -> Query (DataFrame r) (DataFrame {group :: g, count :: Int})` 
  group the rows of the dataframe and count the size of the groups
* `summarize :: forall r x. (r -> x) -> Query (DataFrame r) (Array x)` 
  convert each row of the dataframe to some type and return an array
* `mutate :: forall r s. (r -> s) -> Query (DataFrame r) (Dataframe s)` 
  change each row of the dataframe to some other type
* `sort :: forall r. (r -> r -> Ordering) -> Query (DataFrame r) (Dataframe r)` 
  sort the rows of the dataframe given the ordering function
* `trim :: forall r. Int -> Query (DataFrame r) (Dataframe r)` 
  keep only the first n rows of the DataFrame

The `chain :: forall r s t. Query r s -> Query s t -> Query r t` function
allows us to chain queries and keep the original context.

# API Docs

API documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-dataframe).

# Todos

* DataFrames should be able to operate either as a set of columns or a set of rows.
* Queries should do caching

