
module Data.DataFrame where

import Prelude
import Control.Monad.Reader (Reader, ask, runReader)
import Data.Array as A
--import Data.Sequence as S
--import Data.List as L
import Data.Map as M
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Tuple (Tuple(..))

import Data.Monoid (class Monoid)

-- | A DataFrame is an ordered set of rows of type `r`.
newtype DataFrame r = DataFrame (Array r)

-- | A query result gives both the original input and the query result.
-- | Queries can be chained together.
-- |
-- | The idea of the query result is that maintaining the original input
-- | is helpful for things like histograms and graphs you want the original 
-- | frame for computing ranges for axes and bin widths
-- |
-- |The idea of the query result is to maintain the orignal data frame:
type Query df r = Reader df r

instance semigroupDataFrame :: Semigroup (DataFrame r) where
  append (DataFrame d1) (DataFrame d2) = DataFrame $ d1 <> d2

instance monoidDataFrame :: Monoid (DataFrame r) where
  mempty = DataFrame []

instance foldableDataFrame :: Foldable DataFrame where
  foldr   f z (DataFrame df) = foldr   f z df
  foldl   f z (DataFrame df) = foldl   f z df
  foldMap f   (DataFrame df) = foldMap f df

instance functorDataFrame :: Functor DataFrame where
  map f (DataFrame df) = DataFrame $ map f df

instance applyDataFrame :: Apply DataFrame where
  apply (DataFrame f) (DataFrame df) = DataFrame $ apply f df

-- | Create a data frame from an arbitrary Foldable. Each item of the Foldable
-- | will become a "row" of the DataFrame.
init :: forall f r. Foldable f => f r -> DataFrame r
init = DataFrame <<< A.fromFoldable

-- | Evaluate the query given the starting input. Returns the result 
-- | of the Query.
runQuery :: forall a b. Query a b -> a -> b
runQuery = runReader

-- | The number of "rows" in the DataFrame.
rows :: forall r. DataFrame r -> Int
rows (DataFrame df) = A.length df

-- | Get back the original input so that it can be chained into 
-- | annother query.
reset :: forall df. Query df df
reset = ask

-- | Create a query that will filter a dataframe so that it only contains
-- | rows where the filter function returns true.
filter :: forall r. (r -> Boolean) -> Query (DataFrame r) (DataFrame r)
filter f = do
  (DataFrame df) <- ask
  pure $ DataFrame (A.filter f df)

-- | Group the rows of the DataFrame based on the grouping function.
group :: forall r g. Ord g 
      => (r -> g) 
      -> Query (DataFrame r) (DataFrame {group :: g, data :: DataFrame r})
group f = do
  (DataFrame df) <- ask
  pure $ DataFrame $ _toGroupInfo (M.toUnfoldable (_groups f df))

-- | Count the size of each group of rows in the dataframe.
count :: forall r g. Ord g 
      => (r -> g) 
      -> Query (DataFrame r) (DataFrame {group :: g, count :: Int})
count f = do
  (DataFrame df) <- ask
  pure $ DataFrame $ _toCountInfo (M.toUnfoldable (_groups f df))

-- | Reduce each row to another value and then get an array of that
-- | value. Useful for feeding to functions like maximum, average, etc.
summarize :: forall r x. (r -> x) -> Query (DataFrame r) (Array x)
summarize f = do
  (DataFrame df) <- ask
  pure $ map f df

-- | Change each row of the DataFrame using the conversion function.
-- |
-- | Can be used, e.g., to do column filtering
mutate :: forall r s. (r -> s) -> Query (DataFrame r) (DataFrame s)
mutate f = do
  (DataFrame df) <- ask
  pure $ DataFrame (map f df)

-- | Sort the rows of the dataframe by the given sorting function.
sort :: forall r. (r -> r -> Ordering) -> Query (DataFrame r) (DataFrame r)
sort f = do
  (DataFrame df) <- ask
  pure $ DataFrame (A.sortBy f df)

-- | Only keep all but the first n rows of the DataFrame.
trim :: forall r. Int -> Query (DataFrame r) (DataFrame r)
trim n = do
  (DataFrame df) <- ask
  pure $ DataFrame (A.take n df)

-- | Run 2 queries and return the first input as the context.
chain :: forall r s t. Query r s -> Query s t -> Query r t
chain q1 q2 = do -- TODO: see if there's a better way to do this
  -- TODO: replace this whith >=>
  df <- ask
  let q1res = runReader q1 df
      q2res = runReader q2 q1res
  pure q2res

_groups :: forall a g. Ord g => (a -> g) -> Array a -> M.Map g (Array a)
_groups f xs = M.fromFoldableWith (<>) $ map (\x -> Tuple (f x) [x]) xs

--_groups' :: forall a g. Ord g => (a -> g) -> Array a -> M.Map g (S.Seq a)
--_groups' f xs = M.fromFoldableWith (<>) $ map (\x -> Tuple (f x) (S.singleton x)) xs

--_groups'' :: forall a g. Ord g => (a -> g) -> Array a -> M.Map g (L.List a)
--_groups'' f xs = M.fromFoldableWith (<>) $ map (\x -> Tuple (f x) (L.singleton x)) xs

_toGroupInfo :: forall a g
             .  Array (Tuple g (Array a)) 
             -> Array {group :: g, data :: DataFrame a}
_toGroupInfo = map (\(Tuple gid x) -> {group:gid, data:DataFrame x})

_toCountInfo :: forall a g
             .  Array (Tuple g (Array a)) 
             -> Array {group :: g, count :: Int}
_toCountInfo = map (\(Tuple gid x) -> {group:gid, count:A.length x})

