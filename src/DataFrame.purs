
module Data.DataFrame where

import Prelude
import Control.Monad.Reader (Reader, ask, runReader)
import Data.Array as A
--import Data.Sequence as S
--import Data.List as L
--import Data.Map as M
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Tuple (Tuple(..))

import Data.Monoid (class Monoid)

newtype DataFrame r = DataFrame (Array r)

-- The idea of the query result is to maintain the orignal data frame:
-- * with things like histograms and graphs you want the original frame
--   for computing ranges for axes and bin widths
-- * ???
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

-- originally I was thinking a Query would take a data frame as input and
-- return a QueryResult but this isn't really valid in the current scheme.
--type QueryResult r s = ???

init :: forall f r. Foldable f => f r -> DataFrame r
init = DataFrame <<< A.fromFoldable

runQuery :: forall a b. Query a b -> a -> b
runQuery = runReader

rows :: forall r. DataFrame r -> Int
rows (DataFrame df) = A.length df

reset :: forall df. Query df df
reset = ask

filter :: forall r. (r -> Boolean) -> Query (DataFrame r) (DataFrame r)
filter f = do
  (DataFrame df) <- ask
  pure $ DataFrame (A.filter f df)

group :: forall r g. Ord g 
      => (r -> g) 
      -> Query (DataFrame r) (DataFrame {group :: g, data :: DataFrame r})
group f = do
  (DataFrame df) <- ask
  pure $ DataFrame $ _toGroupInfo (M.toUnfoldable (_groups f df))

count :: forall r g. Ord g 
      => (r -> g) 
      -> Query (DataFrame r) (DataFrame {group :: g, count :: Int})
count f = do
  (DataFrame df) <- ask
  pure $ DataFrame $ _toCountInfo (M.toUnfoldable (_groups f df))

summarize :: forall r x. (r -> x) -> Query (DataFrame r) (Array x)
summarize f = do
  (DataFrame df) <- ask
  pure $ map f df

mutate :: forall r s. (r -> s) -> Query (DataFrame r) (DataFrame s)
mutate f = do
  (DataFrame df) <- ask
  pure $ DataFrame (map f df)

trim :: forall r. Int -> Query (DataFrame r) (DataFrame r)
trim n = do
  (DataFrame df) <- ask
  pure $ DataFrame (A.take n df)

chain :: forall r s t. Query r s -> Query s t -> Query r t
chain q1 q2 = do -- TODO: see if there's a better way to do this
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

