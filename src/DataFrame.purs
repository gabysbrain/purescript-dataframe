
module DataFrame where

import Prelude
import Control.Monad.Reader (Reader, ask, runReader)
import Data.Array as A
import Data.Map as M
import Data.Tuple (Tuple(..))

newtype DataFrame r = DataFrame (Array r)

-- The idea of the query result is to maintain the orignal data frame:
-- * with things like histograms and graphs you want the original frame
--   for computing ranges for axes and bin widths
-- * ???
type Query df r = Reader df r

-- originally I was thinking a Query would take a data frame as input and
-- return a QueryResult but this isn't really valid in the current scheme.
--type QueryResult r s = ???

rows :: forall r. DataFrame r -> Int
rows (DataFrame df) = A.length df

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

chain :: forall r s t. Query r s -> Query s t -> Query r t
chain q1 q2 = do -- TODO: see if there's a better way to do this
  df <- ask
  let q1res = runReader q1 df
      q2res = runReader q2 q1res
  pure q2res

--original :: forall r s. Query r s -> r
--original = do
  --df <- ask
  --pure $ df

_groups :: forall a g. Ord g => (a -> g) -> Array a -> M.Map g (Array a)
_groups f xs = M.fromFoldableWith (<>) $ map (\x -> Tuple (f x) [x]) xs

_toGroupInfo :: forall a g
             .  Array (Tuple g (Array a)) 
             -> Array {group :: g, data :: DataFrame a}
_toGroupInfo = map (\(Tuple gid x) -> {group:gid, data:DataFrame x})

_toCountInfo :: forall a g
             .  Array (Tuple g (Array a)) 
             -> Array {group :: g, count :: Int}
_toCountInfo = map (\(Tuple gid x) -> {group:gid, count:A.length x})

