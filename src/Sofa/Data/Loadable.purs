module Sofa.Data.Loadable
  ( Loadable(..)
  , isIdle
  , isLoaded
  , isLoading
  , toMaybe
  ) where

import Prelude
import Data.Maybe (Maybe(..))

-- | A representation of URL data that is loaded. The load can, e.g., fail with
-- | an error message.
data Loadable a
  = Idle
  | Loaded a
  | Loading
  | Error String

derive instance functorLoadable :: Functor Loadable

instance applyLoadable :: Apply Loadable where
  apply Idle _ = Idle
  apply (Loaded f) r = f <$> r
  apply Loading _ = Loading
  apply (Error err) _ = Error err

instance applicationLoadable :: Applicative Loadable where
  pure = Loaded

instance bindLoadable :: Bind Loadable where
  bind Idle _ = Idle
  bind (Loaded x) f = f x
  bind Loading _ = Loading
  bind (Error err) _ = Error err

-- | Whether the given loadable is in an idle state.
isIdle :: forall a. Loadable a -> Boolean
isIdle = case _ of
  Idle -> true
  _ -> false

-- | Whether the given loadable is in a loading state.
isLoading :: forall a. Loadable a -> Boolean
isLoading = case _ of
  Loading -> true
  _ -> false

-- | Whether the given loadable is in a loading state.
isLoaded :: forall a. Loadable a -> Boolean
isLoaded = case _ of
  Loaded _ -> true
  _ -> false

-- | Converts the given loadable into a maybe value. A `Loaded` value is
-- | converted to a `Just`, all other loadable values are converted to
-- | `Nothing`.
toMaybe :: forall a. Loadable a -> Maybe a
toMaybe = case _ of
  Loaded x -> Just x
  _ -> Nothing
