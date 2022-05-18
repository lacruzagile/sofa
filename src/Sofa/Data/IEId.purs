module Sofa.Data.IEId
  ( IEId(..)
  , genInternalId
  , genInternalId'
  , toExternalId
  , toRawId
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.UUID (UUID, genUUID, genv5UUID)
import Data.UUID as UUID
import Effect (Effect)

-- | An identifier that can be either "internal" or "external", an internal ID
-- | is one generated and used only within the app while an external ID is
-- | provided externally.
-- |
-- | For example, until we save the order we won't have any external ID
-- | available for an order section. Therefore, to still be able to refer to the
-- | section we generate an internal ID until the order is saved, at which point
-- | we switch over to use the external ID.
data IEId a
  = InternalId a
  | ExternalId a

derive instance eqIEId :: Eq a => Eq (IEId a)

instance ordIEId :: Ord a => Ord (IEId a) where
  compare (InternalId _) (ExternalId _) = LT
  compare (ExternalId _) (InternalId _) = GT
  compare (InternalId a) (InternalId b) = compare a b
  compare (ExternalId a) (ExternalId b) = compare a b

-- | Generate an internal ID, in the form of a v4 UUID.
genInternalId :: forall a. (String -> a) -> Effect (IEId a)
genInternalId f = (InternalId <<< f <<< UUID.toString) <$> genUUID

-- | Generate an internal ID, in the form of a v5 UUID.
genInternalId' :: forall a. (String -> a) -> String -> UUID -> IEId a
genInternalId' f s ns = InternalId $ f $ UUID.toString $ genv5UUID s ns

-- | Whether the given ID matches either the given internal or external ID.
eqEither :: forall a. Eq a => a -> IEId a -> Boolean
eqEither x = case _ of
  InternalId y -> x == y
  ExternalId y -> x == y

-- | Whether the given ID matches either the given internal or external ID inside.
hasId :: forall a r. Eq a => a -> { id :: IEId a | r } -> Boolean
hasId x { id } = case id of
  InternalId y -> x == y
  ExternalId y -> x == y

-- | If the given ID is external, then return just that ID, otherwise return
-- | nothing.
toExternalId :: forall a. IEId a -> Maybe a
toExternalId = case _ of
  InternalId _ -> Nothing
  ExternalId id -> Just id

-- | Extracts the raw ID, ignoring whether it is internal or external.
toRawId :: forall a. IEId a -> a
toRawId = case _ of
  InternalId id -> id
  ExternalId id -> id
