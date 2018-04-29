{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SuperRecord.TaggedVariant
    ( TaggedVariant
    , TaggedVariantMember
    , emptyTaggedVariant, toTaggedVariant, fromTaggedVariant
    , TaggedVariantMatch(..), TaggedVariantMatcher(..)
    )
where

import SuperRecord.Field

import Data.Maybe
import GHC.Base (Any)
import GHC.TypeLits
import Unsafe.Coerce
import qualified Data.Text as T

data TaggedVariant (opts :: [*])
    = TaggedVariant {-# UNPACK #-} !T.Text Any

type role TaggedVariant representational

instance Show (TaggedVariant '[]) where
    show _ = "<EmptyTaggedVariant>"

instance (KnownSymbol lbl, Show t, Show (TaggedVariant ts)) => Show (TaggedVariant (lbl := t ': ts)) where
    show v1 =
        let w1 :: Maybe t
            w1 = fromTaggedVariant (FldProxy :: FldProxy lbl) v1
        in fromMaybe (show $ shrinkTaggedVariant v1) $ show <$> w1

instance Eq (TaggedVariant '[]) where
    _ == _ = True

instance (KnownSymbol lbl, Eq t, Eq (TaggedVariant ts)) => Eq (TaggedVariant (lbl := t ': ts)) where
    v1 == v2 =
        let w1 :: Maybe t
            w1 = fromTaggedVariant (FldProxy :: FldProxy lbl) v1
            w2 :: Maybe t
            w2 = fromTaggedVariant (FldProxy :: FldProxy lbl) v2
        in fromMaybe (shrinkTaggedVariant v1 == shrinkTaggedVariant v2) $ (==) <$> w1 <*> w2

instance Ord (TaggedVariant '[]) where
    compare _ _ = EQ

instance (KnownSymbol lbl, Ord t, Ord (TaggedVariant ts)) => Ord (TaggedVariant (lbl := t ': ts)) where
    compare v1 v2 =
        let w1 :: Maybe t
            w1 = fromTaggedVariant (FldProxy :: FldProxy lbl) v1
            w2 :: Maybe t
            w2 = fromTaggedVariant (FldProxy :: FldProxy lbl) v2
        in fromMaybe (shrinkTaggedVariant v1 `compare` shrinkTaggedVariant v2) $
           compare <$> w1 <*> w2

type family TaggedVariantMember lbl t opts where
    TaggedVariantMember lbl t (lbl := t ': xs) = 'True ~ 'True
    TaggedVariantMember lbl t (lbl1 := t1 ': ys) = TaggedVariantMember lbl t ys

toTaggedVariant ::
    forall opts lbl a.
    TaggedVariantMember lbl a opts
    => lbl := a -> TaggedVariant opts
toTaggedVariant (proxy := value) =
    TaggedVariant (T.pack $ symbolVal proxy) (unsafeCoerce value)

emptyTaggedVariant :: TaggedVariant '[]
emptyTaggedVariant = TaggedVariant T.empty undefined

fromTaggedVariant ::
    forall opts lbl a.
    (KnownSymbol lbl, TaggedVariantMember lbl a opts)
    => FldProxy lbl -> TaggedVariant opts -> Maybe a
fromTaggedVariant proxy (TaggedVariant tag value) =
    if tag == T.pack (symbolVal proxy)
       then Just (unsafeCoerce value)
       else Nothing

data TaggedVariantMatch r ts where
    TaggedVariantCase ::
        FldProxy lbl -> (t -> r) -> TaggedVariantMatch r ts
        -> TaggedVariantMatch r (lbl := t ':  ts)
    TaggedVariantEnd :: TaggedVariantMatch r '[]
    TaggedVariantWildCard :: r -> TaggedVariantMatch r ts

shrinkTaggedVariant :: TaggedVariant (t ': ts) -> TaggedVariant ts
shrinkTaggedVariant (TaggedVariant tag value) = TaggedVariant tag value

class TaggedVariantMatcher r opts where
   taggedVariantMatch :: TaggedVariant opts -> TaggedVariantMatch r opts -> r

instance (KnownSymbol lbl, TaggedVariantMatcher r ts) => TaggedVariantMatcher r (lbl := t ': ts) where
   taggedVariantMatch v match =
     case match of
       TaggedVariantCase proxy@(FldProxy :: FldProxy lbl) (f :: t -> r) continue ->
          let mValue :: Maybe t
              mValue = fromTaggedVariant proxy v
          in case mValue of
               Just val -> f val
               Nothing -> taggedVariantMatch (shrinkTaggedVariant v) continue
       TaggedVariantWildCard r -> r

instance TaggedVariantMatcher r '[] where
   taggedVariantMatch _ match =
     case match of
       TaggedVariantWildCard r -> r
       TaggedVariantEnd -> error "This should never happen"
