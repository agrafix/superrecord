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
    , emptyTaggedVariant, toTaggedVariant, toTaggedVariant', fromTaggedVariant
    , TaggedVariantMatch(..), TaggedVariantMatcher(..)
    )
where

import SuperRecord.Field

import Control.Applicative
import Control.DeepSeq
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Internal (c2w)
import Data.Maybe
import GHC.Base (Any)
import GHC.TypeLits
import Unsafe.Coerce
import qualified Data.ByteString.Short as BSS
import qualified Data.Text as T

data TaggedVariant (opts :: [*])
    = TaggedVariant {-# UNPACK #-} !BSS.ShortByteString Any

type role TaggedVariant representational

instance NFData (TaggedVariant '[]) where
    rnf (TaggedVariant x _) = x `deepseq` ()

instance (KnownSymbol lbl, NFData t, NFData (TaggedVariant ts)) => NFData (TaggedVariant (lbl := t ': ts)) where
    rnf v1 =
        let w1 :: Maybe t
            w1 = fromTaggedVariant (FldProxy :: FldProxy lbl) v1
        in w1 `deepseq` shrinkTaggedVariant v1 `deepseq` ()

instance ToJSON (TaggedVariant '[]) where
    toJSON _ = toJSON ()

instance (KnownSymbol lbl, ToJSON t, ToJSON (TaggedVariant ts)) => ToJSON (TaggedVariant (lbl := t ': ts)) where
    toJSON v1 =
        let w1 :: Maybe t
            w1 = fromTaggedVariant (FldProxy :: FldProxy lbl) v1
            tag = T.pack $ symbolVal (FldProxy :: FldProxy lbl)
        in let val =
                   fromMaybe (toJSON $ shrinkTaggedVariant v1) $
                   (\x -> object [tag .= x]) <$> w1
           in val

instance FromJSON (TaggedVariant '[]) where
    parseJSON r =
        do () <- parseJSON r
           pure emptyTaggedVariant

instance ( FromJSON t, FromJSON (TaggedVariant ts)
         , KnownSymbol lbl
         ) => FromJSON (TaggedVariant (lbl := t ': ts)) where
    parseJSON r =
        do let tag = T.pack $ symbolVal (FldProxy :: FldProxy lbl)
               myParser :: Parser t
               myParser = withObject ("Tagged " ++ show tag) (\o -> o .: tag) r
               myPackedParser :: Parser (TaggedVariant (lbl := t ': ts))
               myPackedParser = toTaggedVariant (FldProxy :: FldProxy lbl) <$> myParser
               nextPackedParser :: Parser (TaggedVariant ts)
               nextPackedParser = parseJSON r
               lift (TaggedVariant t v) = (TaggedVariant t v)
               myNextPackedParser :: Parser (TaggedVariant (lbl := t ': ts))
               myNextPackedParser = lift <$> nextPackedParser
           myPackedParser <|> myNextPackedParser

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
    (KnownSymbol lbl, TaggedVariantMember lbl a opts)
    => FldProxy lbl -> a -> TaggedVariant opts
toTaggedVariant proxy value =
    TaggedVariant (BSS.pack $ map c2w $ symbolVal proxy) (unsafeCoerce value)

toTaggedVariant' ::
    forall opts lbl a.
    (TaggedVariantMember lbl a opts)
    => lbl := a -> TaggedVariant opts
toTaggedVariant' (proxy := value) =
    toTaggedVariant proxy value

emptyTaggedVariant :: TaggedVariant '[]
emptyTaggedVariant = TaggedVariant BSS.empty undefined

fromTaggedVariant ::
    forall opts lbl a.
    (KnownSymbol lbl, TaggedVariantMember lbl a opts)
    => FldProxy lbl -> TaggedVariant opts -> Maybe a
fromTaggedVariant proxy (TaggedVariant tag value) =
    if tag == BSS.pack (map c2w (symbolVal proxy))
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
