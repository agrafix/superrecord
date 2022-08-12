{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SuperRecord.Variant.Tagged
    ( TaggedVariant, toTaggedVariant, fromTaggedVariant
    , taggedVariantCase
    , JsonTaggedVariant(..)
    )
where

import SuperRecord.Field
import SuperRecord.Variant

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Parser, parseFail)
import Data.Maybe
import GHC.TypeLits

#if MIN_VERSION_aeson(2, 0, 0)
import qualified Data.Aeson.Key as Key
#else
import qualified Data.Text as T
#endif

#if MIN_VERSION_aeson(2, 0, 0)
jsonKey :: String -> Key.Key
jsonKey = Key.fromString
#else
jsonKey :: String -> T.Text
jsonKey = T.pack
#endif
{-# INLINE jsonKey #-}

-- | Just a type alias vor 'Variant'
type TaggedVariant opts = Variant opts

-- | Newtype wrapper for  'TaggedVariant' which provides a useful JSON
-- encoding of tagged variants in the form @{"tag": <value>}@
newtype JsonTaggedVariant opts
    = JsonTaggedVariant { unJsonTaggedVariant :: TaggedVariant opts }


instance ToJSON (JsonTaggedVariant '[]) where
    toJSON _ = toJSON ()

instance (KnownSymbol lbl, ToJSON t, ToJSON (JsonTaggedVariant ts)) => ToJSON (JsonTaggedVariant (lbl := t ': ts)) where
    toJSON (JsonTaggedVariant v1) =
        let w1 :: Maybe t
            w1 = fromTaggedVariant (FldProxy :: FldProxy lbl) v1
            tag = jsonKey $ symbolVal (FldProxy :: FldProxy lbl)
        in let val =
                   fromMaybe (toJSON $ JsonTaggedVariant $ shrinkVariant v1) $
                   (\x -> object [tag .= x]) <$> w1
           in val

instance FromJSON (JsonTaggedVariant '[]) where
    parseJSON _ =
        parseFail "There is no JSON value devoid of a value, so no way to represent an emptyVariant"

instance ( FromJSON t, FromJSON (JsonTaggedVariant ts)
         , KnownSymbol lbl
         ) => FromJSON (JsonTaggedVariant (lbl := t ': ts)) where
    parseJSON r =
        do let tag = jsonKey $ symbolVal (FldProxy :: FldProxy lbl)
               myParser :: Parser t
               myParser = withObject ("Tagged " ++ show tag) (\o -> o .: tag) r
               myPackedParser :: Parser (JsonTaggedVariant (lbl := t ': ts))
               myPackedParser =
                   JsonTaggedVariant . toTaggedVariant (FldProxy :: FldProxy lbl) <$>
                   myParser

               nextPackedParser :: Parser (JsonTaggedVariant ts)
               nextPackedParser = parseJSON r
               myNextPackedParser :: Parser (JsonTaggedVariant (lbl := t ': ts))
               myNextPackedParser =
                   JsonTaggedVariant . extendVariant . unJsonTaggedVariant <$> nextPackedParser
           myPackedParser <|> myNextPackedParser

-- | Helper function to construct a tagged variant value given the tag
-- and the value. Note that you can use OverloadedLabels for nicer syntax
-- to construct the 'FldProxy'.
--
-- > toTaggedVariant #myTag "myValue"
toTaggedVariant ::
    forall opts lbl a pos.
    ( KnownSymbol lbl, VariantMember (lbl := a) opts
    , KnownNat pos, VariantPos (lbl := a) opts ~ pos
    )
    => FldProxy lbl -> a -> TaggedVariant opts
toTaggedVariant proxy value = toVariant (proxy := value)

-- | Convert a variant back to a common Haskell type. Returns nothing
-- if the variant is not of the right tag and type.
fromTaggedVariant ::
    forall opts lbl a pos.
    ( KnownSymbol lbl, VariantMember (lbl := a) opts
    , KnownNat pos, VariantPos (lbl := a) opts ~ pos
    )
    => FldProxy lbl -> TaggedVariant opts -> Maybe a
fromTaggedVariant _ variant =
    let loader :: Maybe (lbl := a)
        loader = fromVariant variant
    in case loader of
         Just (_ := r) -> Just r
         Nothing -> Nothing

-- | Nicer syntax for 'VariantCase' for tagged variants.
taggedVariantCase ::
    forall lbl t ts r.
    FldProxy lbl -> (t -> r) -> VariantMatch r ts
    -> VariantMatch r ((lbl := t) ': ts)
taggedVariantCase _ go match =
    let f :: (lbl := t) -> r
        f (_ := x) = go x
    in VariantCase f match
{-# INLINE taggedVariantCase #-}
