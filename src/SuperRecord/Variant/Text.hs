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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SuperRecord.Variant.Text
    ( TextVariant
    , TextVariantMember
    , emptyTextVariant, toTextVariant, fromTextVariant
    , TextVariantMatch(..), TextVariantMatcher(..)
    , TextVariantBuilder(..)
    )
where

import SuperRecord.Field

import Control.DeepSeq
import Data.Aeson
import GHC.TypeLits
import qualified Data.Text as T

-- | A text only variant: A wrapped 'T.Text' that can only be
-- one of the given values tracked at type level. Very useful
-- for interacting with enum-like string in JSON APIs.
newtype TextVariant (opts :: [Symbol])
    = TextVariant T.Text
    deriving (Show, Eq, Ord, NFData)

instance ToJSON (TextVariant opts) where
    toJSON = toJSON . fromTextVariant

instance TextVariantBuilder opts => FromJSON (TextVariant opts) where
    parseJSON x =
        do r <- parseJSON x
           let go txt =
                   case buildTextVariant txt of
                     Nothing -> fail ("Invalid text variant value: " ++ show txt)
                     Just ok -> pure ok
           withText "TextVariant" go r

type role TextVariant representational

type family TextVariantMember (lbl :: Symbol) (opts :: [Symbol]) where
    TextVariantMember lbl (lbl ': xs) = 'True ~ 'True
    TextVariantMember lbl (lbl1 ': ys) = TextVariantMember lbl ys

-- | Create a 'TextVariant' value from a statically known string. Use
-- OverloadedLabels for nice syntax: @toTextVariant #myString@
toTextVariant ::
    forall opts lbl.
    (KnownSymbol lbl, TextVariantMember lbl opts)
    => FldProxy lbl -> TextVariant opts
toTextVariant proxy =
    TextVariant (T.pack $ symbolVal proxy)

-- | An empty 'TextVariant', equivalent to `()`
emptyTextVariant :: TextVariant '[]
emptyTextVariant = TextVariant mempty

-- | Convert a 'TextVariant' back to a normal 'T.Text'. This operation
-- is cheap since 'TextVariant' is a simple newtype.
fromTextVariant :: TextVariant opts -> T.Text
fromTextVariant (TextVariant val) = val

data TextVariantMatch r ts where
    TextVariantCase ::
        FldProxy lbl -> r -> TextVariantMatch r ts -> TextVariantMatch r (lbl ':  ts)
    TextVariantEnd :: TextVariantMatch r '[]
    TextVariantWildCard :: r -> TextVariantMatch r ts

shrinkTextVariant :: TextVariant (t ': ts) -> TextVariant ts
shrinkTextVariant (TextVariant tag) = TextVariant tag

-- | Pattern matching helper with totality check. Note that the performance
-- of this pattern match is roughly like a normal pattern match. (See benchmarks)
class TextVariantMatcher r opts where
   textVariantMatch :: TextVariant opts -> TextVariantMatch r opts -> r

instance (KnownSymbol lbl, TextVariantMatcher r ts) => TextVariantMatcher r (lbl ': ts) where
   textVariantMatch v@(TextVariant tag) match =
     case match of
       TextVariantCase proxy@(FldProxy :: FldProxy lbl) r continue ->
          if T.pack (symbolVal proxy) == tag
          then r
          else textVariantMatch (shrinkTextVariant v) continue
       TextVariantWildCard r -> r

instance TextVariantMatcher r '[] where
   textVariantMatch _ match =
     case match of
       TextVariantWildCard r -> r
       TextVariantEnd -> error "This should never happen"

-- | Build a variant from a text that is not statically known at compile time.
-- Returns 'Nothing' on failure (i.E. when a value is given that is not part of
-- the variant)
class TextVariantBuilder opts where
   buildTextVariant :: T.Text -> Maybe (TextVariant opts)

instance (KnownSymbol lbl, TextVariantBuilder ts)
    => TextVariantBuilder (lbl ': ts) where
   buildTextVariant text =
     let tag = T.pack (symbolVal (FldProxy :: FldProxy lbl))
     in if tag == text
        then Just (TextVariant tag)
        else let nextCheck :: T.Text -> Maybe (TextVariant ts)
                 nextCheck = buildTextVariant
             in case nextCheck text of
                  Nothing -> Nothing
                  Just _ -> Just (TextVariant text)

instance TextVariantBuilder '[] where
   buildTextVariant _ = Nothing
