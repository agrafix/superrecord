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
module SuperRecord.Variant
    ( Variant
    , VariantMember, VariantPos
    , emptyVariant, toVariant, fromVariant
    , VariantMatch(..), VariantMatcher(..)
    )
where

import Control.Applicative
import Control.DeepSeq
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe
import Data.Proxy
import GHC.Base (Any)
import GHC.TypeLits
import Unsafe.Coerce

data Variant (opts :: [*])
    = Variant {-# UNPACK #-} !Word Any

type role Variant representational

instance NFData (Variant '[]) where
    rnf (Variant x _) = x `deepseq` ()

instance (NFData t, NFData (Variant ts)) => NFData (Variant (t ': ts)) where
    rnf v1 =
        let w1 :: Maybe t
            w1 = fromVariant v1
        in w1 `deepseq` shrinkVariant v1 `deepseq` ()

instance ToJSON (Variant '[]) where
    toJSON _ = toJSON ()

instance (ToJSON t, ToJSON (Variant ts)) => ToJSON (Variant (t ': ts)) where
    toJSON v1 =
        let w1 :: Maybe t
            w1 = fromVariant v1
        in fromMaybe (toJSON $ shrinkVariant v1) $ toJSON <$> w1

instance FromJSON (Variant '[]) where
    parseJSON r =
        do () <- parseJSON r
           pure emptyVariant

instance ( FromJSON t, FromJSON (Variant ts)
         ) => FromJSON (Variant (t ': ts)) where
    parseJSON r =
        do let myParser :: Parser t
               myParser = parseJSON r
               myPackedParser :: Parser (Variant (t ': ts))
               myPackedParser = toVariant <$> myParser
               nextPackedParser :: Parser (Variant ts)
               nextPackedParser = parseJSON r
               lift (Variant t v) = (Variant (t + 1) v)
               myNextPackedParser :: Parser (Variant (t ': ts))
               myNextPackedParser = lift <$> nextPackedParser
           myPackedParser <|> myNextPackedParser

instance Show (Variant '[]) where
    show _ = "<EmptyVariant>"

instance (Show t, Show (Variant ts)) => Show (Variant (t ': ts)) where
    show v1 =
        let w1 :: Maybe t
            w1 = fromVariant v1
        in fromMaybe (show $ shrinkVariant v1) $ show <$> w1

instance Eq (Variant '[]) where
    _ == _ = True

instance (Eq t, Eq (Variant ts)) => Eq (Variant (t ': ts)) where
    v1 == v2 =
        let w1 :: Maybe t
            w1 = fromVariant v1
            w2 :: Maybe t
            w2 = fromVariant v2
        in fromMaybe (shrinkVariant v1 == shrinkVariant v2) $ (==) <$> w1 <*> w2

instance Ord (Variant '[]) where
    compare _ _ = EQ

instance (Ord t, Ord (Variant ts)) => Ord (Variant (t ': ts)) where
    compare v1 v2 =
        let w1 :: Maybe t
            w1 = fromVariant v1
            w2 :: Maybe t
            w2 = fromVariant v2
        in fromMaybe (shrinkVariant v1 `compare` shrinkVariant v2) $
           compare <$> w1 <*> w2

type family VariantMember a opts where
    VariantMember x (x ': xs) = 'True ~ 'True
    VariantMember x (y ': ys) = VariantMember x ys


type family VariantPosHelper idx a opts where
    VariantPosHelper idx x (x ': xs) = idx
    VariantPosHelper idx x (y ': xs) = VariantPosHelper (idx + 1) x xs

type VariantPos a opts = VariantPosHelper 1 a opts

toVariant ::
    forall opts a pos.
    (KnownNat pos, VariantPos a opts ~ pos, VariantMember a opts)
    => a -> Variant opts
toVariant x = Variant (fromIntegral $ natVal (Proxy :: Proxy pos)) (unsafeCoerce x)

emptyVariant :: Variant '[]
emptyVariant = Variant 0 undefined

fromVariant ::
    forall opts a pos.
    (KnownNat pos, VariantPos a opts ~ pos, VariantMember a opts)
    => Variant opts -> Maybe a
fromVariant (Variant tag value) =
    if tag == check
       then Just (unsafeCoerce value)
       else Nothing
    where
      check = fromIntegral $ natVal (Proxy :: Proxy pos)

data VariantMatch r ts where
    VariantCase :: (t -> r) -> VariantMatch r ts -> VariantMatch r (t ':  ts)
    VariantEnd :: VariantMatch r '[]
    VariantWildCard :: r -> VariantMatch r ts

shrinkVariant :: Variant (t ': ts) -> Variant ts
shrinkVariant (Variant tag value) = Variant (tag - 1) value

class VariantMatcher r opts where
   variantMatch :: Variant opts -> VariantMatch r opts -> r

instance VariantMatcher r ts => VariantMatcher r (t ': ts) where
   variantMatch v match =
     case match of
       VariantCase (f :: t -> r) continue ->
          let mValue :: Maybe t
              mValue = fromVariant v
          in case mValue of
               Just val -> f val
               Nothing -> variantMatch (shrinkVariant v) continue
       VariantWildCard r -> r

instance VariantMatcher r '[] where
   variantMatch _ match =
     case match of
       VariantWildCard r -> r
       VariantEnd -> error "This should never happen"
