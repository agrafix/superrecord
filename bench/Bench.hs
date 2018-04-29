{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import SuperRecord
import SuperRecord.TaggedVariant
import SuperRecord.TextVariant
import SuperRecord.Variant

import Criterion
import Criterion.Main

import Control.DeepSeq
import Data.Aeson
import GHC.Generics
import GHC.TypeLits
import qualified Bookkeeper as B
import qualified Data.Text as T
import qualified Labels as L

data Nested
    = Nested
    { n_f41 :: String
    } deriving (Read, Generic)

instance NFData Nested
instance ToJSON Nested
instance FromJSON Nested

data Native
    = Native
    { n_f1 :: String
    , n_f2 :: Int
    , n_f3 :: Bool
    , n_f4 :: Nested
    } deriving (Read, Generic)

instance NFData Native
instance ToJSON Native
instance FromJSON Native

someIntList :: [Int]
someIntList =
    read "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]"

data CList a
    = CList
    { cl_list :: !(Maybe (a, CList a))
    } deriving (Show, Eq)

toCList :: [a] -> CList a
toCList [] = CList Nothing
toCList (x : xs) = CList (Just (x, toCList xs))

idxC :: Int -> CList a -> Maybe a
idxC !i c
    | i <= 0 = fst <$> cl_list c
    | otherwise =
          case cl_list c of
            Just (_, xs) -> idxC (i - 1) xs
            Nothing -> Nothing

newtype RList a
    = RList { unRlist :: Rec '[ "list" := Maybe (a, RList a) ] }

toRList :: [a] -> RList a
toRList [] = RList $ #list := Nothing & rnil
toRList (x : xs) = RList $ #list := Just (x, toRList xs) & rnil

idxR :: Int -> RList a -> Maybe a
idxR !i (RList r)
    | i <= 0 = fst <$> get #list r
    | otherwise =
          case get #list r of
            Just (_, xs) -> idxR (i - 1) xs
            Nothing -> Nothing

type Ex1 =
    '[ "f1" := String
     , "f2" := Int
     , "f3" := Bool
     , "f4" := Rec '[ "f41" := String ]
     ]

r1 :: Rec Ex1
r1 =
    #f1 := "Hi"
    & #f2 := 213
    & #f3 := True
    & #f4 := (#f41 := "abc" & rnil)
    & rnil


deriving instance (KnownSymbol a, Read b) => Read ((L.:=) a b )

r1L ::
    ( "f1" L.:= String
    , "f2" L.:= Int
    , "f3" L.:= Bool
    , "f4" L.:= ("f41" L.:= String)
    )
r1L =
    -- needed to prevent unrealistic inlining (normally, data comes from the real world and is
    -- not statically known at compile time)
    read "(Proxy := \"Hi\", Proxy := 213, Proxy := True, Proxy := (Proxy := \"abc\"))"

r1B ::
    B.Book
    '[ "f1" B.:=> String
     , "f2" B.:=> Int
     , "f3" B.:=> Bool
     , "f4" B.:=> B.Book '[ "f41" B.:=> String ]
     ]
r1B =
    B.emptyBook
    B.& #f1 B.=: "Hi"
    B.& #f2 B.=: 213
    B.& #f3 B.=: True
    B.& #f4 B.=: (B.emptyBook B.& #f41 B.=: "abc")

r1N :: Native
r1N =
    -- needed to prevent unrealistic inlining (normally, data comes from the real world and is
    -- not statically known at compile time)
    read "Native { n_f1 = \"Hi\", n_f2 = 213, n_f3 = True, n_f4 = Nested { n_f41 = \"Hi\"} }"

data NativeSumType
    = SBool Bool
    | SInt Int
    | SStr String
    deriving (Show, Read, Eq, Generic)

instance NFData NativeSumType

nativeSumType :: NativeSumType
nativeSumType = read "SStr \"fooo\"" -- see above why we read here.

nativeSumTypeFun :: NativeSumType -> Int
nativeSumTypeFun st =
    case st of
      SBool x -> if x then 1 else 0
      SInt i -> i
      SStr s -> length s

type TaggedSumType = TaggedVariant '["sbool" := Bool, "sint" := Int, "sstr" := String]

taggedSumType :: TaggedSumType
taggedSumType = toTaggedVariant #sstr ("fooo" :: String)

taggedSumTypeFun :: TaggedSumType -> Int
taggedSumTypeFun st =
    taggedVariantMatch st $
    TaggedVariantCase #sbool (\x -> if x then 1 else 0) $
    TaggedVariantCase #sint (\i -> i) $
    TaggedVariantCase #sstr (\s -> length s) $
    TaggedVariantEnd

type VariantSumType = Variant '[Bool, Int, String]

variantSumType :: VariantSumType
variantSumType = toVariant ("fooo" :: String)

variantSumTypeFun :: VariantSumType -> Int
variantSumTypeFun st =
    variantMatch st $
    VariantCase (\x -> if x then 1 else 0) $
    VariantCase (\i -> i) $
    VariantCase (\s -> length s) $
    VariantEnd

type VariantTextSumType = TextVariant '["foo", "bar", "baz"]

variantTextSumType :: VariantTextSumType
variantTextSumType = toTextVariant #baz

variantTextSumTypeFun :: VariantTextSumType -> Int
variantTextSumTypeFun st =
    textVariantMatch st $
    TextVariantCase #foo 1 $
    TextVariantCase #bar 2 $
    TextVariantCase #baz 3 $
    TextVariantEnd

nativeTextSumType :: T.Text
nativeTextSumType = read "\"baz\"" -- see above why reading here.

nativeTextSumTypeFun :: T.Text -> Int
nativeTextSumTypeFun st =
    case st of
      "foo" -> 1
      "bar" -> 2
      "baz" -> 3
      _ -> 0

main :: IO ()
main =
    defaultMain
    [ bgroup "record" recordBench
    , bgroup "variant" variantBench
    , bgroup "text-variant" textVariantBench
    ]

textVariantBench =
    [ bench "native" $ nf nativeTextSumTypeFun nativeTextSumType
    , bench "text" $ nf variantTextSumTypeFun variantTextSumType
    ]

variantBench =
    [ bench "native" $ nf nativeSumTypeFun nativeSumType
    , bench "tagged" $ nf taggedSumTypeFun taggedSumType
    , bench "variant" $ nf variantSumTypeFun variantSumType
    ]

recordBench =
    [ bgroup "get"
        [ bench "superrecord" $ nf (get #f2) r1
        , bench "labels" $ nf (L.get #f2) r1L
        , bench "bookkeeper" $ nf (\r -> r B.?: #f2) r1B
        , bench "native" $ nf n_f2 r1N
        ]
    , bgroup "get nested"
        [ bench "superrecord get" $ nf (get #f41 . get #f4) r1
        , bench "superrecord getPath" $ nf (getPath (#f4 &:- #f41)) r1
        , bench "labels" $ nf (L.get #f41 . L.get #f4) r1L
        , bench "bookkeeper" $ nf (\r -> r B.?: #f4 B.?: #f41) r1B
        , bench "native" $ nf (n_f41 . n_f4) r1N
        ]
    , bgroup "set nested"
        [ bench "superrecord" $
            nf (\r -> (setPath (#f4 &:- #f41) "Hello" r) &. #f4 &. #f41) r1
        , bench "labels" $
            nf (\r -> L.get #f41 . L.get #f4 $ L.modify #f4 (L.set #f41 "Hello") r) r1L
        , bench "bookkeeper" $
            nf (\r -> (r B.& #f4 B.%: (\s -> s B.& #f41 B.%: const ("Hello" :: String))) B.?: #f4 B.?: #f41) r1B
        , bench "native" $
            nf (\r -> n_f41 $ n_f4 (r { n_f4 = (n_f4 r) { n_f41 = "Hello" } })) r1N
        ]
    , bgroup "set get"
        [ bench "superrecord" $ nf (get #f2 . set #f2 123) r1
        , bench "labels" $ nf (L.get #f2 . L.set #f2 123) r1L
        , bench "bookkeeper" $ nf (\r -> (r B.& #f2 B.%: const (123 :: Int)) B.?: #f2) r1B
        , bench "native" $ nf (\r -> n_f2 (r { n_f2 = 123 })) r1N
        ]
    , bgroup "set rec"
        [ bench "superrecord" $ nf (set #f2 123) r1
        , bench "native" $ nf (\r -> r { n_f2 = 123 }) r1N
        ]
    , bgroup "combine rec"
        [ bench "superrecord" $ nf (\r -> r ++: (#foo := True & rnil)) r1
        ]
    , bgroup "json"
        [ bench "superrecord" $ nf @[Rec Ex1] (throwOnNone . decode' . encode) $ replicate 50 r1
        , bench "native" $ nf @[Native] (throwOnNone . decode' . encode) $ replicate 50 r1N
        ]
    , bgroup "dummy list"
        [ bench "superrecord" $ nf (idxR 9 . toRList) someIntList
        , bench "native" $ nf (idxC 9 . toCList) someIntList
        ]
    ]

throwOnNone :: Maybe a -> a
throwOnNone (Just x) = x
throwOnNone _ = error "What?!"
