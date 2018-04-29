
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import SuperRecord
import SuperRecord.TaggedVariant
import SuperRecord.TextVariant
import SuperRecord.Variant

import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Encoding
import GHC.Generics (Generic)
import Test.Hspec
import qualified Data.Text as T

data V1
data V2
data V3

type TestRecAppend =
    RecAppend '["f1" := V1, "f2" := V2] '["f3" := V3] ~ '["f1" := V1, "f2" := V2, "f3" := V3]

data SomeType
    = SomeType
    { st_foo :: !String
    , st_bar :: !Int
    } deriving (Show, Eq, Generic)

type Ex1 =
    '["foo" := String, "int" := Int]

r1 :: Record Ex1
r1 =
    #foo := "Hi"
    & #int := 213
    & rnil

r2 :: Record '["foo" := String]
r2 = #foo := "He" & rnil

polyFun :: Has "foo" lts String => Rec lts -> String
polyFun = get #foo

polyFun2 :: HasOf '["foo" := String, "bar" := Bool] lts => Rec lts -> String
polyFun2 r =
    get #foo r ++ " -> " ++ show (get #bar r)

rNested :: Record '["foo" := Record '["bar" := Int] ]
rNested =
    #foo := (#bar := 213 & rnil) & rnil

mtlAsk :: (MonadReader (Rec env) m, Has "id" env Int) => m Int
mtlAsk = asksR #id

type BigFieldList =
  '[ "f1" := Int
   , "f2" := Int
   , "f3" := Int
   , "f4" := Int
   , "f5" := Int
   , "f6" := Int
   , "f7" := Int
   , "f8" := Int
   , "f9" := Int
   , "f10" := Int
   ]

bigRec :: Record BigFieldList
bigRec =
    #f1   := 1
  & #f2   := 2
  & #f3   := 3
  & #f4   := 4
  & #f5   := 5
  & #f6   := 6
  & #f7   := 7
  & #f8   := 8
  & #f9   := 9
  & #f10  := 10
  & rnil

main :: TestRecAppend => IO ()
main =
    hspec $
    do recordTests
       variantTests
       taggedVariantTests
       textVariantTests

variantTests :: SpecWith ()
variantTests =
    describe "Variants" $
    do it "works with single element variant" $
           let v :: Variant '[Bool]
               v = toVariant True
           in fromVariant v `shouldBe` Just True
       it "works with multi element variant" $
           let v :: Variant '[Bool, Int]
               v = toVariant (32 :: Int)
           in fromVariant v `shouldBe` Just (32 :: Int)
       it "works with pattern matching" $
           let r :: Variant '[Bool, Int, ()] -> String
               r v =
                   variantMatch v $
                   VariantCase (\x -> if x then "ok" else "no") $
                   VariantCase (\i -> if i > 10 then "oki" else "noi") $
                   VariantCase (\() -> "()")
                   VariantEnd
           in do r (toVariant ()) `shouldBe` "()"
                 r (toVariant (23 :: Int)) `shouldBe` "oki"
                 r (toVariant False) `shouldBe` "no"
       it "works with wildcard pattern matching" $
           let r :: Variant '[Bool, Int, ()] -> String
               r v =
                   variantMatch v $
                   VariantCase (\x -> if x then "ok" else "no") $
                   VariantWildCard "wild"
           in do r (toVariant ()) `shouldBe` "wild"
                 r (toVariant (23 :: Int)) `shouldBe` "wild"
                 r (toVariant False) `shouldBe` "no"
       it "has correct equality" $
           let mkVal :: Int -> Variant '[Bool, Int]
               mkVal = toVariant
           in do mkVal 2 == mkVal 5 `shouldBe` False
                 mkVal 2 == mkVal 2 `shouldBe` True
       it "has correct ord" $
           let mkVal :: Int -> Variant '[Bool, Int]
               mkVal = toVariant
           in do mkVal 2 > mkVal 5 `shouldBe` False
                 mkVal 2 < mkVal 5 `shouldBe` True

taggedVariantTests :: SpecWith ()
taggedVariantTests =
    describe "TaggedVariants" $
    do it "works with single element variant" $
           let v :: TaggedVariant '["foo" := Bool]
               v = toTaggedVariant #foo True
           in fromTaggedVariant #foo v `shouldBe` Just True
       it "works with multi element variant" $
           let v :: TaggedVariant '["foo" := Bool, "bar" := Int]
               v = toTaggedVariant #bar (32 :: Int)
           in fromTaggedVariant #bar v `shouldBe` Just (32 :: Int)
       it "works with pattern matching" $
           let r :: TaggedVariant '["foo" := Bool, "bar" := Int, "baz" := ()] -> String
               r v =
                   taggedVariantMatch v $
                   TaggedVariantCase #foo (\x -> if x then "ok" else "no") $
                   TaggedVariantCase #bar (\i -> if i > 10 then "oki" else "noi") $
                   TaggedVariantCase #baz (\() -> "()")
                   TaggedVariantEnd
           in do r (toTaggedVariant #baz ()) `shouldBe` "()"
                 r (toTaggedVariant #bar (23 :: Int)) `shouldBe` "oki"
                 r (toTaggedVariant #foo False) `shouldBe` "no"
       it "works with wildcard pattern matching" $
           let r :: TaggedVariant '["foo" := Bool, "bar" := Int, "baz" := ()] -> String
               r v =
                   taggedVariantMatch v $
                   TaggedVariantCase #foo (\x -> if x then "ok" else "no") $
                   TaggedVariantWildCard "wild"
           in do r (toTaggedVariant #baz ()) `shouldBe` "wild"
                 r (toTaggedVariant #bar (23 :: Int)) `shouldBe` "wild"
                 r (toTaggedVariant #foo False) `shouldBe` "no"
       it "has correct equality" $
           let mkVal :: Int -> TaggedVariant '["foo" := Bool, "bar" := Int]
               mkVal = toTaggedVariant #bar
           in do mkVal 2 == mkVal 5 `shouldBe` False
                 mkVal 2 == mkVal 2 `shouldBe` True
       it "has correct ord" $
           let mkVal :: Int -> TaggedVariant '["foo" := Bool, "bar" := Int]
               mkVal = toTaggedVariant #bar
           in do mkVal 2 > mkVal 5 `shouldBe` False
                 mkVal 2 < mkVal 5 `shouldBe` True

textVariantTests :: SpecWith ()
textVariantTests =
    describe "TextVariants" $
    do it "works with single element variant" $
           let v :: TextVariant '["foo"]
               v = toTextVariant #foo
           in fromTextVariant v `shouldBe` "foo"
       it "works with multi element variant" $
           let v :: TextVariant '["foo", "bar"]
               v = toTextVariant #bar
           in fromTextVariant v `shouldBe` "bar"
       it "can be built from runtime text" $
           let makeV :: T.Text -> Maybe (TextVariant '["foo", "bar"])
               makeV = buildTextVariant
           in do fmap fromTextVariant (makeV "bar") `shouldBe` Just "bar"
                 fmap fromTextVariant (makeV "foo") `shouldBe` Just "foo"
                 fmap fromTextVariant (makeV "asdasd") `shouldBe` Nothing
       it "works with pattern matching" $
           let r :: TextVariant '["foo", "bar", "baz"] -> String
               r v =
                   textVariantMatch v $
                   TextVariantCase #foo "foo" $
                   TextVariantCase #bar "bar" $
                   TextVariantCase #baz "baz"
                   TextVariantEnd
           in do r (toTextVariant #baz) `shouldBe` "baz"
                 r (toTextVariant #bar) `shouldBe` "bar"
                 r (toTextVariant #foo) `shouldBe` "foo"
       it "works with wildcard pattern matching" $
           let r :: TextVariant '["foo", "bar", "baz"] -> String
               r v =
                   textVariantMatch v $
                   TextVariantCase #foo "foo" $
                   TextVariantWildCard "wild"
           in do r (toTextVariant #baz) `shouldBe` "wild"
                 r (toTextVariant #bar) `shouldBe` "wild"
                 r (toTextVariant #foo) `shouldBe` "foo"
       it "has correct equality" $
           let val1 :: TextVariant '["foo", "bar"]
               val1 = toTextVariant #bar
               val2 :: TextVariant '["foo", "bar"]
               val2 = toTextVariant #foo
           in do val1 == val2 `shouldBe` False
                 val1 == val1 `shouldBe` True
       it "has correct ord" $
           let val1 :: TextVariant '["foo", "bar"]
               val1 = toTextVariant #bar
               val2 :: TextVariant '["foo", "bar"]
               val2 = toTextVariant #foo
           in do val1 > val2 `shouldBe` False
                 val1 < val2 `shouldBe` True

recordTests :: SpecWith ()
recordTests =
    describe "Records" $
    do it "getter works" $
           do get #foo r1 `shouldBe` "Hi"
              get #int r1 `shouldBe` 213
              polyFun r1 `shouldBe` "Hi"
              polyFun r2 `shouldBe` "He"
              get #bar (get #foo rNested) `shouldBe` 213
              rNested &. #foo &. #bar `shouldBe` 213
              getPath (#foo &:- #bar) rNested `shouldBe` 213
       it "hasOf works" $
           polyFun2 (#foo := "123" & #bar := True & #bim := False & rnil) `shouldBe` "123 -> True"
       it "setter works" $
           do let r1u = set #foo "Hey" r1
              get #foo r1 `shouldBe` "Hi"
              get #foo r1u `shouldBe` "Hey"
              get #int (set #int 123 r1) `shouldBe` 123
              set #int 213 (set #int 123 r1) `shouldBe` r1
              setPath (#foo &:- #bar) 123 rNested
                  `shouldBe` (#foo := (#bar := 123 & rnil) & rnil)
              modifyPath (#foo &:- #bar) (+1) rNested
                  `shouldBe` (#foo := (#bar := 214 & rnil) & rnil)
       it "modify works" $
           do let r1u = modify #foo (\x -> x ++ "!") r1
              get #foo r1 `shouldBe` "Hi"
              get #foo r1u `shouldBe` "Hi!"
       it "getting record keys works" $
           do let vals = recKeys r1
              vals `shouldBe` ["foo", "int"]
       it "fromNative works" $
           do let r = fromNative (SomeType "hello" 123)
              get #st_foo r `shouldBe` "hello"
              get #st_bar r `shouldBe` 123
       it "toNative works" $
           do let ra = (#st_foo := "hello" & #st_bar := 123 & rnil)
              toNative ra `shouldBe` SomeType "hello" 123
              let rb = (#st_bar := 123 & #st_foo := "hello" & rnil)
              toNative rb `shouldBe` SomeType "hello" 123
              let rc = (#other := True & #st_bar := 123 & #st_foo := "hello" & rnil)
              toNative rc `shouldBe` SomeType "hello" 123
       it "can be constructed in any order" $
           do let areEq =
                      (#foo := True & #bar := False & rnil)
                      == (#bar := False & #foo := True & rnil)
              areEq `shouldBe` True
              let areNotEq =
                      (#foo := False & #bar := False & rnil)
                      == (#bar := False & #foo := True & rnil)
              areNotEq `shouldBe` False
       it "combine works" $
           do let rc = r1 ++: (#bar := True & rnil)
              rc &. #foo `shouldBe` "Hi"
              rc &. #int `shouldBe` 213
              rc &. #bar `shouldBe` True
              rc `shouldBe` (#foo := "Hi" & #int := 213 & #bar := True & rnil)
       it "combine works 2" $
           do let rc = r1 ++: (#bim := 123 & #fizz := "Hoy" & #bar := True & rnil)
              rc &. #foo `shouldBe` "Hi"
              rc &. #int `shouldBe` 213
              rc &. #bar `shouldBe` True
              rc &. #fizz `shouldBe` ("Hoy" :: String)
              rc &. #bim `shouldBe` (123 :: Int)
              rc `shouldBe`
                  (#foo := "Hi" & #int := 213 & #bim := 123 & #fizz := "Hoy" & #bar := True & rnil)
       it "showRec words" $
           do let vals = showRec r1
              vals `shouldBe` [("foo", "\"Hi\""), ("int", "213")]
       it "show works" $
           show r1 `shouldBe` "[(\"foo\",\"\\\"Hi\\\"\"),(\"int\",\"213\")]"
       it "equality works" $
           do r1 == r1 `shouldBe` True
              r1 == set #foo "Hai" r1 `shouldBe` False
       it "toJSON matches fromJSON" $
           do decode (encode r1) `shouldBe` Just r1
              decode (encode r2) `shouldBe` Just r2
              decode (encode rNested) `shouldBe` Just rNested
              decode "{\"foo\": true}" `shouldBe` Just (#foo := True & rnil)
       let r1JSON = object ["foo" .= ("Hi" :: String), "int" .= (213 :: Int)]
       it "toJSON produces an object" $
           toJSON r1 `shouldBe` r1JSON
       it "toEncoding produces an object" $
           decode (encodingToLazyByteString (toEncoding r1)) `shouldBe`
               Just r1JSON
       it "parseJSON parses an object" $
           decode "{\"foo\": \"Hi\", \"int\": 213}" `shouldBe` Just r1
       it "reader works" $
           do runReaderT mtlAsk (#id := 123 & rnil) `shouldReturn` 123
