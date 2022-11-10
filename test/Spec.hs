
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import SuperRecord
import SuperRecord.Variant
import SuperRecord.Variant.Tagged
import SuperRecord.Variant.Text

import Control.Monad.Reader
import Control.Monad.State (State, evalState)
import qualified Control.Monad.State as State
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

polyFun :: Has lts "foo" String => Rec lts -> String
polyFun = get #foo

polyFun2 :: HasOf '["foo" := String, "bar" := Bool] lts => Rec lts -> String
polyFun2 r =
    get #foo r ++ " -> " ++ show (get #bar r)

rNested :: Record '["foo" := Record '["bar" := Int] ]
rNested =
    #foo := (#bar := 213 & rnil) & rnil

mtlAsk :: (MonadReader (Rec env) m, Has env "id" Int) => m Int
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

type NumFields = '[ "int" := Int, "double" := Double, "word" := Word, "float" := Float ]

numRec :: Rec NumFields
numRec = #int    := (-3)   `unsafeRCons`
         #double := 7.5    `unsafeRCons`
         #word   := 256    `unsafeRCons`
         #float  := (-3/4) `unsafeRCons`
         ( unsafeRNil 4 )

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
       it "serialises to JSON" $
           do encode ((toVariant (123 :: Int)) :: Variant '[Bool, Int])
                  `shouldBe` "123"
              encode ((toVariant True) :: Variant '[Bool, Int])
                  `shouldBe` "true"
       it "parses from JSON" $
           do decode "true" `shouldBe`
                  ((Just $ toVariant True) :: Maybe (Variant '[Bool, Int]))
              decode "123" `shouldBe`
                  (Just ((toVariant (123 :: Int)) :: (Variant '[Bool, Int])))
              decode "\"foo\"" `shouldBe`
                  (Nothing :: Maybe (Variant '[Bool, Int]))
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
           let v :: Variant '["foo" := Bool]
               v = toTaggedVariant #foo True
           in fromTaggedVariant #foo v `shouldBe` Just True
       it "works with multi element variant" $
           let v :: Variant '["foo" := Bool, "bar" := Int]
               v = toTaggedVariant #bar (32 :: Int)
           in fromTaggedVariant #bar v `shouldBe` Just (32 :: Int)
       it "works with pattern matching" $
           let r :: Variant '["foo" := Bool, "bar" := Int, "baz" := ()] -> String
               r v =
                   variantMatch v $
                   taggedVariantCase #foo (\x -> if x then "ok" else "no") $
                   taggedVariantCase #bar (\i -> if i > 10 then "oki" else "noi") $
                   taggedVariantCase #baz (\() -> "()")
                   VariantEnd
           in do r (toTaggedVariant #baz ()) `shouldBe` "()"
                 r (toTaggedVariant #bar (23 :: Int)) `shouldBe` "oki"
                 r (toTaggedVariant #foo False) `shouldBe` "no"
       it "works with wildcard pattern matching" $
           let r :: Variant '["foo" := Bool, "bar" := Int, "baz" := ()] -> String
               r v =
                   variantMatch v $
                   taggedVariantCase #foo (\x -> if x then "ok" else "no") $
                   VariantWildCard "wild"
           in do r (toTaggedVariant #baz ()) `shouldBe` "wild"
                 r (toTaggedVariant #bar (23 :: Int)) `shouldBe` "wild"
                 r (toTaggedVariant #foo False) `shouldBe` "no"
       it "serialises to JSON" $
           do encode (JsonTaggedVariant (toTaggedVariant #int (123 :: Int)) :: JsonTaggedVariant '["bool" := Bool, "int" := Int])
                  `shouldBe` "{\"int\":123}"
              encode (JsonTaggedVariant (toTaggedVariant #bool True) :: JsonTaggedVariant '["bool" := Bool, "int" := Int])
                  `shouldBe` "{\"bool\":true}"
       it "parses from JSON" $
           do fmap unJsonTaggedVariant (decode "{\"bool\":true}") `shouldBe`
                  ((Just $ toTaggedVariant #bool True) :: Maybe (Variant '["bool" := Bool, "int" := Int]))
              fmap unJsonTaggedVariant (decode "{\"int\":123}") `shouldBe`
                  ((Just $ toTaggedVariant #int (123 :: Int)) :: Maybe (Variant '["bool" := Bool, "int" := Int]))
              fmap unJsonTaggedVariant (decode "{\"sss\":123}") `shouldBe`
                  (Nothing :: Maybe (Variant '["bool" := Bool, "int" := Int]))
       it "has correct equality" $
           let mkVal :: Int -> Variant '["foo" := Bool, "bar" := Int]
               mkVal = toTaggedVariant #bar
           in do mkVal 2 == mkVal 5 `shouldBe` False
                 mkVal 2 == mkVal 2 `shouldBe` True
       it "has correct ord" $
           let mkVal :: Int -> Variant '["foo" := Bool, "bar" := Int]
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
       it "serialises to JSON" $
           let makeV :: T.Text -> Maybe (TextVariant '["foo", "bar"])
               makeV = buildTextVariant
           in do fmap encode (makeV "bar") `shouldBe` Just "\"bar\""
                 fmap encode (makeV "foo") `shouldBe` Just "\"foo\""
                 fmap encode (makeV "asdasd") `shouldBe` Nothing
       it "parses from JSON" $
           let val1 :: TextVariant '["foo", "bar"]
               val1 = toTextVariant #bar
               val2 :: TextVariant '["foo", "bar"]
               val2 = toTextVariant #foo
           in do eitherDecode "\"foo\"" `shouldBe` Right val2
                 eitherDecode "\"bar\"" `shouldBe` Right val1
                 decode "basdasdar" `shouldBe`
                     (Nothing :: Maybe (TextVariant '["foo", "bar"]))
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
           show r1 `shouldBe` "[#foo := \"Hi\",#int := 213]"
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
       it "recBuildPure works" $
          ( recBuildPure @(ConstC Num) @NumFields ( \ _ _ -> 17 ) )
            `shouldBe`
          ( #int := 17 & #double := 17 & #word := 17 & #float := 17 & rnil
          :: Record NumFields
          )
       it "recBuild works" $
          ( ( `evalState` ( 0 :: Integer ) ) $
             recBuild @(ConstC Num) @(State Integer) @NumFields
               ( \ _ _ -> do { i <- State.get; State.put (i+1); pure (fromInteger $ 3 * i) } )
          )
            `shouldBe`
          ( #double := 0 & #float := 3 & #int := 6 & #word := 9 & rnil :: Record NumFields )
       it "unsafeRecBuild works" $
          ( ( `evalState` ( 0 :: Integer ) ) $
             unsafeRecBuild @NumFields @NumFields @(ConstC Num) @(State Integer)
               ( \ _ _ -> do { i <- State.get; State.put (i+1); pure (fromInteger $ 3 * i) } )
          )
            `shouldBe`
          ( #int    := 0 `unsafeRCons`
            #double := 3 `unsafeRCons`
            #word   := 6 `unsafeRCons`
            #float  := 9 `unsafeRCons`
            ( unsafeRNil 4 ) :: Rec NumFields
          )
       it "traverse works" $
          ( ( `evalState` ( 0 :: Integer ) ) $
            traverseC @(Const2C' Num) @(State Integer) @NumFields
              ( \ _ a -> do { i <- State.get; State.put (i+1); pure (1 + a * fromInteger i) } )
              numRec
          )
            `shouldBe`
          ( #int    := 1      `unsafeRCons`
            #double := 8.5    `unsafeRCons`
            #word   := 513    `unsafeRCons`
            #float  := (-5/4) `unsafeRCons`
            ( unsafeRNil 4 ) :: Rec NumFields
          )
       it "project works" $
          ( project @_ @'[ "f3" := Int, "f5" := Int ] bigRec )
            `shouldBe`
          ( #f3 := 3 & #f5 := 5 & rnil )
       it "inject works" $
          ( inject
              ( #f3 := 33 & #f5 := 55 & rnil :: Record '[ "f3" := Int, "f5" := Int ] )
              bigRec
          )
            `shouldBe`
          ( #f1   := 1
          & #f2   := 2
          & #f3   := 33
          & #f4   := 4
          & #f5   := 55
          & #f6   := 6
          & #f7   := 7
          & #f8   := 8
          & #f9   := 9
          & #f10  := 10
          & rnil
          )
