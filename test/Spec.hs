
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

import Control.Monad.Reader
import Data.Aeson
import GHC.Generics (Generic)
import Test.Hspec

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
   , "f11" := Int
   , "f12" := Int
   , "f13" := Int
   , "f14" := Int
   , "f15" := Int
   , "f16" := Int
   , "f17" := Int
   , "f18" := Int
   , "f19" := Int
   , "f20" := Int
   , "f21" := Int
   , "f22" := Int
   , "f23" := Int
   , "f24" := Int
   , "f25" := Int
   , "f26" := Int
   , "f27" := Int
   , "f28" := Int
   , "f29" := Int
   , "f30" := Int
   , "f31" := Int
   , "f32" := Int
   , "f33" := Int
   , "f34" := Int
   , "f35" := Int
   , "f36" := Int
   , "f37" := Int
   , "f38" := Int
   , "f39" := Int
   , "f40" := Int
   , "f41" := Int
   , "f42" := Int
   , "f43" := Int
   , "f44" := Int
   , "f45" := Int
   , "f46" := Int
   , "f47" := Int
   , "f48" := Int
   , "f49" := Int
   , "f50" := Int
   , "f51" := Int
   , "f52" := Int
   , "f53" := Int
   , "f54" := Int
   , "f55" := Int
   , "f56" := Int
   , "f57" := Int
   , "f58" := Int
   , "f59" := Int
   , "f60" := Int
   , "f61" := Int
   , "f62" := Int
   , "f63" := Int
   , "f64" := Int
   , "f65" := Int
   , "f66" := Int
   , "f67" := Int
   , "f68" := Int
   , "f69" := Int
   , "f70" := Int
   , "f71" := Int
   , "f72" := Int
   , "f73" := Int
   , "f74" := Int
   , "f75" := Int
   , "f76" := Int
   , "f77" := Int
   , "f78" := Int
   , "f79" := Int
   , "f80" := Int
   , "f81" := Int
   , "f82" := Int
   , "f83" := Int
   , "f84" := Int
   , "f85" := Int
   , "f86" := Int
   , "f87" := Int
   , "f88" := Int
   , "f89" := Int
   , "f90" := Int
   , "f91" := Int
   , "f92" := Int
   , "f93" := Int
   , "f94" := Int
   , "f95" := Int
   , "f96" := Int
   , "f97" := Int
   , "f98" := Int
   , "f99" := Int
   -- , "f100" := Int
   -- , "f101" := Int
   -- , "f102" := Int
   -- , "f103" := Int
   -- , "f104" := Int
   -- , "f105" := Int
   -- , "f106" := Int
   -- , "f107" := Int
   -- , "f108" := Int
   -- , "f109" := Int
   -- , "f110" := Int
   -- , "f111" := Int
   -- , "f112" := Int
   -- , "f113" := Int
   -- , "f114" := Int
   -- , "f115" := Int
   -- , "f116" := Int
   -- , "f117" := Int
   -- , "f118" := Int
   -- , "f119" := Int
   -- , "f120" := Int
   -- , "f121" := Int
   -- , "f122" := Int
   -- , "f123" := Int
   -- , "f124" := Int
   -- , "f125" := Int
   -- , "f126" := Int
   -- , "f127" := Int
   -- , "f128" := Int
   -- , "f129" := Int
   -- , "f130" := Int
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
  & #f11  := 11
  & #f12  := 12
  & #f13  := 13
  & #f14  := 14
  & #f15  := 15
  & #f16  := 16
  & #f17  := 17
  & #f18  := 18
  & #f19  := 19
  & #f20  := 20
  & #f21  := 21
  & #f22  := 22
  & #f23  := 23
  & #f24  := 24
  & #f25  := 25
  & #f26  := 26
  & #f27  := 27
  & #f28  := 28
  & #f29  := 29
  & #f30  := 30
  & #f31  := 31
  & #f32  := 32
  & #f33  := 33
  & #f34  := 34
  & #f35  := 35
  & #f36  := 36
  & #f37  := 37
  & #f38  := 38
  & #f39  := 39
  & #f40  := 40
  & #f41  := 41
  & #f42  := 42
  & #f43  := 43
  & #f44  := 44
  & #f45  := 45
  & #f46  := 46
  & #f47  := 47
  & #f48  := 48
  & #f49  := 49
  & #f50  := 50
  & #f51  := 51
  & #f52  := 52
  & #f53  := 53
  & #f54  := 54
  & #f55  := 55
  & #f56  := 56
  & #f57  := 57
  & #f58  := 58
  & #f59  := 59
  & #f60  := 60
  & #f61  := 61
  & #f62  := 62
  & #f63  := 63
  & #f64  := 64
  & #f65  := 65
  & #f66  := 66
  & #f67  := 67
  & #f68  := 68
  & #f69  := 69
  & #f70  := 70
  & #f71  := 71
  & #f72  := 72
  & #f73  := 73
  & #f74  := 74
  & #f75  := 75
  & #f76  := 76
  & #f77  := 77
  & #f78  := 78
  & #f79  := 79
  & #f80  := 80
  & #f81  := 81
  & #f82  := 82
  & #f83  := 83
  & #f84  := 84
  & #f85  := 85
  & #f86  := 86
  & #f87  := 87
  & #f88  := 88
  & #f89  := 89
  & #f90  := 90
  & #f91  := 91
  & #f92  := 92
  & #f93  := 93
  & #f94  := 94
  & #f95  := 95
  & #f96  := 96
  & #f97  := 97
  & #f98  := 98
  & #f99  := 99
  -- & #f100 := 100
  -- & #f101 := 101
  -- & #f102 := 102
  -- & #f103 := 103
  -- & #f104 := 104
  -- & #f105 := 105
  -- & #f106 := 106
  -- & #f107 := 107
  -- & #f108 := 108
  -- & #f109 := 109
  -- & #f110 := 110
  -- & #f111 := 111
  -- & #f112 := 112
  -- & #f113 := 113
  -- & #f114 := 114
  -- & #f115 := 115
  -- & #f116 := 116
  -- & #f117 := 117
  -- & #f118 := 118
  -- & #f119 := 119
  -- & #f120 := 120
  -- & #f121 := 121
  -- & #f122 := 122
  -- & #f123 := 123
  -- & #f124 := 124
  -- & #f125 := 125
  -- & #f126 := 126
  -- & #f127 := 127
  -- & #f128 := 128
  -- & #f129 := 129
  -- & #f130 := 130
  & rnil

main :: TestRecAppend => IO ()
main = hspec $
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
       it "reader works" $
           do runReaderT mtlAsk (#id := 123 & rnil) `shouldReturn` 123
