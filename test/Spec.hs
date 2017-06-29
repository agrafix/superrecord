{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
import SuperRecord

import Test.Hspec

type Ex1 = '["foo" := String, "int" := Int]

r1 :: Rec Ex1
r1 =
    #foo := "Hi"
    & #int := 213
    & rnil

main :: IO ()
main = hspec $
    do it "getter works" $
           do get #foo r1 `shouldBe` "Hi"
              get #int r1 `shouldBe` 213
       it "setter works" $
           do let r2 = set #foo "Hey" r1
              get #foo r1 `shouldBe` "Hi"
              get #foo r2 `shouldBe` "Hey"
       it "getting record keys works" $
           do let vals = recKeys r1
              vals `shouldBe` ["foo", "int"]
       it "showRec words" $
           do let vals = showRec r1
              vals `shouldBe` ["foo", "int"]
