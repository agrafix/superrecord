{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
import SuperRecord

import Test.Hspec

type Ex1 = '["foo" := String, "int" := Int]

r1 :: Rec Ex1
r1 = rcons (#foo := "Hi") $ rcons (#int := 213) rnil

main :: IO ()
main = hspec $
    do it "getter works" $
           do get #foo r1 `shouldBe` "Hi"
              get #int r1 `shouldBe` 213
