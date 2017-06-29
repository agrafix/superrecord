{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
import Criterion
import Criterion.Main

import SuperRecord
import qualified Bookkeeper as B
import qualified Labels as L


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

r1L ::
    ( "f1" L.:= String
    , "f2" L.:= Int
    , "f3" L.:= Bool
    , "f4" L.:= ("f41" L.:= String)
    )
r1L =
    ( #f1 L.:= "Hi"
    , #f2 L.:= 213
    , #f3 L.:= True
    , #f4 L.:= (#f41 L.:= "abc")
    )

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

main :: IO ()
main =
    defaultMain
    [ bgroup "get"
        [ bench "superrecord" $ nf (get #f2) r1
        , bench "labels" $ nf (L.get #f2) r1L
        , bench "bookkeeper" $ nf (\r -> r B.?: #f2) r1B
        ]
    , bgroup "get nested"
        [ bench "superrecord" $ nf (get #f41 . get #f4) r1
        , bench "labels" $ nf (L.get #f41 . L.get #f4) r1L
        , bench "bookkeeper" $ nf (\r -> r B.?: #f4 B.?: #f41) r1B
        ]
    , bgroup "set"
        [ bench "superrecord" $ nf (get #f2 . set #f2 123) r1
        , bench "labels" $ nf (L.get #f2 . L.set #f2 123) r1L
        , bench "bookkeeper" $ nf (\r -> (r B.& #f2 B.%: const (123 :: Int)) B.?: #f2) r1B
        ]
    ]
