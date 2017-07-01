{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
import Criterion
import Criterion.Main

import Control.DeepSeq
import Data.Aeson
import GHC.Generics
import SuperRecord
import qualified Bookkeeper as B
import qualified Labels as L

data Nested
    = Nested
    { n_f41 :: String
    } deriving (Generic)

instance NFData Nested
instance ToJSON Nested
instance FromJSON Nested

data Native
    = Native
    { n_f1 :: String
    , n_f2 :: Int
    , n_f3 :: Bool
    , n_f4 :: Nested
    } deriving (Generic)

instance NFData Native
instance ToJSON Native
instance FromJSON Native

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

r1N =
    Native
    { n_f1 = "Hi"
    , n_f2 = 213
    , n_f3 = True
    , n_f4 = Nested "abc"
    }

main :: IO ()
main =
    defaultMain
    [ bgroup "get"
        [ bench "superrecord" $ nf (get #f2) r1
        , bench "labels" $ nf (L.get #f2) r1L
        , bench "bookkeeper" $ nf (\r -> r B.?: #f2) r1B
        , bench "native" $ nf n_f2 r1N
        ]
    , bgroup "get nested"
        [ bench "superrecord" $ nf (get #f41 . get #f4) r1
        , bench "labels" $ nf (L.get #f41 . L.get #f4) r1L
        , bench "bookkeeper" $ nf (\r -> r B.?: #f4 B.?: #f41) r1B
        , bench "native" $ nf (n_f41 . n_f4) r1N
        ]
    , bgroup "set nested"
        [ bench "superrecord" $
            nf (\r -> (setPath (#f4 &: #f41 &: snil) "Hello" r) &. #f4 &. #f41) r1
        , bench "labels" $
            nf (\r -> L.get #f41 . L.get #f4 $ L.modify #f4 (L.set #f41 "Hello") r) r1L
        , bench "bookkeeper" $
            nf (\r -> (r B.& #f4 B.%: (\s -> s B.& #f41 B.%: const "Hello")) B.?: #f4 B.?: #f41) r1B
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
    ]

throwOnNone :: Maybe a -> a
throwOnNone (Just x) = x
throwOnNone _ = error "What?!"
