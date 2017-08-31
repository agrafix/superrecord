{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module SuperRecord.Sort
    ( FieldListSort
    )
where

import SuperRecord.Field

import Data.Proxy
import GHC.TypeLits

type family If (cond :: Bool) (ifTrue :: k) (ifFalse :: k) :: k where
    If 'True x y = x
    If 'False x y = y

type family IsGT (o :: Ordering) :: Bool where
    IsGT 'GT = 'True
    IsGT 'LT = 'False
    IsGT 'EQ = 'False

type family IsLT (o :: Ordering) :: Bool where
    IsLT 'GT = 'False
    IsLT 'LT = 'True
    IsLT 'EQ = 'False

type family IsLEq (o :: Ordering) :: Bool where
    IsLEq 'GT = 'False
    IsLEq 'LT = 'True
    IsLEq 'EQ = 'True

type family SymbolLEq (s :: Symbol) (t :: Symbol) :: Bool where
    SymbolLEq s t = IsLEq (CmpSymbol s t)

type family HalfOfHelper (n :: Nat) (i :: Nat) (dist :: Nat) (o :: Ordering) :: Nat where
    HalfOfHelper n m dist 'GT = m - 1
    HalfOfHelper n m dist 'EQ = m
    HalfOfHelper n m 1 'LT = m
    HalfOfHelper n m dist 'LT = HalfOfHelper n (m + 2) (n - ((m + 2) * 2)) (CmpNat ((m + 2) * 2) n)

type family HalfOf (n :: Nat) :: Nat where
    -- some optimizations for faster compilation
    HalfOf 0 = 0
    HalfOf 1 = 1
    HalfOf 2 = 1
    HalfOf 3 = 1
    HalfOf 4 = 2
    HalfOf 5 = 2
    HalfOf 6 = 3
    HalfOf 7 = 3
    HalfOf 8 = 4
    HalfOf 9 = 4
    HalfOf 10 = 5
    -- the general case
    HalfOf n = HalfOfHelper n 0 n 'LT -- usually (CmpNat 0 n), but 0 ist already handled!

type family LengthOf (xs :: [k]) :: Nat where
    LengthOf '[] = 0
    LengthOf (x ': xs) = 1 + LengthOf xs

type family ListTake (xs :: [*]) (n :: Nat) :: [*] where
    ListTake '[] n = '[]
    ListTake xs 0 = '[]
    ListTake (x ': xs) n = (x ': ListTake xs (n - 1))

type family ListDrop (xs :: [*]) (n :: Nat) :: [*] where
    ListDrop '[] n = '[]
    ListDrop xs 0 = xs
    ListDrop (x ': xs) n = ListDrop xs (n - 1)

type family FieldListMerge (xs :: [*]) (ys :: [*]) :: [*] where
    FieldListMerge xs '[] = xs
    FieldListMerge '[] ys = ys
    FieldListMerge (x := xv ': xs) (y := yv ': ys) =
        If (SymbolLEq x y)
            ((x := xv) ': FieldListMerge xs (y := yv ': ys))
            ((y := yv) ': FieldListMerge (x := xv ': xs) ys)

type family ListSortStep (xs :: [*]) (halfLen :: Nat) :: [*] where
    ListSortStep xs halfLen =
        FieldListMerge
            (FieldListSort (ListTake xs halfLen))
            (FieldListSort (ListDrop xs halfLen))

-- | Sort a list of fields using merge sort
type family FieldListSort (xs :: [*]) :: [*] where
    FieldListSort '[] = '[]
    FieldListSort '[x] = '[x]
    FieldListSort '[x, y] = FieldListMerge '[x] '[y] -- not needed, just an optimization
    FieldListSort xs =
        ListSortStep xs (HalfOf (LengthOf xs))

-- Some type level tests. These will produce an "overlapping patterns" warning when failing

_testLengthOf :: ( LengthOf '[1, 2, 3, 4] ~ x, x ~ 4 ) => Proxy x
_testLengthOf = Proxy

_testHalfOf :: ( HalfOf 0 ~ x, x ~ 0 ) => Proxy x
_testHalfOf = Proxy

_testHalfOf1 :: ( HalfOf 1 ~ x, x ~ 1 ) => Proxy x
_testHalfOf1 = Proxy

_testHalfOf2 :: ( HalfOf 2 ~ x, x ~ 1 ) => Proxy x
_testHalfOf2 = Proxy

_testHalfOf3 :: ( HalfOf 3 ~ x, x ~ 1 ) => Proxy x
_testHalfOf3 = Proxy

_testHalfOf4 :: ( HalfOf 4 ~ x, x ~ 2 ) => Proxy x
_testHalfOf4 = Proxy

_testHalfOf99 :: ( HalfOf 99 ~ x, x ~ 49 ) => Proxy x
_testHalfOf99 = Proxy

_testHalfOf100 :: ( HalfOf 100 ~ x, x ~ 50 ) => Proxy x
_testHalfOf100 = Proxy

_testHalfOf101 :: ( HalfOf 101 ~ x, x ~ 50 ) => Proxy x
_testHalfOf101 = Proxy

_testHalfOf200 :: ( HalfOf 200 ~ x, x ~ 100 ) => Proxy x
_testHalfOf200 = Proxy

_testHalfOf400 :: ( HalfOf 400 ~ x, x ~ 200 ) => Proxy x
_testHalfOf400 = Proxy

_testHalfOf401 :: ( HalfOf 401 ~ x, x ~ 200 ) => Proxy x
_testHalfOf401 = Proxy

_testSort0 ::
    ( FieldListSort '[] ~ x
    , x ~ '[]
    ) => Proxy x
_testSort0 = Proxy

_testSort1 ::
    ( FieldListSort '["test" := Int] ~ x
    , x ~ '["test" := Int]
    ) => Proxy x
_testSort1 = Proxy

_testSort2 ::
    ( FieldListSort '["test" := Int, "abc" := String] ~ x
    , x ~ '["abc" := String, "test" := Int]
    ) => Proxy x
_testSort2 = Proxy

_testSort3 ::
    ( FieldListSort '["test" := Int, "abc" := String, "def" := String] ~ x
    , x ~ '["abc" := String, "def" := String, "test" := Int]
    ) => Proxy x
_testSort3 = Proxy
