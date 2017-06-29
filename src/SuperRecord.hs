{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module SuperRecord
    ( (:=)(..)
    , Rec, rnil, rcons, (&)
    , get, set
    , RecTyIdxH, RecIdxTyH
    , reflectRec,  RecApply(..)
    , showRec, RecKeys(..)
    , RecEq(..)
    )
where

import Data.Constraint
import Data.Dynamic
import GHC.OverloadedLabels
import GHC.TypeLits
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- | Field named @l@ labels value of type @t@ adapted from the awesome /labels/ package.
-- Example: @(#name := \"Chris\") :: (\"name\" := String)@
data label := value = KnownSymbol label => FldProxy label := value
deriving instance Typeable (:=)
deriving instance Typeable (label := value)
infix 6 :=

instance (Eq value) => Eq (label := value) where
  (_ := x) == (_ := y) = x == y
  {-# INLINE (==) #-}

instance (Ord value) => Ord (label := value) where
  compare (_ := x) (_ := y) = x `compare` y
  {-# INLINE compare #-}

instance (Show t) =>
         Show (l := t) where
  showsPrec p (l := t) =
    showParen (p > 10) (showString ("#" ++ (symbolVal l) ++ " := " ++ show t))

-- | A proxy witness for a label. Very similar to 'Proxy', but needed to implement
-- a non-orphan 'IsLabel' instance
data FldProxy (t :: Symbol)
    = FldProxy
    deriving (Show, Read, Eq, Ord, Typeable)

instance l ~ l' => IsLabel (l :: Symbol) (FldProxy l') where
    fromLabel _ = FldProxy

-- | The core record type.
newtype Rec (lts :: [*])
   = Rec { unRec :: V.Vector Dynamic }

instance (RecApply lts lts Show (String, String)) => Show (Rec lts) where
    show = show . showRec

instance RecEq lts lts => Eq (Rec lts) where
    (==) (a :: Rec lts) (b :: Rec lts) = recEq a b (Proxy :: Proxy lts)

-- | An empty record
rnil :: Rec '[]
rnil = Rec V.empty
{-# INLINEABLE rnil #-}

-- | Prepend a record entry to a record 'Rec'
rcons :: Typeable t => l := t -> Rec lts -> Rec (l := t ': lts)
rcons (_ := val) (Rec vec) =
    Rec $ V.cons (toDyn val) vec
{-# INLINEABLE rcons #-}

-- | Alias for 'rcons'
(&) :: Typeable t => l := t -> Rec lts -> Rec (l := t ': lts)
(&) = rcons

infixr 5 &

type family RecTyIdxH (i :: Nat) (l :: Symbol) (lts :: [*]) :: Nat where
    RecTyIdxH idx l (l := t ': lts) = idx
    RecTyIdxH idx m (l := t ': lts) = RecTyIdxH (1 + idx) m lts
    RecTyIdxH idx m '[] =
        TypeError
        ( 'Text "Could not find label "
          ':<>: 'Text m
        )

type family RecIdxTyH (i :: Nat) (r :: Nat) (lts :: [*]) :: * where
    RecIdxTyH idx idx (l := t ': lts) = t
    RecIdxTyH idx other (l := t ': lts) = RecIdxTyH idx (other + 1) lts
    RecIdxTyH idx other '[] =
        TypeError ('Text "Could not find index " ':<>: 'ShowType idx)

get ::
    forall l v lts idx.
    ( RecTyIdxH 0 l lts ~ idx
    , RecIdxTyH idx 0 lts ~ v
    , KnownNat idx
    , Typeable v
    ) => FldProxy l -> Rec lts -> v
get _ (Rec vec) =
    let readAt = fromIntegral $ natVal (Proxy :: Proxy idx)
        val = V.unsafeIndex vec readAt
    in case (fromDynamic val :: Maybe v) of
         Nothing ->
             let expected = typeOf (undefined :: v)
                 got = dynTypeRep val
             in error $
                "SuperRecord: internal type error. This should not happen. Expected type "
                ++ show expected ++ " but got " ++ show got
         Just v -> v
{-# INLINEABLE get #-}

set ::
    forall l v lts idx.
    ( RecTyIdxH 0 l lts ~ idx
    , RecIdxTyH idx 0 lts ~ v
    , KnownNat idx
    , Typeable v
    ) => FldProxy l -> v -> Rec lts -> Rec lts
set _ val r =
    let setAt = fromIntegral $ natVal (Proxy :: Proxy idx)
        dynVal = toDyn val
    in Rec (V.modify (\v -> VM.unsafeWrite v setAt dynVal) $ unRec r)
{-# INLINEABLE set #-}


class RecKeys (lts :: [*]) where
    type RecKeysT lts :: [Symbol]
    recKeys :: t lts -> [String]

instance RecKeys '[] where
    type RecKeysT '[] = '[]
    recKeys _ = []

instance (KnownSymbol l, RecKeys lts) => RecKeys (l := t ': lts) where
    type RecKeysT (l := t ': lts) = (l ': RecKeysT lts)
    recKeys (_ :: f (l := t ': lts)) =
        let lbl :: FldProxy l
            lbl = FldProxy
            more :: Proxy lts
            more = Proxy
        in (symbolVal lbl : recKeys more)

-- | Apply a function to each key element pair for a record
reflectRec ::
    forall c r lts. (RecApply lts lts c r)
    => Proxy c
    -> (forall a. c a => String -> a -> r)
    -> Rec lts
    -> [r]
reflectRec _ f r =
    recApply (\(Dict :: Dict (c a)) s v -> f s v) r (Proxy :: Proxy lts)

-- | Convert all elements of a record to a 'String'
showRec :: forall lts. (RecApply lts lts Show (String, String)) => Rec lts -> [(String, String)]
showRec = reflectRec @Show Proxy (\k v -> (k, show v))

class RecApply (rts :: [*]) (lts :: [*]) c r where
    recApply :: (forall a. Dict (c a) -> String -> a -> r) -> Rec rts -> Proxy lts -> [r]

instance RecApply rts '[] c r where
    recApply _ _ _ = []

instance
    ( KnownSymbol l
    , RecApply rts (RemoveAccessTo l lts) c r
    , RecTyIdxH 0 l rts ~ idx
    , RecIdxTyH idx 0 rts ~ v
    , Typeable v
    , KnownNat idx
    , c v
    ) => RecApply rts (l := t ': lts) c r where
    recApply f r (_ :: Proxy (l := t ': lts)) =
        let lbl :: FldProxy l
            lbl = FldProxy
            val = get lbl r
            res = f Dict (symbolVal lbl) val
            pNext :: Proxy (RemoveAccessTo l (l := t ': lts))
            pNext = Proxy
        in (res : recApply f r pNext)

class RecEq (rts :: [*]) (lts :: [*]) where
    recEq :: Rec rts -> Rec rts -> Proxy lts -> Bool

instance RecEq rts '[] where
    recEq _ _ _ = True

instance
    ( RecEq rts (RemoveAccessTo l lts)
    , RecTyIdxH 0 l rts ~ idx
    , RecIdxTyH idx 0 rts ~ v
    , Typeable v, Eq v, KnownNat idx
    ) => RecEq rts (l := t ': lts) where
    recEq r1 r2 (_ :: Proxy (l := t ': lts)) =
       let lbl :: FldProxy l
           lbl = FldProxy
           val = get lbl r1
           val2 = get lbl r2
           res = val == val2
           pNext :: Proxy (RemoveAccessTo l (l := t ': lts))
           pNext = Proxy
       in res && recEq r1 r2 pNext

type family RemoveAccessTo (l :: Symbol) (lts :: [*]) :: [*] where
    RemoveAccessTo l (l := t ': lts) = RemoveAccessTo l lts
    RemoveAccessTo q (l := t ': lts) = (l := t ': RemoveAccessTo l lts)
    RemoveAccessTo q '[] = '[]
