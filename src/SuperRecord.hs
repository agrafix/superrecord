{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE MagicHash #-}
module SuperRecord
    ( -- * Basics
      (:=)(..)
    , Rec, rnil, rcons, (&)
    , Has, get, set
      -- * Reflection
    , reflectRec,  RecApply(..)
      -- * Machinery
    , RecTyIdxH, RecIdxTyH
    , showRec, RecKeys(..)
    , RecEq(..)
    , recToValue, recToEncoding
    , recJsonParser, RecJsonParse(..)
    )
where

import Control.DeepSeq
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Constraint
import Data.Proxy
import Data.Typeable
import GHC.OverloadedLabels
import GHC.Prim
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Primitive.Array as A
import qualified Data.Text as T

-- | Field named @l@ labels value of type @t@ adapted from the awesome /labels/ package.
-- Example: @(#name := \"Chris\") :: (\"name\" := String)@
data label := value = KnownSymbol label => FldProxy label := !value
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
      showParen (p > 10) (showString ("#" ++ symbolVal l ++ " := " ++ show t))

-- | A proxy witness for a label. Very similar to 'Proxy', but needed to implement
-- a non-orphan 'IsLabel' instance
data FldProxy (t :: Symbol)
    = FldProxy
    deriving (Show, Read, Eq, Ord, Typeable)

instance l ~ l' => IsLabel (l :: Symbol) (FldProxy l') where
    fromLabel _ = FldProxy

-- | The core record type.
newtype Rec (lts :: [*])
   = Rec { _unRec :: (A.Array Any) }

instance (RecApply lts lts Show) => Show (Rec lts) where
    show = show . showRec

instance RecEq lts lts => Eq (Rec lts) where
    (==) (a :: Rec lts) (b :: Rec lts) = recEq a b (Proxy :: Proxy lts)
    {-# INLINE (==) #-}

instance
    ( RecApply lts lts ToJSON
    ) => ToJSON (Rec lts) where
    toJSON = recToValue
    toEncoding = recToEncoding

instance (RecSize lts ~ s, KnownNat s, RecJsonParse lts) => FromJSON (Rec lts) where
    parseJSON = recJsonParser

instance RecNfData lts lts => NFData (Rec lts) where
    rnf = recNfData (Proxy :: Proxy lts)

-- | An empty record
rnil :: Rec '[]
rnil = unsafeRnil 0
{-# INLINE rnil #-}

-- | An empty record with an initial size for the record
unsafeRnil :: Int -> Rec '[]
unsafeRnil initSize =
    Rec $ unsafePerformIO (A.newArray initSize (error "No Value") >>= A.unsafeFreezeArray)
{-# INLINE unsafeRnil #-}

-- | Prepend a record entry to a record 'Rec'
rcons :: forall l t lts s. (RecSize lts ~ s, KnownNat s) => l := t -> Rec lts -> Rec (l := t ': lts)
rcons (_ := val) (Rec vec) =
    Rec $
    unsafePerformIO $!
    do m2 <- A.newArray (size + 1) (error "No Value")
       A.copyArray m2 0 vec 0 size
       A.writeArray m2 size (unsafeCoerce# val)
       A.unsafeFreezeArray m2
    where
        size = fromIntegral $ natVal' (proxy# :: Proxy# s)
{-# INLINE rcons #-}

-- | Prepend a record entry to a record 'Rec'. Assumes that the record was created with
-- 'unsafeRnil' and still has enough free slots, mutates the original 'Rec' which should
-- not be reused after
unsafeRCons ::
    forall l t lts s. (RecSize lts ~ s, KnownNat s) => l := t -> Rec lts -> Rec (l := t ': lts)
unsafeRCons (_ := val) (Rec vec) =
    Rec $
    unsafePerformIO $!
    do m2 <- A.unsafeThawArray vec
       A.writeArray m2 size (unsafeCoerce# val)
       A.unsafeFreezeArray m2
    where
        size = fromIntegral $ natVal' (proxy# :: Proxy# s)
{-# INLINE unsafeRCons #-}

-- | Alias for 'rcons'
(&) :: forall l t lts s. (RecSize lts ~ s, KnownNat s) => l := t -> Rec lts -> Rec (l := t ': lts)
(&) = rcons
{-# INLINE (&) #-}

infixr 5 &

type family RecSize (lts :: [*]) :: Nat where
    RecSize '[] = 0
    RecSize (l := t ': lts) = 1 + RecSize lts

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

-- | State that a record contains a label. Leave idx an s free variables, used internally
type Has l lts idx s v =
   ( RecTyIdxH 0 l lts ~ idx
   , RecIdxTyH idx 0 lts ~ v
   , KnownNat idx
   , RecSize lts ~ s, KnownNat s
   )

-- | Get an existing record field
get ::
    forall l v lts idx s.
    (Has l lts idx s v) => FldProxy l -> Rec lts -> v
get _ (Rec vec) =
    let !size = fromIntegral $ natVal' (proxy# :: Proxy# s)
        !readAt = size - fromIntegral (natVal' (proxy# :: Proxy# idx)) - 1
        anyVal :: Any
        anyVal = A.indexArray vec readAt
    in unsafeCoerce# anyVal
{-# INLINE get #-}

-- | Update an existing record field
set ::
    forall l v lts idx s.
    (Has l lts idx s v)
    => FldProxy l -> v -> Rec lts -> Rec lts
set _ !val (Rec vec) =
    let !size = fromIntegral $ natVal' (proxy# :: Proxy# s)
        !setAt = size - fromIntegral (natVal' (proxy# :: Proxy# idx)) - 1
        dynVal = unsafeCoerce# val
        r2 =
            unsafePerformIO $!
            do m2 <- A.newArray size (error "No Value")
               A.copyArray m2 0 vec 0 size
               A.writeArray m2 setAt dynVal
               Rec <$> A.unsafeFreezeArray m2
    in r2
{-# INLINE set #-}

-- | Get keys of a record on value and type level
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
    forall c r lts. (RecApply lts lts c)
    => Proxy c
    -> (forall a. c a => String -> a -> r)
    -> Rec lts
    -> [r]
reflectRec _ f r =
    recApply (\(Dict :: Dict (c a)) s v -> f s v) r (Proxy :: Proxy lts)
{-# INLINE reflectRec #-}

-- | Convert all elements of a record to a 'String'
showRec :: forall lts. (RecApply lts lts Show) => Rec lts -> [(String, String)]
showRec = reflectRec @Show Proxy (\k v -> (k, show v))

recToValue :: forall lts. (RecApply lts lts ToJSON) => Rec lts -> Value
recToValue r = toJSON $ reflectRec @ToJSON Proxy (\k v -> (T.pack k, toJSON v)) r

recToEncoding :: forall lts. (RecApply lts lts ToJSON) => Rec lts -> Encoding
recToEncoding r = pairs $ mconcat $ reflectRec @ToJSON Proxy (\k v -> (T.pack k .= v)) r

recJsonParser :: forall lts s. (RecSize lts ~ s, KnownNat s, RecJsonParse lts) => Value -> Parser (Rec lts)
recJsonParser =
    withObject "Record" $ \o ->
    recJsonParse initSize o
    where
        initSize = fromIntegral $ natVal' (proxy# :: Proxy# s)

-- | Machinery needed to implement 'reflectRec'
class RecApply (rts :: [*]) (lts :: [*]) c where
    recApply :: (forall a. Dict (c a) -> String -> a -> r) -> Rec rts -> Proxy lts -> [r]

instance RecApply rts '[] c where
    recApply _ _ _ = []

instance
    ( KnownSymbol l
    , RecApply rts (RemoveAccessTo l lts) c
    , Has l rts idx s v
    , c v
    ) => RecApply rts (l := t ': lts) c where
    recApply f r (_ :: Proxy (l := t ': lts)) =
        let lbl :: FldProxy l
            lbl = FldProxy
            val = get lbl r
            res = f Dict (symbolVal lbl) val
            pNext :: Proxy (RemoveAccessTo l (l := t ': lts))
            pNext = Proxy
        in (res : recApply f r pNext)

-- | Machinery to implement equality
class RecEq (rts :: [*]) (lts :: [*]) where
    recEq :: Rec rts -> Rec rts -> Proxy lts -> Bool

instance RecEq rts '[] where
    recEq _ _ _ = True

instance
    ( RecEq rts (RemoveAccessTo l lts)
    , Has l rts idx s v
    , Eq v
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

-- | Machinery to implement parseJSON
class RecJsonParse (lts :: [*]) where
    recJsonParse :: Int -> Object -> Parser (Rec lts)

instance RecJsonParse '[] where
    recJsonParse initSize _ = pure (unsafeRnil initSize)

instance
    ( KnownSymbol l, FromJSON t, RecJsonParse lts
    , RecSize lts ~ s, KnownNat s
    ) => RecJsonParse (l := t ': lts) where
    recJsonParse initSize obj =
        do let lbl :: FldProxy l
               lbl = FldProxy
           (v :: t) <- obj .: T.pack (symbolVal lbl)
           rest <- recJsonParse initSize obj
           pure $ unsafeRCons (lbl := v) rest

-- | Machinery for NFData
class RecNfData (lts :: [*]) (rts :: [*]) where
    recNfData :: Proxy lts -> Rec rts -> ()

instance RecNfData '[] rts where
    recNfData _ _ = ()

instance
    ( Has l rts idx s v
    , NFData v
    , RecNfData (RemoveAccessTo l lts) rts
    ) => RecNfData (l := t ': lts) rts where
    recNfData (_ :: (Proxy (l := t ': lts))) r =
        let !v = get (FldProxy :: FldProxy l) r
            pNext :: Proxy (RemoveAccessTo l (l := t ': lts))
            pNext = Proxy
        in deepseq v (recNfData pNext r)
