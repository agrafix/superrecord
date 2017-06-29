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
    , get, set, showRec, RecKeys(..)
    , RecTyIdxH, RecIdxTyH
    )
where

import Data.Constraint
import Data.Dynamic
import Debug.Trace
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
    in case fromDynamic val of
         Nothing -> error "SuperRecord: internal type error. This should not happen"
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

--showRec :: forall lts. (MkRecAppRec lts Show String, RecApp lts String) => Rec lts -> [String]
--showRec =
--    recApp (mkRecAppRec (Proxy :: Proxy lts) (Proxy :: Proxy String) (\(Dict :: Dict (Show a)) _ v -> show v))

class MkRecAppRec lts c r where
    mkRecAppRec ::
        Proxy lts -> Proxy r -> (forall a. Dict (c a) -> String -> a -> r) -> Rec (RecAppRec lts r)

instance MkRecAppRec '[] c r where
    mkRecAppRec Proxy _ _ = rnil

instance (Typeable t, Typeable r, KnownSymbol l, MkRecAppRec lts c r, c t) => MkRecAppRec (l := t ': lts) c r where
    mkRecAppRec (Proxy :: Proxy (l := t ': lts)) p f =
        let more :: Proxy lts
            more = Proxy
            lbl :: FldProxy l
            lbl = FldProxy
        in rcons (lbl := (\s x -> f Dict s x)) (trace "CAL!" $ mkRecAppRec more p f)

type family RecAppRec (lts :: [*]) (r :: *) :: [*] where
    RecAppRec (l := t ': lts) r = (l := (String -> t -> r)  ': RecAppRec lts r)
    RecAppRec '[] r = '[]

type family RemoveAccessTo (l :: Symbol) (lts :: [*]) :: [*] where
    RemoveAccessTo l (l := t ': lts) = RemoveAccessTo l lts
    RemoveAccessTo q (l := t ': lts) = (l := t ': RemoveAccessTo l lts)
    RemoveAccessTo q '[] = '[]

removeAccess :: proxy l -> Rec lts -> Rec (RemoveAccessTo l lts)
removeAccess _ (Rec v) = Rec v
{-# INLINE removeAccess #-}

{-
removeAccessF ::
    proxy (l :: Symbol)
    -> proxy (lts :: [*])
    -> proxy (r :: *)
    -> Rec (RecAppRec lts r)
    -> Rec (RecAppRec (RemoveAccessTo l lts) r)
removeAccessF = undefined

class RecApp lts r where
    recApp :: Rec (RecAppRec lts r) -> Rec lts -> [r]

instance RecApp '[] r where
    recApp _ _ = []

instance (Typeable t, Typeable r, KnownSymbol l) => RecApp (l := t ': lts) r where
    recApp (rF :: Rec (RecAppRec (l := t ': lts) r))  (rV :: Rec (l := t ': lts)) =
        let lbl :: FldProxy l
            lbl = FldProxy
            f = get lbl rF
            v = get lbl rV
            lblStr = symbolVal lbl
            rF' :: Rec (RecAppRec (RemoveAccessTo l (l := t ': lts)) r)
            rF' = removeAccess lbl rF
            rV' = removeAccess lbl rV
        in (f lblStr v : recApp rF' rV')
-}
