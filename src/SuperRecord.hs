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
    )
where

import Data.Dynamic
import GHC.Exts
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
