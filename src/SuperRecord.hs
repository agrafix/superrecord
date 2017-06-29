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
module SuperRecord where

import Data.Dynamic
import GHC.OverloadedLabels
import GHC.TypeLits
import qualified Data.Vector as V

data label := value = KnownSymbol label => FldProxy label := value
deriving instance Typeable (:=)
deriving instance Typeable (label := value)
infix 6 :=

data FldProxy (t :: Symbol)
    = FldProxy
    deriving (Show, Read, Eq, Ord, Typeable)

instance l ~ l' => IsLabel (l :: Symbol) (FldProxy l') where
    fromLabel _ = FldProxy

newtype Rec (lts :: [*])
   = Rec { unRec :: V.Vector Dynamic }

rnil :: Rec '[]
rnil = Rec V.empty

rcons :: Typeable t => l := t -> Rec lts -> Rec (l := t ': lts)
rcons (_ := val) (Rec vec) =
    Rec $ V.cons (toDyn val) vec

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
        val = vec V.! readAt
    in case fromDynamic val of
         Nothing -> error "SuperRecord: internal type error. This should not happen"
         Just v -> v
