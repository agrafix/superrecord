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

data FldProxy (t :: Symbol)
    = FldProxy
    deriving (Show, Read, Eq, Ord, Typeable)

instance l ~ l' => IsLabel (l :: Symbol) (FldProxy l') where
    fromLabel _ = FldProxy

newtype Rec (ls :: [Symbol]) (ts :: [*])
   = Rec { unRec :: V.Vector Dynamic }

rnil :: Rec '[] '[]
rnil = Rec V.empty

rcons :: Typeable t => FldProxy l -> t -> Rec ls ts -> Rec (l ': ls) (t ': ts)
rcons _ val (Rec vec) =
    Rec $ V.cons (toDyn val) vec

type family RecTyIdxH (i :: Nat) (l :: Symbol) (ls :: [Symbol]) :: Nat where
    RecTyIdxH idx l (l ': ls) = idx
    RecTyIdxH idx m (l ': ls) = RecTyIdxH (1 + idx) m ls

type family RecIdxTyH (i :: Nat) (r :: Nat) (ls :: [*]) :: * where
    RecIdxTyH idx idx (l ': ls) = l
    RecIdxTyH idx other (l ': ls) = RecIdxTyH idx (other + 1) ls

get ::
    forall l ls ts v idx.
    ( RecTyIdxH 0 l ls ~ idx
    , RecIdxTyH 0 idx ts ~ v
    , KnownNat idx
    , Typeable v
    ) => FldProxy l -> Rec ls ts -> v
get _ (Rec vec) =
    let readAt = fromIntegral $ natVal (Proxy :: Proxy idx)
        val = vec V.! readAt
    in case fromDynamic val of
         Nothing -> error "SuperRecord: internal type error. This should not happen"
         Just v -> v
