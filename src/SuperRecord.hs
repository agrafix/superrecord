{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
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
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

#ifdef JS_RECORD
{-# LANGUAGE JavaScriptFFI #-}
#endif

module SuperRecord
    ( -- * Basics
      (:=)(..)
    , Record, rnil, rcons, (&)
    , fld
    , Has, HasOf
    , get, (&.)
    , set
    , modify
    , getPath, setPath, modifyPath, RecApplyPath, (:&), (&:), (&:-)
    , combine, (++:), RecAppend
      -- * Reflection
    , reflectRec, reflectRecFold, RecApply(..)
      -- * Native type interop
    , FromNative, fromNative
    , ToNative, toNative
      -- * MTL interop
    , asksR, asksRP
    , getsR, setsR, modifiesR
    , getsRP, setsRP, modifiesRP
      -- * Lens interop
    , lens
      -- * Machinery
    , Rec
    , RecCopy
    , RecTyIdxH
    , showRec, RecKeys(..), recKeys
    , RecEq(..)
    , recToValue, recToEncoding
    , recJsonParser, RecJsonParse(..)
    , RecNfData(..)
    , RecSize, RemoveAccessTo
    , FldProxy(..), RecDeepTy
    , RecAll
    , KeyDoesNotExist
    , Sort
    )
where

import SuperRecord.Field
import SuperRecord.Sort

import Control.DeepSeq
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Constraint
import Data.Proxy
import GHC.Base (Int(..), Any)
import GHC.Generics
import GHC.Prim
import GHC.TypeLits
import qualified Control.Monad.State as S
import qualified Data.Text as T

#ifdef JS_RECORD
import GHCJS.Marshal
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.JSString as JSS
import qualified JavaScript.Object.Internal as JS
#else
import GHC.ST ( ST(..) , runST)
#endif

-- | Sort a list of fields using merge sort, alias to 'FieldListSort'
type Sort xs = FieldListSort xs

-- | The core record type. Prefer this type when manually writing type
-- signatures
type Record lts = Rec (Sort lts)

-- | Internal record type. When manually writing an explicit type signature for
-- a record, use 'Record' instead. For abstract type signatures 'Rec' will work
-- well.
data Rec (lts :: [*])
   = Rec
   {
#ifndef JS_RECORD
       _unRec :: SmallArray# Any -- Note that the values are physically in reverse order
#else
       _unRec :: !JS.Object
#endif
   }

type role Rec representational

#ifdef JS_RECORD
copyObject :: JS.Object -> IO JS.Object
copyObject obj =
    do objNew <- JS.create
       props <- JS.listProps obj
       forM_ props $ \prop ->
           do val <- JS.unsafeGetProp prop obj
              JS.unsafeSetProp prop val objNew
       pure objNew
#endif

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

-- Hack needed because $! doesn't have the same special treatment $ does to work with ST yet
#ifndef JS_RECORD
runST' :: (forall s. ST s a) -> a
runST' !s = runST s
#endif

-- | An empty record
rnil :: Rec '[]
rnil = unsafeRnil 0
{-# INLINE rnil #-}

-- | An empty record with an initial size for the record
unsafeRnil :: Int -> Rec '[]
#ifndef JS_RECORD
unsafeRnil (I# n#) =
    runST' $ ST $ \s# ->
    case newSmallArray# n# (error "No Value") s# of
      (# s'#, arr# #) ->
          case unsafeFreezeSmallArray# arr# s'# of
            (# s''#, a# #) -> (# s''# , Rec a# #)
#else
unsafeRnil _ =
    unsafePerformIO $! Rec <$> JS.create
#endif
{-# INLINE unsafeRnil #-}

-- | Prepend a record entry to a record 'Rec'
rcons ::
    forall l t lts s sortedLts.
    ( RecSize lts ~ s
    , sortedLts ~ Sort (l := t ': lts)
    , KnownNat s
    , KnownNat (RecVecIdxPos l sortedLts)
    , KeyDoesNotExist l lts
    , RecCopy lts lts sortedLts
#ifdef JS_RECORD
    , ToJSVal t
#endif
    )
    => l := t -> Rec lts -> Rec sortedLts

#ifndef JS_RECORD
rcons (_ := val) lts =
    runST' $ ST $ \s# ->
    case newSmallArray# newSize# (error "No value") s# of
      (# s'#, arr# #) ->
          case recCopyInto (Proxy :: Proxy lts) lts (Proxy :: Proxy sortedLts) arr# s'# of
            s''# ->
                case writeSmallArray# arr# setAt# (unsafeCoerce# val) s''# of
                  s'''# ->
                      case unsafeFreezeSmallArray# arr# s'''# of
                        (# s''''#, a# #) -> (# s''''#, Rec a# #)
    where
        !(I# setAt#) =
            fromIntegral (natVal' (proxy# :: Proxy# (RecVecIdxPos l sortedLts)))
        newSize# = size# +# 1#
        !(I# size#) = fromIntegral $ natVal' (proxy# :: Proxy# s)
#else
rcons (lbl := val) (Rec obj) =
    Rec $! unsafePerformIO $!
    do obj' <- copyObject obj
       val' <- toJSVal val
       JS.unsafeSetProp (JSS.pack $ symbolVal lbl) val' obj'
       pure obj'
#endif
{-# INLINE rcons #-}

class RecCopy (pts :: [*]) (lts :: [*]) (rts :: [*]) where
    recCopyInto ::
        Proxy pts -> Rec lts -> Proxy rts
        -> SmallMutableArray# s Any
        -> State# s
        -> State# s

instance RecCopy '[] lts rts where
    recCopyInto _ _ _ _ s# = s#

instance
    ( Has l rts t
    , Has l lts t
    , RecCopy (RemoveAccessTo l (l := t ': pts)) lts rts
    ) => RecCopy (l := t ': pts) lts rts where
    recCopyInto _ lts prxy tgt# s# =
        let lbl :: FldProxy l
            lbl = FldProxy
            val = get lbl lts
            pNext :: Proxy (RemoveAccessTo l (l := t ': pts))
            pNext = Proxy
            !(I# setAt#) =
                fromIntegral (natVal' (proxy# :: Proxy# (RecVecIdxPos l rts)))
        in case writeSmallArray# tgt# setAt# (unsafeCoerce# val) s# of
             s'# -> recCopyInto pNext lts prxy tgt# s'#

-- | Prepend a record entry to a record 'Rec'. Assumes that the record was created with
-- 'unsafeRnil' and still has enough free slots, mutates the original 'Rec' which should
-- not be reused after
unsafeRCons ::
    forall l t lts s.
    ( RecSize lts ~ s
    , KnownNat s
    , KeyDoesNotExist l lts
#ifdef JS_RECORD
    , ToJSVal t
#endif
    )
    => l := t -> Rec lts -> Rec (l := t ': lts)

#ifndef JS_RECORD
unsafeRCons (_ := val) (Rec vec#) =
    runST' $ ST $ \s# ->
    case unsafeThawSmallArray# vec# s# of
      (# s'#, arr# #) ->
          case writeSmallArray# arr# size# (unsafeCoerce# val) s'# of
            s''# ->
                case unsafeFreezeSmallArray# arr# s''# of
                  (# s'''#, a# #) -> (# s'''#, Rec a# #)
    where
        !(I# size#) = fromIntegral $ natVal' (proxy# :: Proxy# s)
#else
unsafeRCons (lbl := val) (Rec obj) =
    Rec $! unsafePerformIO $!
    do val' <- toJSVal val
       JS.unsafeSetProp (JSS.pack $ symbolVal lbl) val' obj
       pure obj
#endif
{-# INLINE unsafeRCons #-}

-- | Alias for 'rcons'
(&) ::
    forall l t lts s sortedLts.
    ( RecSize lts ~ s
    , sortedLts ~ Sort (l := t ': lts)
    , KnownNat s
    , KnownNat (RecVecIdxPos l sortedLts)
    , KeyDoesNotExist l lts
    , RecCopy lts lts sortedLts
#ifdef JS_RECORD
    , ToJSVal t
#endif
    )
    => l := t -> Rec lts -> Rec sortedLts
(&) = rcons
{-# INLINE (&) #-}

infixr 5 &

type family RecAll (c :: u -> Constraint) (rs :: [u]) :: Constraint where
  RecAll c '[] = ()
  RecAll c (r ': rs) = (c r, RecAll c rs)

type family KeyDoesNotExist (l :: Symbol) (lts :: [*]) :: Constraint where
    KeyDoesNotExist l '[] = 'True ~ 'True
    KeyDoesNotExist l (l := t ': lts) =
        TypeError
        ( 'Text "Duplicate key " ':<>: 'Text l
        )
    KeyDoesNotExist q (l := t ': lts) = KeyDoesNotExist q lts

type RecAppend lhs rhs = RecAppendH lhs rhs rhs '[]

type family ListConcat (xs :: [*]) (ys :: [*]) :: [*] where
    ListConcat '[] ys = ys
    ListConcat xs '[] = xs
    ListConcat (x ': xs) ys = x ': (ListConcat xs ys)

type family ListReverse (xs :: [*]) :: [*] where
    ListReverse (x ': xs) = ListConcat (ListReverse xs) '[x]
    ListReverse '[] = '[]

type family RecAppendH (lhs ::[*]) (rhs :: [*]) (rhsall :: [*]) (accum :: [*]) :: [*] where
    RecAppendH (l := t ': lhs) (m := u ': rhs) rhsall acc = RecAppendH (l := t ': lhs) rhs rhsall acc
    RecAppendH (l := t ': lhs) '[] rhsall acc = RecAppendH lhs rhsall rhsall (l := t ': acc)
    RecAppendH '[] rhs rhsall acc = ListConcat (ListReverse acc) rhsall

type family RecSize (lts :: [*]) :: Nat where
    RecSize '[] = 0
    RecSize (l := t ': lts) = 1 + RecSize lts

type RecVecIdxPos l lts = RecSize lts - RecTyIdxH 0 l lts - 1

type family RecTyIdxH (i :: Nat) (l :: Symbol) (lts :: [*]) :: Nat where
    RecTyIdxH idx l (l := t ': lts) = idx
    RecTyIdxH idx m (l := t ': lts) = RecTyIdxH (1 + idx) m lts
    RecTyIdxH idx m '[] =
        TypeError
        ( 'Text "Could not find label "
          ':<>: 'Text m
        )

type family RecTy (l :: Symbol) (lts :: [*]) :: k where
    RecTy l (l := t ': lts) = t
    RecTy q (l := t ': lts) = RecTy q lts

-- | Require a record to contain at least the listed labels
type family HasOf (req :: [*]) (lts :: [*]) :: Constraint where
    HasOf (l := t ': req) lts = (Has l lts t, HasOf req lts)
    HasOf '[] lts = 'True ~ 'True

-- | Require a record to contain a label
type Has l lts v =
   ( RecTy l lts ~ v
   , KnownNat (RecSize lts)
   , KnownNat (RecVecIdxPos l lts)
#ifdef JS_RECORD
   , KnownSymbol l, FromJSVal v, ToJSVal v
#endif
   )

-- | Get an existing record field
get ::
    forall l v lts.
    ( Has l lts v
    )
    => FldProxy l -> Rec lts -> v
#ifndef JS_RECORD
get _ (Rec vec#) =
    let !(I# readAt#) =
            fromIntegral (natVal' (proxy# :: Proxy# (RecVecIdxPos l lts)))
        anyVal :: Any
        anyVal =
           case indexSmallArray# vec# readAt# of
             (# a# #) -> a#
    in unsafeCoerce# anyVal
#else
get lbl (Rec obj) =
    unsafePerformIO $!
    do r <- JS.unsafeGetProp (JSS.pack $ symbolVal lbl) obj
       fromJSValUnchecked r
#endif
{-# INLINE get #-}

-- | Alias for 'get'
(&.) :: forall l v lts. (Has l lts v) => Rec lts -> FldProxy l -> v
(&.) = flip get
infixl 3 &.

-- | Update an existing record field
set ::
    forall l v lts.
    (Has l lts v)
    => FldProxy l -> v -> Rec lts -> Rec lts
#ifndef JS_RECORD
set _ !val (Rec vec#) =
    let !(I# size#) = fromIntegral $ natVal' (proxy# :: Proxy# (RecSize lts))
        !(I# setAt#) = fromIntegral (natVal' (proxy# :: Proxy# (RecVecIdxPos l lts)))
        dynVal :: Any
        !dynVal = unsafeCoerce# val
        r2 =
            runST' $ ST $ \s# ->
            case newSmallArray# size# (error "No value") s# of
              (# s'#, arr# #) ->
                  case copySmallArray# vec# 0# arr# 0# size# s'# of
                    s''# ->
                        case writeSmallArray# arr# setAt# dynVal s''# of
                          s'''# ->
                              case unsafeFreezeSmallArray# arr# s'''# of
                                (# s''''#, a# #) -> (# s''''#, Rec a# #)
    in r2
#else
set lbl !val (Rec obj) =
    unsafePerformIO $!
    do newObj <- copyObject obj
       val' <- toJSVal val
       JS.unsafeSetProp (JSS.pack $ symbolVal lbl) val' newObj
       pure $ Rec newObj
#endif
{-# INLINE set #-}

-- | Update an existing record field
modify ::
    forall l v lts.
    (Has l lts v)
    => FldProxy l -> (v -> v) -> Rec lts -> Rec lts
modify lbl fun r = set lbl (fun $ get lbl r) r
{-# INLINE modify #-}

-- | Constructor for field accessor paths
data lbl :& more = FldProxy lbl :& more
infixr 8 :&

-- | Constructor for field accessor paths
(&:) :: FldProxy q -> more -> q :& more
(&:) = (:&)
{-# INLINE (&:) #-}

infixr 8 &:

-- | Specialized version of (&:) to help writing the last piece of the path w/o
-- confusing the type checker
(&:-) :: FldProxy q -> FldProxy r -> q :& FldProxy r
(&:-) = (:&)
{-# INLINE (&:-) #-}

infixr 8 &:-

-- | Helper function to allow to clearing specify unknown 'IsLabel' cases
fld :: FldProxy l -> FldProxy l
fld = id

type family RecDeepTy (ps :: r) (lts :: [*]) :: * where
    RecDeepTy (l :& more) (l := Rec t ': lts) = RecDeepTy more t
    RecDeepTy (l :& more) (l := t ': lts) = t
    RecDeepTy (l :& more) (q := t ': lts) = RecDeepTy (l :& more) lts
    RecDeepTy (FldProxy l) '[l := t] = t
    RecDeepTy l '[l := t] = t

class RecApplyPath p x where
    -- | Perform a deep update, setting the key along the path to the
    -- desired value
    setPath' :: p -> (RecDeepTy p x -> RecDeepTy p x) -> Rec x -> Rec x

    -- | Perform a deep read
    getPath' :: p -> Rec x -> RecDeepTy p x

instance (Has l lts t, t ~ RecDeepTy (FldProxy l) lts) => RecApplyPath (FldProxy l) lts where
    setPath' = modify
    {-# INLINE setPath' #-}

    getPath' = get
    {-# INLINE getPath' #-}

instance
    ( RecDeepTy (l :& more) lts ~ RecDeepTy more rts
    , RecTy l lts ~ Rec rts
    , Has l lts v
    , v ~ Rec rts
    , RecApplyPath more rts
    ) => RecApplyPath (l :& more) lts where
    setPath' (x :& more) v r =
        let innerVal :: Rec rts
            innerVal = get x r
        in set x (setPath' more v innerVal) r
    {-# INLINE setPath' #-}

    getPath' (x :& more) r = getPath' more (get x r)
    {-# INLINE getPath' #-}

-- | Perform a deep update, setting the key along the path to the
-- desired value
setPath :: RecApplyPath k x => k -> RecDeepTy k x -> Rec x -> Rec x
setPath s v = setPath' s (const v)
{-# INLINE setPath #-}

-- | Perform a deep update, transforming the value at the final key
modifyPath :: RecApplyPath k x => k -> (RecDeepTy k x -> RecDeepTy k x) -> Rec x -> Rec x
modifyPath = setPath'
{-# INLINE modifyPath #-}

-- | Perform a deep read. This is somewhat similar to using (&.), but is useful
-- when you want to share a 'RecPath' between 'getPath', 'modifyPath' and/or 'setPath'
getPath :: RecApplyPath k x => k -> Rec x -> RecDeepTy k x
getPath = getPath'
{-# INLINE getPath #-}

-- | Combine two records
combine ::
    forall lhs rhs sortRes.
    ( KnownNat (RecSize lhs)
    , KnownNat (RecSize rhs)
    , KnownNat (RecSize lhs + RecSize rhs)
    , sortRes ~ Sort (RecAppend lhs rhs)
    , RecCopy lhs lhs sortRes
    , RecCopy rhs rhs sortRes
    )
    => Rec lhs
    -> Rec rhs
    -> Rec sortRes

#ifndef JS_RECORD
combine lts rts =
    let !(I# size#) =
            fromIntegral $ natVal' (proxy# :: Proxy# (RecSize lhs + RecSize rhs))
    in runST' $ ST $ \s# ->
            case newSmallArray# size# (error "No value") s# of
              (# s'#, arr# #) ->
                  case recCopyInto (Proxy :: Proxy lhs) lts (Proxy :: Proxy sortRes) arr# s'# of
                    s''# ->
                        case recCopyInto (Proxy :: Proxy rhs) rts (Proxy :: Proxy sortRes) arr# s''# of
                          s'''# ->
                              case unsafeFreezeSmallArray# arr# s'''# of
                                (# s''''#, a# #) -> (# s''''#, Rec a# #)
#else
combine (Rec o1) (Rec o2) =
    unsafePerformIO $
    Rec <$> mergeObjs o1 o2
#endif
{-# INLINE combine #-}

-- | Alias for 'combine'
(++:) ::
    forall lhs rhs sortRes.
    ( KnownNat (RecSize lhs)
    , KnownNat (RecSize rhs)
    , KnownNat (RecSize lhs + RecSize rhs)
    , sortRes ~ Sort (RecAppend lhs rhs)
    , RecCopy lhs lhs sortRes
    , RecCopy rhs rhs sortRes
    )
    => Rec lhs
    -> Rec rhs
    -> Rec sortRes
(++:) = combine
{-# INLINE (++:) #-}

data RecFields (flds :: [Symbol]) where
    RFNil :: RecFields '[]
    RFCons :: KnownSymbol f => FldProxy f -> RecFields xs -> RecFields (f ': xs)

recKeys :: forall t (lts :: [*]). RecKeys lts => t lts -> [String]
recKeys = recKeys' . recFields

recKeys' :: RecFields lts -> [String]
recKeys' x =
    case x of
      RFNil -> []
      RFCons q qs -> symbolVal q : recKeys' qs

-- | Get keys of a record on value and type level
class RecKeys (lts :: [*]) where
    type RecKeysT lts :: [Symbol]
    recFields :: t lts -> RecFields (RecKeysT lts)

instance RecKeys '[] where
    type RecKeysT '[] = '[]
    recFields _ = RFNil

instance (KnownSymbol l, RecKeys lts) => RecKeys (l := t ': lts) where
    type RecKeysT (l := t ': lts) = (l ': RecKeysT lts)
    recFields (_ :: f (l := t ': lts)) =
        let lbl :: FldProxy l
            lbl = FldProxy
            more :: Proxy lts
            more = Proxy
        in (lbl `RFCons` recFields more)

-- | Apply a function to each key element pair for a record
reflectRec ::
    forall c r lts. (RecApply lts lts c)
    => Proxy c
    -> (forall a. c a => String -> a -> r)
    -> Rec lts
    -> [r]
reflectRec _ f r =
    reverse $
    recApply (\(Dict :: Dict (c a)) s v xs -> (f s v : xs)) r (Proxy :: Proxy lts) []
{-# INLINE reflectRec #-}

-- | Fold over all elements of a record
reflectRecFold ::
    forall c r lts. (RecApply lts lts c)
    => Proxy c
    -> (forall a. c a => String -> a -> r -> r)
    -> Rec lts
    -> r
    -> r
reflectRecFold _ f r =
    recApply (\(Dict :: Dict (c a)) s v x -> f s v x) r (Proxy :: Proxy lts)
{-# INLINE reflectRecFold #-}

-- | Convert all elements of a record to a 'String'
showRec :: forall lts. (RecApply lts lts Show) => Rec lts -> [(String, String)]
showRec = reflectRec @Show Proxy (\k v -> (k, show v))

recToValue :: forall lts. (RecApply lts lts ToJSON) => Rec lts -> Value
recToValue r = object $ reflectRec @ToJSON Proxy (\k v -> (T.pack k, toJSON v)) r

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
    recApply :: (forall a. Dict (c a) -> String -> a -> b -> b) -> Rec rts -> Proxy lts -> b -> b

instance RecApply rts '[] c where
    recApply _ _ _ b = b

instance
    ( KnownSymbol l
    , RecApply rts (RemoveAccessTo l lts) c
    , Has l rts v
    , c v
    ) => RecApply rts (l := t ': lts) c where
    recApply f r (_ :: Proxy (l := t ': lts)) b =
        let lbl :: FldProxy l
            lbl = FldProxy
            val = get lbl r
            res = f Dict (symbolVal lbl) val b
            pNext :: Proxy (RemoveAccessTo l (l := t ': lts))
            pNext = Proxy
        in recApply f r pNext res

-- | Machinery to implement equality
class RecEq (rts :: [*]) (lts :: [*]) where
    recEq :: Rec rts -> Rec rts -> Proxy lts -> Bool

instance RecEq rts '[] where
    recEq _ _ _ = True

instance
    ( RecEq rts (RemoveAccessTo l lts)
    , Has l rts v
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
    , RecSize lts ~ s, KnownNat s, KeyDoesNotExist l lts
#ifdef JS_RECORD
    , ToJSVal t
#endif
    ) => RecJsonParse (l := t ': lts) where
    recJsonParse initSize obj =
        do let lbl :: FldProxy l
               lbl = FldProxy
           rest <- recJsonParse initSize obj
           (v :: t) <- obj .: T.pack (symbolVal lbl)
           pure $ unsafeRCons (lbl := v) rest

-- | Machinery for NFData
class RecNfData (lts :: [*]) (rts :: [*]) where
    recNfData :: Proxy lts -> Rec rts -> ()

instance RecNfData '[] rts where
    recNfData _ _ = ()

instance
    ( Has l rts v
    , NFData v
    , RecNfData (RemoveAccessTo l lts) rts
    ) => RecNfData (l := t ': lts) rts where
    recNfData (_ :: (Proxy (l := t ': lts))) r =
        let !v = get (FldProxy :: FldProxy l) r
            pNext :: Proxy (RemoveAccessTo l (l := t ': lts))
            pNext = Proxy
        in deepseq v (recNfData pNext r)

-- | Conversion helper to bring a Haskell type to a record. Note that the
-- native Haskell type must be an instance of 'Generic'
class FromNative a lts | a -> lts where
    fromNative' :: a x -> Rec lts

instance FromNative cs lts => FromNative (D1 m cs) lts where
    fromNative' (M1 xs) = fromNative' xs

instance FromNative cs lts => FromNative (C1 m cs) lts where
    fromNative' (M1 xs) = fromNative' xs

instance
    ( KnownSymbol name
#ifdef JS_RECORD
    , ToJSVal t
#endif
    )
    => FromNative (S1 ('MetaSel ('Just name) p s l) (Rec0 t)) '[name := t]
    where
    fromNative' (M1 (K1 t)) = (FldProxy :: FldProxy name) := t & rnil

instance
    ( FromNative l lhs
    , FromNative r rhs
    , lts ~ Sort (RecAppend lhs rhs)
    , RecCopy lhs lhs lts
    , RecCopy rhs rhs lts
    , KnownNat (RecSize lhs)
    , KnownNat (RecSize rhs)
    , KnownNat (RecSize lhs + RecSize rhs)
    )
    => FromNative (l :*: r) lts where
    fromNative' (l :*: r) = fromNative' l ++: fromNative' r

-- | Convert a native Haskell type to a record
fromNative :: (Generic a, FromNative (Rep a) lts) => a -> Rec lts
fromNative = fromNative' . from
{-# INLINE fromNative #-}

-- | Conversion helper to bring a record back into a Haskell type. Note that the
-- native Haskell type must be an instance of 'Generic'
#if MIN_VERSION_base(4, 10, 0)
class ToNative a lts where
    toNative' :: Rec lts -> a x
#else
class ToNative a lts | a -> lts where
    toNative' :: Rec lts -> a x
#endif

instance ToNative cs lts => ToNative (D1 m cs) lts where
    toNative' xs = M1 $ toNative' xs

instance ToNative cs lts => ToNative (C1 m cs) lts where
    toNative' xs = M1 $ toNative' xs

instance
    (Has name lts t)
    => ToNative (S1 ('MetaSel ('Just name) p s l) (Rec0 t)) lts
    where
    toNative' r =
        M1 $ K1 (get (FldProxy :: FldProxy name) r)

instance
    ( ToNative l lts
    , ToNative r lts
    )
    => ToNative (l :*: r) lts where
    toNative' r = toNative' r :*: toNative' r

#ifdef JS_RECORD
instance ToJSVal (Rec x) where
    toJSVal (Rec (JS.Object obj)) = pure obj

instance FromJSVal (Rec x) where
    fromJSVal jv = pure (Just $ Rec $ JS.Object jv) -- TODO: implement checking!!
#endif

-- | Convert a record to a native Haskell type
toNative :: (Generic a, ToNative (Rep a) lts) => Rec lts -> a
toNative = to . toNative'
{-# INLINE toNative #-}

-- | Like 'asks' for 'MonadReader', but you provide a record field you would like
-- to read from your environment
asksR :: (Has lbl lts v, MonadReader (Rec lts) m) => FldProxy lbl -> m v
asksR f = asks (get f)
{-# INLINE asksR #-}

-- | Like 'asks' for 'MonadReader', but you provide a record field you would like
-- to read from your environment
asksRP :: (RecApplyPath k x, MonadReader (Rec x) m) => k -> m (RecDeepTy k x)
asksRP p = asks (getPath p)
{-# INLINE asksRP #-}

-- | Like 'gets' for 'MonadState', but you provide a record field you would like
-- to read from your environment
getsR :: (Has lbl lts v, S.MonadState (Rec lts) m) => FldProxy lbl -> m v
getsR f = S.gets (get f)
{-# INLINE getsR #-}

-- | Similar to 'put' for 'MonadState', but you only set a single record field
setsR :: (Has lbl lts v, S.MonadState (Rec lts) m) => FldProxy lbl -> v -> m ()
setsR f v = S.modify (set f v)
{-# INLINE setsR #-}

-- | Similar to 'modify' for 'MonadState', but you update a single record field
modifiesR :: (Has lbl lts v, S.MonadState (Rec lts) m) => FldProxy lbl -> (v -> v) -> m ()
modifiesR f go = S.modify (modify f go)
{-# INLINE modifiesR #-}

-- | Similar to 'gets' for 'MonadState', but allows getting a value along a 'RecPath'
getsRP :: (RecApplyPath k x, S.MonadState (Rec x) m) => k -> m (RecDeepTy k x)
getsRP p = S.gets (getPath p)
{-# INLINE getsRP #-}

-- | Similar to 'put' for 'MonadState', but you only set a single record field
setsRP :: (RecApplyPath k x, S.MonadState (Rec x) m) => k -> RecDeepTy k x -> m ()
setsRP p v = S.modify (setPath p v)
{-# INLINE setsRP #-}

-- | Similar to 'modify' for 'MonadState', but you update a single record field
modifiesRP ::(RecApplyPath k x, S.MonadState (Rec x) m) => k -> (RecDeepTy k x -> RecDeepTy k x) -> m ()
modifiesRP p go = S.modify (modifyPath p go)
{-# INLINE modifiesRP #-}

type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

-- | Convert a field label to a lens
lens ::
    Has l lts v => FldProxy l -> Lens (Rec lts) (Rec lts) v v
lens lbl f r =
    fmap (\v -> set lbl v r) (f (get lbl r))
{-# INLINE lens #-}

#ifdef JS_RECORD
foreign import javascript unsafe "Object.assign({}, $1, $2)" mergeObjs ::
    JS.Object -> JS.Object -> IO JS.Object
#endif
