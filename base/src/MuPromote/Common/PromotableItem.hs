{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module defines 'PromotableItem's and functions handling them.
module MuPromote.Common.PromotableItem
  where
  {- old export list:
    -- * Promotable items
    PromotableItem(..),
    PIObject(..),
    textType, textAtom,
    -}

import Control.Applicative
import Control.Monad
import Data.Aeson as DA
import Data.Aeson.Types
import Data.Hashable
import Data.Scientific
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64 as BS64
import qualified Data.HashMap.Lazy as HM
import Data.SafeCopy
import Data.Serialize
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Text.Encoding as TE

-- Types: (to be moved to muPromote-base)

-- | Promotable items.
type PromotableItem = PIObject

-- | An AtomType is a unicode "self descriptive" string, in the same sense
-- that mime-types are supposed to be. Also describes the encoding of the
-- AtomData.
type AtomType = T.Text

-- | The data contents of an atom is simply a string of bytes, is
-- interpretation depending on the readers understanding of the
-- corresponding AtomType.
type AtomData = LBS.ByteString

-- | Open-ended collection of data with minimal structure. Specifically,
-- they can be records of objects, lists of objects and data atoms with
-- a descriptive type tag, typically a mime-type.
data PIObject =
    -- Keys are a list of unicode symbols, abstract of encoding.
    PIOKV (HM.HashMap T.Text PIObject)
  | PIOList [PIObject]
  | PIODataAtom AtomType AtomData
  deriving (Eq, Show, Ord)

-- $(deriveSafeCopy 0 'base ''PIObject)
-- -ddump-splices:
instance SafeCopy PIObject where
  putCopy (PIOKV arg_a6p1)
    = contain
        (do { putWord8 0;
              safePut_HashMapTextPIObject_a6rm <- getSafePut;
              safePut_HashMapTextPIObject_a6rm arg_a6p1;
              return () })
  putCopy (PIOList arg_a6rn)
    = contain
        (do { putWord8 1;
              safePut_ListPIObject_a6ro <- getSafePut;
              safePut_ListPIObject_a6ro arg_a6rn;
              return () })
  putCopy (PIODataAtom arg_a6rp arg_a6rq)
    = contain
        (do { putWord8 2;
              safePut_Text_a6vx <- getSafePut;
              safePut_ByteString_a6vy <- getSafePut;
              safePut_Text_a6vx arg_a6rp;
              safePut_ByteString_a6vy arg_a6rq;
              return () })
  getCopy
    = contain
        (label
           "MuPromote.Common.PromotableItem.PIObject:"
           (do { tag_a6vz <- getWord8;
                 case tag_a6vz of {
                   0 -> do { safeGet_HashMapTextPIObject_a6vA <- getSafeGet;
                             ((return PIOKV) <*> safeGet_HashMapTextPIObject_a6vA) };
                   1 -> do { safeGet_ListPIObject_a6vB <- getSafeGet;
                             ((return PIOList) <*> safeGet_ListPIObject_a6vB) };
                   2 -> do { safeGet_Text_a6vC <- getSafeGet;
                             safeGet_ByteString_a6vD <- getSafeGet;
                             (((return PIODataAtom) <*> safeGet_Text_a6vC)
                              <*> safeGet_ByteString_a6vD) };
                   _ -> fail
                          ("Could not identify tag \""
                           ++
                             ((show tag_a6vz)
                              ++
                                "\" for type \"MuPromote.Common.PromotableItem.PIObject\" that has only 3 constructors.  Maybe your data is corrupted?")) } }))
  version = 0
  kind = base
  errorTypeName _ = "MuPromote.Common.PromotableItem.PIObject"

-- | A trivial SafeCopy instance for HM.HashMap.
instance (Hashable k, Eq k, SafeCopy k, SafeCopy v) => SafeCopy (HM.HashMap k v)  where

  kind = base
  version = 0

  getCopy = contain $ HM.fromList <$> join getSafeGet

  putCopy hm = contain $ join $ getSafePut <*> pure (HM.toList hm)

-- | A trivial Ord instance for HM.HashMap.
instance (Ord k, Ord v) => Ord (HM.HashMap k v)  where

  -- compare :: a -> a -> Bool
  -- (compare .) :: (c -> a) -> c -> a -> Ordering
  -- (. compare) :: ((a -> Ordering) -> d) -> a -> d

  compare = (. compare) (. HM.toList) . HM.toList


-- | A plain text string is identified by this type string.
textType :: AtomType
textType = "text/plain; charset=UTF-8"

boolType :: AtomType
boolType = "binary/bit; True=1, False=0"

-- | Arbitrary precision numbers. Currently just what the "Serialize
-- Rational" instance yields.
numberType :: AtomType
numberType = "binary/number/scientific"

-- | Create correctly annotated utf-8 text atoms.
textAtom :: T.Text -> PIObject
textAtom = PIODataAtom textType . LBS.fromStrict .  TE.encodeUtf8

isDataAtom :: PIObject -> Bool
isDataAtom (PIODataAtom _ _) = True
isDataAtom _ = False

matchTextAtom :: PIObject -> Maybe T.Text
matchTextAtom (PIODataAtom typ val) | typ == textType =
  either (const Nothing) Just $ TE.decodeUtf8' $ LBS.toStrict val
matchTextAtom _ = Nothing

matchBoolAtom :: PIObject -> Maybe Bool
matchBoolAtom (PIODataAtom typ val) | typ == boolType =
  case LBS.head val of
    0 -> Just False
    1 -> Just True
    _ -> Nothing
matchBoolAtom _ = Nothing

matchNumberAtom :: PIObject -> Maybe Scientific
matchNumberAtom (PIODataAtom typ val) | typ == numberType =
  either (const Nothing) (Just . fromRational) (runGetLazy get val)
matchNumberAtom _ = Nothing

numberAtom :: Scientific -> PIObject
numberAtom sci = PIODataAtom numberType (runPutLazy $ put $ toRational sci)

boolAtom :: Bool -> PIObject
boolAtom b = PIODataAtom boolType
  (if b then LBS.cons' 1 LBS.empty else LBS.cons' 0 LBS.empty)

piObject :: [(T.Text, PIObject)] -> PIObject
piObject = PIOKV . HM.fromList

piList :: [PIObject] -> PIObject
piList = PIOList

-- Give a mapping from normal-form JSON to 'PromotableItem's.
instance FromJSON PromotableItem where

  parseJSON (DA.String txt) =
    return $ textAtom txt

  parseJSON (DA.Bool b) =
    return $ boolAtom b

  parseJSON (DA.Number sciNum) =
    return $ numberAtom sciNum

  parseJSON (DA.Null) = modifyFailure (const "PromotableItems cannot be null") empty

  parseJSON (DA.Array vec) = piList <$> mapM parseJSON (V.toList vec)

  -- Parse the { type: "..", value: ... } transliteration to atoms, and if
  -- it doesn't match, just to KVpairs.
  parseJSON (DA.Object obj) = do
    typ <- obj .:? "type"
    val <- obj .:? "value"
    case (typ,val) of
      (Just t, Just v) ->
        PIODataAtom (T.pack t) <$> decodeBS64Val v
      _ -> PIOKV <$> HM.traverseWithKey (const parseJSON) obj

    where

      decodeBS64Val :: T.Text -> Parser LBS.ByteString
      decodeBS64Val t = either (flip typeMismatch (DA.String t)) pure $
        LBS.fromStrict <$> BS64.decode (TE.encodeUtf8 t)

instance ToJSON PromotableItem where

  toJSON (PIOKV kvs) = DA.Object $ HM.map toJSON kvs
  toJSON (PIOList l) = DA.Array $ V.fromList $ map toJSON l
  toJSON atom | Just txt <- matchTextAtom atom   = DA.String txt
  toJSON atom | Just num <- matchNumberAtom atom = DA.Number num
  toJSON atom | Just b   <- matchBoolAtom atom   = DA.Bool b
  toJSON (PIODataAtom typ val) =
    DA.object [
      ("type", DA.String typ),
      -- We trust that BS64.encode only produces valid utf8 strings
      -- and thus neglect error handling.
      ("value", DA.String $ TE.decodeUtf8 $ BS64.encode $ LBS.toStrict val)
      ]

{-
{-# LANGUAGE DeriveGeneric #-}
module MuPromote.Common.PromotableItem
  (
    -- * Promotable items
    PromotableItem(..),
    parsePromotable,
    renderPromotable,

    -- -- * Managing weight-item collections.
    -- mergeItems,
    -- addItem

  ) where
import Data.Aeson
import Data.SafeCopy
import Data.Serialize
import GHC.Generics

-- | The data type representing Promotable Items.
data PromotableItem = PromotableItem {
  -- | A name referring to the item, known by the promotion provider
  name :: String,

  -- | The provider that handles this promotable.
  promotionProcessor :: String
} deriving (Eq, Generic, Read, Show)

instance FromJSON PromotableItem
instance ToJSON PromotableItem

-- | A function for deserializing 'PromotableItem's.
parsePromotable :: String -> Maybe PromotableItem
parsePromotable str = case reads str of
  [] -> Nothing
  (p,""):_ -> Just p
  _ -> Nothing

-- | A function for serializing 'PromotableItem's.
renderPromotable :: PromotableItem -> String
renderPromotable = show

-- | Merge the PromotableItems xs into ys, summing accordingly.
mergeItems :: [(Double, PromotableItem)] -> [(Double, PromotableItem)] -> [(Double, PromotableItem)]
mergeItems xs ys = foldr addItem xs ys

-- | Actually add (and sum) the weight+item to enrolled items.
addItem :: (Double, PromotableItem) -> [(Double, PromotableItem)] -> [(Double, PromotableItem)]
addItem wi [] = [wi]
addItem (w,i) ((w',i'):r) | i == i' = (w+w', i):r
addItem wi (wi':r) = wi':(addItem wi r)

instance SafeCopy PromotableItem where

  version = 0

  putCopy pItem = contain $ do
    put $ name pItem
    put $ promotionProcessor pItem

  getCopy = contain $ do
    nm <- get
    pp <- get
    return $ PromotableItem nm pp
    -}
