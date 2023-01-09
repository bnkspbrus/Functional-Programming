{-# LANGUAGE LambdaCase #-}

module HW3.Evaluator(eval) where

import Codec.Compression.Zlib
  ( bestCompression,
    compressLevel,
    compressWith,
    decompress,
    defaultCompressParams,
  )
import Codec.Serialise (deserialise, serialise)
import Control.Monad (foldM)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Bifunctor (second)
import qualified Data.ByteString as B
  ( ByteString,
    append,
    drop,
    foldr,
    index,
    length,
    pack,
    reverse,
    take,
    unpack,
  )
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (toList)
import qualified Data.Map as M
  ( Map,
    elems,
    empty,
    fromList,
    fromListWith,
    insertWith,
    keys,
    lookup,
    map,
    mapKeys,
    toList,
  )
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as S
  ( Seq ((:<|)),
    cycleTaking,
    drop,
    fromFunction,
    fromList,
    index,
    length,
    reverse,
    singleton,
    take,
    (><),
  )
import qualified Data.Text as T
  ( Text,
    append,
    drop,
    foldr,
    index,
    length,
    pack,
    replicate,
    reverse,
    singleton,
    strip,
    take,
    toLower,
    toUpper,
    unpack,
  )
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Tuple (swap)
import GHC.Real (Ratio ((:%)), denominator, numerator)
import GHC.Word (Word8)
import HW3.Base
import System.FilePath ((</>))
import Text.Read (readMaybe)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalExpr

evalExpr :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalExpr (HiExprValue val) = return val
evalExpr (HiExprDict list) = HiValueDict . M.fromList <$> mapM mapPair list
evalExpr (HiExprApply fun args) = do
  eFun <- evalExpr fun
  apply eFun args
evalExpr (HiExprRun expr) = do
  eExpr <- evalExpr expr
  runExpr eExpr

mapPair :: HiMonad m => (HiExpr, HiExpr) -> ExceptT HiError m (HiValue, HiValue)
mapPair (x, y) = do
  x' <- evalExpr x
  y' <- evalExpr y
  return (x', y')

runExpr :: HiMonad m => HiValue -> ExceptT HiError m HiValue
runExpr (HiValueAction action) = ExceptT $ Right <$> runAction action
runExpr _ = ExceptT $ return $ Left HiErrorInvalidArgument

invalidArgument :: Monad m => ExceptT HiError m HiValue
invalidArgument = ExceptT $ return $ Left HiErrorInvalidArgument

arityMismatch :: Monad m => ExceptT HiError m HiValue
arityMismatch = ExceptT $ return $ Left HiErrorArityMismatch

binary :: HiMonad m => ((HiValue, HiValue) -> ExceptT HiError m HiValue) -> [HiExpr] -> ExceptT HiError m HiValue
binary fun [x, y] = do
  x' <- evalExpr x
  y' <- evalExpr y
  fun (x', y')
binary _ _ = arityMismatch

unary :: HiMonad m => (HiValue -> ExceptT HiError m HiValue) -> [HiExpr] -> ExceptT HiError m HiValue
unary fun [x] = do
  x' <- evalExpr x
  fun x'
unary _ _ = arityMismatch

apply :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
apply (HiValueString text) [x] =
  unary
    ( \case
        (HiValueNumber (idx :% 1)) ->
          return $
            let _idx = fromIntegral idx
             in if _idx >= 0 && _idx < T.length text
                  then HiValueString $ T.singleton $ T.index text _idx
                  else HiValueNull
        _ -> invalidArgument
    )
    [x]
apply (HiValueList list) [x] =
  unary
    ( \case
        (HiValueNumber (idx :% 1)) ->
          return $
            let _idx = fromIntegral idx
             in if _idx >= 0 && _idx < S.length list
                  then S.index list _idx
                  else HiValueNull
        _ -> invalidArgument
    )
    [x]
apply (HiValueBytes str) [x] =
  unary
    ( \case
        (HiValueNumber (idx :% 1)) ->
          return $
            let _idx = fromIntegral idx
             in if _idx >= 0 && _idx < B.length str
                  then HiValueNumber $ fromIntegral $ B.index str _idx
                  else HiValueNull
        _ -> invalidArgument
    )
    [x]
apply (HiValueList list) args =
  binary
    ( \case
        (HiValueNumber (start :% 1), HiValueNumber (end :% 1)) ->
          vlist $ S.drop (fromIntegral start) $ S.take (fromIntegral end) list
        _ -> invalidArgument
    )
    args
apply (HiValueBytes str) args =
  binary
    ( \case
        (HiValueNumber (start :% 1), HiValueNumber (end :% 1)) ->
          vbytes $ B.drop (fromIntegral start) $ B.take (fromIntegral end) str
        _ -> invalidArgument
    )
    args
apply (HiValueString text) args =
  binary
    ( \case
        (HiValueNumber (start :% 1), HiValueNumber (end :% 1)) ->
          vstring $ T.drop (toIndex text (fromIntegral start)) $ T.take (toIndex text (fromIntegral end)) text
        (HiValueNumber (start :% 1), HiValueNull) ->
          vstring $ T.drop (toIndex text (fromIntegral start)) text
        (HiValueNull, HiValueNumber (end :% 1)) ->
          vstring $ T.take (toIndex text (fromIntegral end)) text
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunAdd) args =
  binary
    ( \case
        (HiValueNumber x, HiValueNumber y) ->
          vnumber $ x + y
        (HiValueString x, HiValueString y) ->
          vstring $ T.append x y
        (HiValueList x, HiValueList y) ->
          vlist $ x S.>< y
        (HiValueBytes x, HiValueBytes y) ->
          vbytes $ B.append x y
        (HiValueTime x, HiValueNumber y) ->
          vtime $ addUTCTime (fromRational y) x
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunSub) args =
  binary
    ( \case
        (HiValueTime x, HiValueTime y) ->
          vnumber $ toRational $ diffUTCTime x y
        (HiValueNumber x, HiValueNumber y) ->
          vnumber $ x - y
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunMul) args =
  binary
    ( \case
        (HiValueNumber x, HiValueNumber y) ->
          vnumber $ x * y
        (HiValueString x, HiValueNumber (y :% 1)) ->
          vstring $ T.replicate (fromIntegral y) x
        (HiValueList x, HiValueNumber (y :% 1)) ->
          vlist $ S.cycleTaking (fromIntegral y * S.length x) x
        (HiValueBytes x, HiValueNumber (y :% 1)) ->
          vbytes $ B.pack $ take (fromIntegral y * B.length x) $ cycle $ B.unpack x
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunDiv) args =
  binary
    ( \case
        (HiValueNumber _, HiValueNumber (0 :% _)) ->
          ExceptT $ return $ Left HiErrorDivideByZero
        (HiValueNumber x, HiValueNumber y) ->
          vnumber $ x / y
        (HiValueString x, HiValueString y) ->
          vstring $ T.pack $ T.unpack x </> T.unpack y
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunAnd) [a, b] = do
  x' <- evalExpr a
  case x' of
    (HiValueBool False) -> return x'
    HiValueNull -> return x'
    _ -> evalExpr b
apply (HiValueFunction HiFunAnd) _ = arityMismatch
apply (HiValueFunction HiFunOr) [a, b] = do
  x' <- evalExpr a
  case x' of
    (HiValueBool False) -> evalExpr b
    HiValueNull -> evalExpr b
    _ -> return x'
apply (HiValueFunction HiFunOr) _ = arityMismatch
apply (HiValueFunction HiFunLessThan) args = binary (\(x, y) -> vbool $ x < y) args
apply (HiValueFunction HiFunGreaterThan) args = binary (\(x, y) -> vbool $ x > y) args
apply (HiValueFunction HiFunNotGreaterThan) args = binary (\(x, y) -> vbool $ x <= y) args
apply (HiValueFunction HiFunNotLessThan) args = binary (\(x, y) -> vbool $ x >= y) args
apply (HiValueFunction HiFunEquals) args = binary (\(x, y) -> vbool $ x == y) args
apply (HiValueFunction HiFunNotEquals) args = binary (\(x, y) -> vbool $ x /= y) args
apply (HiValueFunction HiFunNot) args =
  unary
    ( \case
        (HiValueBool x) -> vbool $ not x
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunIf) [a, b, c] = do
  x' <- evalExpr a
  case x' of
    (HiValueBool x) ->
      evalExpr $ if x then b else c
    _ -> invalidArgument
apply (HiValueFunction HiFunIf) _ = arityMismatch
apply (HiValueFunction HiFunLength) args =
  unary
    ( \case
        (HiValueString x) -> vnumber $ fromIntegral $ T.length x
        (HiValueList x) -> vnumber $ fromIntegral $ S.length x
        (HiValueBytes x) -> vnumber $ fromIntegral $ B.length x
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunToUpper) args =
  unary
    ( \case
        (HiValueString x) -> vstring $ T.toUpper x
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunToLower) args =
  unary
    ( \case
        (HiValueString x) -> vstring $ T.toLower x
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunReverse) args =
  unary
    ( \case
        (HiValueString x) -> vstring $ T.reverse x
        (HiValueList x) -> vlist $ S.reverse x
        (HiValueBytes x) -> vbytes $ B.reverse x
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunTrim) args =
  unary
    ( \case
        (HiValueString x) -> vstring $ T.strip x
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunList) xs = do
  xs' <- mapM evalExpr xs
  vlist $ S.fromList xs'
apply (HiValueFunction HiFunFold) args =
  binary
    ( \case
        (fun, HiValueList (h S.:<| t)) ->
          foldM (\x y -> apply fun [HiExprValue x, HiExprValue y]) h t
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunPackBytes) args =
  unary
    ( \case
        (HiValueList list) ->
          HiValueBytes . B.pack . toList <$> mapM mapToWord8 list
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunUnpackBytes) args =
  unary
    ( \case
        (HiValueBytes str) ->
          vlist $
            S.fromList $ map (HiValueNumber . fromIntegral) $ B.unpack str
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunEncodeUtf8) args =
  unary
    ( \case
        (HiValueString text) -> vbytes $ encodeUtf8 text
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunDecodeUtf8) args =
  unary
    ( \case
        (HiValueBytes str) ->
          return $
            either (const HiValueNull) HiValueString $ decodeUtf8' str
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunZip) args =
  unary
    ( \case
        (HiValueBytes str) ->
          vbytes $
            toStrict $
              compressWith
                defaultCompressParams {compressLevel = bestCompression}
                $ fromStrict str
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunUnzip) args =
  unary
    ( \case
        (HiValueBytes str) ->
          vbytes $ toStrict $ decompress $ fromStrict str
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunSerialise) args = unary (vbytes . toStrict . serialise) args
apply (HiValueFunction HiFunDeserialise) args =
  unary
    ( \case
        (HiValueBytes str) -> return $ deserialise $ fromStrict str
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunRead) args =
  unary
    ( \case
        (HiValueString text) -> vaction $ HiActionRead $ T.unpack text
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunWrite) args =
  binary
    ( \case
        (HiValueString text, HiValueString text2) ->
          vaction $ HiActionWrite (T.unpack text) (encodeUtf8 text2)
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunMkDir) args =
  unary
    ( \case
        (HiValueString text) -> vaction $ HiActionMkDir $ T.unpack text
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunChDir) args =
  unary
    ( \case
        (HiValueString text) -> vaction $ HiActionChDir $ T.unpack text
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunParseTime) args =
  unary
    ( \case
        (HiValueString text) ->
          return $
            case readMaybe (T.unpack text) :: Maybe UTCTime of
              (Just time) -> HiValueTime time
              Nothing -> HiValueNull
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunRange) args =
  binary
    ( \case
        (HiValueNumber start, HiValueNumber end) ->
          vlist $
            let len = fromIntegral $ quot (numerator (end - start)) (denominator (end - start)) + 1
             in S.fromFunction len (\i -> HiValueNumber $ fromIntegral i + start)
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunRand) args =
  binary
    ( \case
        (HiValueNumber (x :% 1), HiValueNumber (y :% 1)) ->
          vaction $ HiActionRand (fromIntegral x) (fromIntegral y)
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunEcho) args =
  unary
    ( \case
        (HiValueString text) -> vaction $ HiActionEcho text
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunKeys) args =
  unary
    ( \case
        (HiValueDict dict) -> vlist $ S.fromList $ M.keys dict
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunValues) args =
  unary
    ( \case
        (HiValueDict dict) -> vlist $ S.fromList $ M.elems dict
        _ -> invalidArgument
    )
    args
apply (HiValueDict dict) args =
  unary
    ( return . fromMaybe HiValueNull . flip M.lookup dict
    )
    args
apply (HiValueFunction HiFunCount) args =
  unary
    ( \case
        (HiValueString text) ->
          vdict $
            M.mapKeys (HiValueString . T.singleton) $
              M.map HiValueNumber $
                T.foldr (flip (M.insertWith (+)) 1) M.empty text
        (HiValueBytes str) ->
          vdict $
            M.mapKeys (HiValueNumber . toRational) $
              M.map HiValueNumber $
                B.foldr (flip (M.insertWith (+)) 1) M.empty str
        (HiValueList list) ->
          vdict $
            M.map HiValueNumber $
              foldr (flip (M.insertWith (+)) 1) M.empty list
        _ -> invalidArgument
    )
    args
apply (HiValueFunction HiFunInvert) args =
  unary
    ( \case
        (HiValueDict dict) -> vdict $ M.map HiValueList $ M.fromListWith (S.><) $ map fswap $ M.toList dict
        _ -> invalidArgument
    )
    args
apply _ _ = ExceptT $ return $ Left HiErrorInvalidFunction

fswap :: (HiValue, HiValue) -> (HiValue, S.Seq HiValue)
fswap = second S.singleton . swap

mapToWord8 :: Monad m => HiValue -> ExceptT HiError m Word8
mapToWord8 (HiValueNumber (num :% 1))
  | 0 <= num && num <= 255 = return $ fromIntegral num
mapToWord8 _ = ExceptT $ return $ Left HiErrorInvalidArgument

toIndex :: T.Text -> Int -> Int
toIndex text start = if start < 0 then T.length text + start else start

vtime :: Monad m => UTCTime -> ExceptT HiError m HiValue
vtime = return . HiValueTime

vlist :: Monad m => S.Seq HiValue -> ExceptT HiError m HiValue
vlist = return . HiValueList

vnumber :: Monad m => Rational -> ExceptT HiError m HiValue
vnumber = return . HiValueNumber

vaction :: Monad m => HiAction -> ExceptT HiError m HiValue
vaction = return . HiValueAction

vbool :: Monad m => Bool -> ExceptT HiError m HiValue
vbool = return . HiValueBool

vstring :: Monad m => T.Text -> ExceptT HiError m HiValue
vstring = return . HiValueString

vbytes :: Monad m => B.ByteString -> ExceptT HiError m HiValue
vbytes = return . HiValueBytes

vdict :: Monad m => M.Map HiValue HiValue -> ExceptT HiError m HiValue
vdict = return . HiValueDict
