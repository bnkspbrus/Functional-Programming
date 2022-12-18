module HW3.Pretty where

--module HW3.Pretty (prettyValue) where

import Data.ByteString (ByteString, unpack)
import Data.Foldable (toList)
import Data.Scientific (floatingOrInteger, fromRationalRepetendUnlimited)
import GHC.Real (Ratio ((:%)))
import HW3.Base
import Numeric (showHex)
import Prettyprinter
  ( Doc,
    Pretty,
    dquotes,
    encloseSep,
    list,
    parens,
    pretty,
    slash,
    space,
    tupled,
    (<+>),
  )
import Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber num) = prettyRational num
prettyValue (HiValueBool bool) = prettyBool bool
prettyValue (HiValueFunction fun) = prettyFunction fun
prettyValue (HiValueString text) = pString text
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueList _list) = list . map prettyValue . toList $ _list
prettyValue (HiValueBytes bstr) = pBytes bstr
prettyValue (HiValueAction HiActionCwd) = pretty "cwd"
prettyValue (HiValueAction (HiActionRead fp)) = pretty "read" <> parens (pString fp)
prettyValue (HiValueAction (HiActionWrite fp bstr)) = pretty "write" <> tupled [pString fp, pBytes bstr]
prettyValue (HiValueAction (HiActionMkDir fp)) = pretty "mkdir" <> parens (pString fp)
prettyValue (HiValueAction (HiActionChDir fp)) = pretty "cd" <> parens (pString fp)
prettyValue (HiValueAction HiActionNow) = pretty "now"
prettyValue (HiValueTime time) = pretty "parse-time" <> parens (pString $ show time)
prettyValue (HiValueAction (HiActionRand x y)) = pretty "rand" <> randList [pretty x, pretty y]
prettyValue (HiValueAction (HiActionEcho text)) = pretty "echo" <> parens (pString text)

byteList :: [Doc AnsiStyle] -> Doc AnsiStyle
byteList = encloseSep (pretty "[# ") (pretty " #]") space

randList :: [Doc AnsiStyle] -> Doc AnsiStyle
randList = encloseSep (pretty "( ") (pretty " )") (pretty ", ")

pBytes :: ByteString -> Doc AnsiStyle
pBytes bstr = byteList $ map (pretty . leadZero . flip showHex "") $ unpack bstr

pString :: Pretty a => a -> Doc AnsiStyle
pString fp = dquotes $ pretty fp

leadZero :: String -> String
leadZero [x] = ['0', x]
leadZero xs = xs

prettyFunction :: HiFun -> Doc AnsiStyle
prettyFunction HiFunIf = pretty "if"
prettyFunction HiFunDiv = pretty "div"
prettyFunction HiFunMul = pretty "mul"
prettyFunction HiFunAdd = pretty "add"
prettyFunction HiFunSub = pretty "sub"
prettyFunction HiFunNot = pretty "not"
prettyFunction HiFunAnd = pretty "and"
prettyFunction HiFunOr = pretty "or"
prettyFunction HiFunLessThan = pretty "less-than"
prettyFunction HiFunGreaterThan = pretty "greater-than"
prettyFunction HiFunEquals = pretty "equals"
prettyFunction HiFunNotLessThan = pretty "not-less-than"
prettyFunction HiFunNotGreaterThan = pretty "not-greater-than"
prettyFunction HiFunNotEquals = pretty "not-equals"
prettyFunction HiFunLength = pretty "lenght"
prettyFunction HiFunToUpper = pretty "to-upper"
prettyFunction HiFunToLower = pretty "to-lower"
prettyFunction HiFunReverse = pretty "reverse"
prettyFunction HiFunTrim = pretty "trim"
prettyFunction HiFunFold = pretty "fold"
prettyFunction HiFunList = pretty "list"
prettyFunction HiFunRange = pretty "range"
prettyFunction HiFunPackBytes = pretty "pack-bytes"
prettyFunction HiFunUnpackBytes = pretty "unpack-bytes"
prettyFunction HiFunZip = pretty "zip"
prettyFunction HiFunUnzip = pretty "unzip"
prettyFunction HiFunSerialise = pretty "serialise"
prettyFunction HiFunDeserialise = pretty "deserialise"
prettyFunction HiFunEncodeUtf8 = pretty "encode-utf8"
prettyFunction HiFunDecodeUtf8 = pretty "decode-utf8"
prettyFunction HiFunRead = pretty "read"
prettyFunction HiFunWrite = pretty "write"
prettyFunction HiFunMkDir = pretty "mkdir"
prettyFunction HiFunChDir = pretty "cd"
prettyFunction HiFunParseTime = pretty "parse-time"
prettyFunction HiFunRand = pretty "rand"
prettyFunction HiFunEcho = pretty "echo"

prettyBool :: Bool -> Doc AnsiStyle
prettyBool True = pretty "true"
prettyBool False = pretty "false"

prettyRational :: Rational -> Doc AnsiStyle
prettyRational v@(x :% y) = case fromRationalRepetendUnlimited v of
  (s, Nothing) -> either pretty pretty (floatingOrInteger s :: Either Double Integer)
  _ ->
    let sign = if x * y > 0 then "+" else "-"
     in let frac = pretty (abs (rem x y)) <> slash <> pretty (abs y)
         in case quot x y of
              0 -> if sign == "-" then pretty sign <> frac else frac
              q -> pretty q <+> pretty sign <+> frac
