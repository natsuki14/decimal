{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Decimal.Fixed (
	Fixed,
	Fixed18,
	Fixed18_0,
	Fixed18_1,
	Fixed18_2,
	Fixed18_3,
	Fixed18_4,
	Fixed18_5,
	Fixed18_6,
	Fixed18_7,
	Fixed18_8,
	Fixed18_9,
	Fixed18_10,
	Fixed18_11,
	Fixed18_12,
	Fixed18_13,
	Fixed18_14,
	Fixed18_15,
	Fixed18_16,
	Fixed18_17,
	Fixed18_18,
	Fixed9,
	Fixed9_0,
	Fixed9_1,
	Fixed9_2,
	Fixed9_3,
	Fixed9_4,
	Fixed9_5,
	Fixed9_6,
	Fixed9_7,
	Fixed9_8,
	Fixed9_9,

	fromRationalRoundUp
) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Char (isNumber)
import Data.Functor (($>))
import Data.Proxy (Proxy(..))
import Data.Ratio ((%), numerator, denominator)
import qualified Data.Scientific as S
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Class (PersistField(..))
import Database.Persist.Sql (PersistFieldSql(..))
import Database.Persist.Types(SqlType(..), PersistValue(..))
import GHC.TypeLits (KnownNat, Nat, natVal)
import qualified Text.ParserCombinators.ReadP as P
import Text.Read (readEither)


newtype Fixed (precision :: Nat) (scale :: Nat) = Fixed {
	unFixedBase :: Integer
} deriving (Enum, Eq, Ord)

instance KnownNat scale => Num (Fixed precision scale) where
	(Fixed a) + (Fixed b) = Fixed $ a + b
	(Fixed a) * (Fixed b) = Fixed $ a * b `div` 10 ^ e
		where e = natVal (Proxy :: Proxy scale)
	abs (Fixed a) = Fixed $ abs a
	signum (Fixed a) = Fixed $ signum a * 10 ^ e
		where e = natVal (Proxy :: Proxy scale)
	fromInteger i = Fixed $ i * 10 ^ e
		where e = natVal (Proxy :: Proxy scale)
	negate (Fixed a) = Fixed $ negate a

instance KnownNat scale => Fractional (Fixed precision scale) where
	fromRational r = Fixed $ (numerator r * 10 ^ e) `div` denominator r
		where e = natVal (Proxy :: Proxy scale)
	(Fixed a) / (Fixed b) = Fixed $ (a * 10 ^ e) `div` b
		where e = natVal (Proxy :: Proxy scale)

instance KnownNat scale => Real (Fixed precision scale) where
	toRational (Fixed a) = a % (10 ^ e)
		where e = natVal (Proxy :: Proxy scale)

instance KnownNat scale => RealFrac (Fixed precision scale) where
	properFraction (Fixed a) = (fromIntegral i, Fixed f)
		where
			e = natVal (Proxy :: Proxy scale)
			(i, f) = a `quotRem` (10 ^ e)

instance KnownNat scale => Show (Fixed precision scale) where
	show (Fixed a) = if l <= e
		then s ++ "0." ++ replicate (e - l) '0' ++ n
		else s ++ i ++ "." ++ f
		where
			e = fromIntegral $ natVal (Proxy :: Proxy scale)
			s = if a < 0 then "-" else ""
			n = show $ abs a
			l = length n
			(i, f) = splitAt (l - e) n

instance KnownNat scale => Read (Fixed precision scale) where
	readsPrec _ = P.readP_to_S $ do
		P.skipSpaces
		sign <- P.option 1 $ P.char '-' $> (-1)
		i <- P.option 0 $ do
			is <- P.munch isNumber
			case readEither is of
				Left err -> fail err
				Right i  -> return i
		f <- P.option 0 $ do
			P.char '.'
			fs <- P.munch isNumber
			case readEither . take scale' $ fs ++ replicate scale' '0' of
				Left err -> fail err
				Right f  -> return f
		(eSign, e) <- P.option (1, 0) $ do
			P.choice [P.char 'e', P.char 'E']
			eSign <- P.option 1 $ P.char '-' $> (-1) :: P.ReadP Int
			es <- P.munch isNumber
			case readEither es of
				Left err -> fail err
				Right e  -> return $ (eSign, e)
		if eSign >= 0
			then return . Fixed $ sign * (i * 10 ^ scale + f) * 10 ^ e
			else return . Fixed $ sign * (i * 10 ^ scale + f) `div` (10 ^ e)
		where
			scale = natVal (Proxy :: Proxy scale)
			scale' = fromIntegral scale

fromRationalRoundUp :: forall precision scale . KnownNat scale => Rational -> Fixed precision scale
fromRationalRoundUp r = Fixed $ q + if m > 0
	then 1
	else 0
	where
		e = natVal (Proxy :: Proxy scale)
		n = numerator r * 10 ^ e
		d = denominator r
		(q, m) = n `divMod` d

type Fixed18 scale = Fixed 18 scale
type Fixed18_0  = Fixed18  0
type Fixed18_1  = Fixed18  1
type Fixed18_2  = Fixed18  2
type Fixed18_3  = Fixed18  3
type Fixed18_4  = Fixed18  4
type Fixed18_5  = Fixed18  5
type Fixed18_6  = Fixed18  6
type Fixed18_7  = Fixed18  7
type Fixed18_8  = Fixed18  8
type Fixed18_9  = Fixed18  9
type Fixed18_10 = Fixed18 10
type Fixed18_11 = Fixed18 11
type Fixed18_12 = Fixed18 12
type Fixed18_13 = Fixed18 13
type Fixed18_14 = Fixed18 14
type Fixed18_15 = Fixed18 15
type Fixed18_16 = Fixed18 16
type Fixed18_17 = Fixed18 17
type Fixed18_18 = Fixed18 18

type Fixed9 scale = Fixed 9 scale
type Fixed9_0 = Fixed9 0
type Fixed9_1 = Fixed9 1
type Fixed9_2 = Fixed9 2
type Fixed9_3 = Fixed9 3
type Fixed9_4 = Fixed9 4
type Fixed9_5 = Fixed9 5
type Fixed9_6 = Fixed9 6
type Fixed9_7 = Fixed9 7
type Fixed9_8 = Fixed9 8
type Fixed9_9 = Fixed9 9

jsonTypeName :: Value -> String
jsonTypeName (Number _) = "number"
jsonTypeName (Object _) = "object"
jsonTypeName (Array  _) = "array"
jsonTypeName (String _) = "string"
jsonTypeName (Bool   _) = "bool"
jsonTypeName Null       = "null"

fromPersistValueError :: Text -> Text -> PersistValue -> Text
fromPersistValueError haskellType databaseType received = T.concat [
	"Failed to parse Haskell type `",
	haskellType,
	"`; expected ",
	databaseType,
	" from database, but received: ",
	T.pack $ show received,
	". Potential solution: Check that your database schema matches your Persistent model definitions." ]


instance KnownNat scale => FromJSON (Fixed precision scale) where
	parseJSON (Number value) = return $ floorFixed value
	parseJSON value = fail $ "Cannot convert a JSON " ++ jsonTypeName value ++ " to a decimal"

instance KnownNat scale => ToJSON (Fixed precision scale) where
	toJSON d = Number $ S.scientific coeff $ (- fromIntegral scale)
		where
			coeff = floor $ d * fromIntegral (10 ^ scale)
			scale = natVal (Proxy :: Proxy scale)

instance KnownNat scale => PersistField (Fixed precision scale) where
	toPersistValue d = PersistRational $ coeff % scale
		where
			coeff = floor $ d * fromIntegral scale
			scale = 10 ^ natVal (Proxy :: Proxy scale)
	fromPersistValue (PersistDouble   d) = Right $ floorFixed d
	fromPersistValue (PersistRational r) = Right $ floorFixed r
	fromPersistValue (PersistInt64    i) = Right $ fromIntegral i
	fromPersistValue value = Left $
		fromPersistValueError "Fixed" "double, rational, or integer" value

instance (KnownNat precision, KnownNat scale)
		=> PersistFieldSql (Fixed precision scale) where
	sqlType _ = SqlNumeric p s
		where
			p = fromIntegral $ natVal (Proxy :: Proxy precision)
			s = fromIntegral $ natVal (Proxy :: Proxy scale)

floorFixed :: forall f precision scale . (RealFrac f, KnownNat scale) => f -> Fixed precision scale
floorFixed f = fromRational $ floor (f * fromIntegral scale) % scale
	where scale = 10 ^ natVal (Proxy :: Proxy scale)
