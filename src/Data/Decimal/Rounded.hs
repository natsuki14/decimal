{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Decimal.Rounded where

import Prelude hiding (floor, round)
import qualified Prelude

import Control.Exception (ArithException(..), throw)
import Control.Monad ((<=<))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Int (Int64)
import Data.Proxy (Proxy(..))
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Class (PersistField(..))
import Database.Persist.Sql (PersistFieldSql(..))
import Database.Persist.Types(SqlType(..), PersistValue(..))
import GHC.TypeLits (KnownNat, Nat, natVal)


data Rounded (scale :: Nat) = PositiveUnit | NegativeUnit | Rounded {
	unRounded :: Int64
} deriving (Eq, Ord)

instance Enum (Rounded scale) where
	toEnum n
		| n ==  1 = PositiveUnit
		| n == -1 = NegativeUnit
		| n <   0  = Rounded . fromIntegral $ n - 1
		| n ==  0  = Rounded 0
		| n >   0  = Rounded . fromIntegral $ n + 1
	fromEnum (Rounded n)
		| n < -1     = fromIntegral $ n - 1
		| n >  1     = fromIntegral $ n + 1
		| otherwise  = fromIntegral $ n

instance KnownNat scale => Show (Rounded scale) where
	show n@(Rounded _) = show $ toIntegral n
	show PositiveUnit = "PositiveUnit"
	show NegativeUnit = "NegativeUnit"

instance KnownNat scale => Num (Rounded scale) where
	(Rounded a) + (Rounded b)  = Rounded $ a + b
	(Rounded a) + PositiveUnit = Rounded a
	(Rounded a) + NegativeUnit = Rounded a
	PositiveUnit + PositiveUnit = PositiveUnit
	PositiveUnit + NegativeUnit = Rounded 0
	NegativeUnit + NegativeUnit = NegativeUnit
	b + a = a + b

	(Rounded a) * (Rounded b) = Rounded $ a * b * 10 ^ e
		where e = natVal (Proxy :: Proxy scale)
	a * PositiveUnit = a
	a * NegativeUnit = negate a
	b * a = a * b

	negate (Rounded a)  = Rounded $ negate a
	negate PositiveUnit = NegativeUnit
	negate NegativeUnit = PositiveUnit

	abs (Rounded a) = Rounded $ abs a
	abs _           = PositiveUnit

	signum (Rounded a)
		| a <  0 = NegativeUnit
		| a == 0 = 0
		| a >  0 = PositiveUnit

	fromInteger = floor

instance KnownNat scale => Real (Rounded scale) where
	toRational = (% 1) . toIntegral

instance KnownNat scale => Integral (Rounded scale) where
	quotRem (Rounded a) (Rounded b) = (Rounded q, Rounded r)
		where
			(q, r) = quotRem a $ b * 10 ^ e
			e = natVal (Proxy :: Proxy scale)
	quotRem (Rounded a) PositiveUnit = (Rounded a, 0)
	quotRem (Rounded a) NegativeUnit = (Rounded (-a), 0)
	quotRem PositiveUnit (Rounded _) = (0, PositiveUnit)
	quotRem PositiveUnit b           = (b, 0)
	quotRem NegativeUnit (Rounded a) = (0, NegativeUnit)
	quotRem NegativeUnit b           = (-b, 0)
	toInteger = toIntegral

toIntegral :: forall scale int . (KnownNat scale, Integral int) => Rounded scale -> int
toIntegral (Rounded a) = fromIntegral a * 10 ^ e
	where e = natVal (Proxy :: Proxy scale)
toIntegral _           = 0

floor :: forall scale r . (KnownNat scale, Real r) => r -> Rounded scale
floor r = Rounded . Prelude.floor $ toRational r / 10 ^ e
	where e = natVal (Proxy :: Proxy scale)

fromReal' :: forall scale int . (KnownNat scale, Integral int) => int -> Either String (Rounded scale)
fromReal' n
	| m == 0    = Right . Rounded . fromIntegral $ d
	| otherwise = Left $ "Cannot represent " ++ show (fromIntegral n) ++ " as a Rounded " ++ show e
	where
		e = natVal (Proxy :: Proxy scale)
		(d, m) = n `divMod` (10 ^ e)

fromReal :: forall scale int m . (KnownNat scale, Integral int, MonadFail m) => int -> m (Rounded scale)
fromReal n = case fromReal' n of
	Left err -> fail err
	Right v  -> return v

adjustScale :: forall scale1 scale2 . (KnownNat scale1, KnownNat scale2) => Rounded scale1 -> Rounded scale2
adjustScale (Rounded a)
	| e1 >= e2  = Rounded $ a * 10 ^ (e1 Prelude.- e2)
	| otherwise = Rounded $ a `div` 10 ^ (e2 Prelude.- e1)
	where
		e1 = natVal (Proxy :: Proxy scale1)
		e2 = natVal (Proxy :: Proxy scale2)
adjustScale PositiveUnit = PositiveUnit
adjustScale NegativeUnit = NegativeUnit

scale :: forall s int . (KnownNat s, Integral int) => Rounded s -> int
scale _ = fromIntegral . natVal $ (Proxy :: Proxy s)

zero :: KnownNat scale => Rounded scale
zero = Rounded 0

type Rounded1 = Rounded 1
type Rounded2 = Rounded 2
type Rounded3 = Rounded 3
type Rounded4 = Rounded 4
type Rounded5 = Rounded 5
type Rounded6 = Rounded 6
type Rounded7 = Rounded 7
type Rounded8 = Rounded 8
type Rounded9 = Rounded 9


instance KnownNat scale => FromJSON (Rounded scale) where
	parseJSON value = do
		n <- parseJSON value
		fromReal (n :: Integer)

instance KnownNat scale => ToJSON (Rounded scale) where
	toJSON = toJSON . (toIntegral :: Rounded scale -> Integer)

instance KnownNat scale => PersistField (Rounded scale) where
	toPersistValue = toPersistValue . (toIntegral :: Rounded scale -> Int64)
	fromPersistValue value = do
		base <- fromPersistValue value
		case fromReal' (base :: Int64) of
			Left err -> Left $ T.pack err
			Right d  -> Right d

instance KnownNat scale => PersistFieldSql (Rounded scale) where
	sqlType _ = sqlType (Proxy :: Proxy Int64)
