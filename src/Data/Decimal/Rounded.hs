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


newtype Rounded (scale :: Nat) = Rounded {
	unRounded :: Int64
} deriving (Enum, Eq, Ord)

instance KnownNat scale => Show (Rounded scale) where
	show = show . toIntegral

instance KnownNat scale => Num (Rounded scale) where
	(Rounded a) + (Rounded b) = Rounded $ a + b
	(Rounded a) - (Rounded b) = Rounded $ a - b
	(Rounded a) * (Rounded b) = Rounded $ a * b * 10 ^ e
		where e = natVal (Proxy :: Proxy scale)
	negate (Rounded a) = Rounded $ negate a
	abs (Rounded a) = Rounded $ abs a
	signum _ = throw LossOfPrecision
	fromInteger = floor

instance KnownNat scale => Real (Rounded scale) where
	toRational = (% 1) . toIntegral

instance KnownNat scale => Integral (Rounded scale) where
	quotRem (Rounded a) (Rounded b) = (Rounded q, Rounded r)
		where
			(q, r) = quotRem a $ b * 10 ^ e
			e = natVal (Proxy :: Proxy scale)
	toInteger = toIntegral

toIntegral :: forall scale int . (KnownNat scale, Integral int) => Rounded scale -> int
toIntegral (Rounded a) = fromIntegral a * 10 ^ e
	where e = natVal (Proxy :: Proxy scale)

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
