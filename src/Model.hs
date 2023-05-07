{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Model where

import Data.Foldable (find)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text, isInfixOf)
import TextShow (TextShow, showt)

-- data CoinData = CoinData {emissionYear :: Year, mint :: Mint, features :: CoinFeature}

-- taka reprezentacja utrudni parsowanie, bo nie bedzie sie dalo sparsowac roku, mennicy, ficzerow i sprawdzic czy to spoko
-- data RP2Coin
--   = Grosz1 (Year, Mint)

-- queries:
-- mam tekst (tytul), jaka to moneeeta?
-- obpytywanie po taksonomi!
-- chce odrozniac warianty (konstytucja 87 kamieni od konstytucji 100 kamieni). To musza byc oddzielne kategorie

-- mam rok, mennice, ficzery, wariant - daj mnie ceny

-- data ClassifierRule =

class (Eq a) => Classifier a where
  keywords :: a -> [Text]

newtype Year = Year {getYear :: Int} deriving (Eq, Ord, Num, TextShow, Show)

instance Classifier Year where
  -- TODO: Interpolate nicer
  keywords (Year x) = [" " `mappend` showt x]

-- Optimize with hashmap
matchesClassifier :: (Classifier a) => a -> Text -> Bool
matchesClassifier a txt =
  all (`isInfixOf` txt) $ keywords a

data Denomination
  = ZL10
  | ZL5
  | ZL2
  | ZL1
  | GR50
  | GR20
  | GR10
  | GR5
  | GR2
  | GR1
  deriving (Eq, Ord, Bounded, Enum, Show)

instance Classifier Denomination where
  keywords ZL10 = ["10 złotych "]
  keywords ZL5 = ["5 złotych "]
  keywords ZL2 = ["2 złote "]
  keywords ZL1 = ["1 złoty "]
  keywords GR50 = ["50 groszy "]
  keywords GR20 = ["20 groszy "]
  keywords GR10 = ["10 groszy "]
  keywords GR5 = ["5 groszy "]
  keywords GR2 = ["2 grosze "]
  keywords GR1 = ["1 grosz "]

data Mint = Paris | Birmingham deriving (Eq, Show, Bounded, Enum)

instance Classifier Mint where
  keywords Paris = [" Paryż "]
  keywords Birmingham = [" Birmingham "]

data CoinFeature = Trial | Reversed deriving (Eq, Show)

data CoinVariant = Jewels Int | OfMint Mint deriving (Eq, Show)

instance Classifier CoinVariant where
  keywords (Jewels x) = [showt x `mappend` " pereł"]
  keywords (OfMint mint) = mappend "mennica " <$> keywords mint

data CoinDef = CoinDef {denomination :: Denomination, variant :: Maybe CoinVariant, year :: Year} deriving (Show, Eq)

instance Classifier CoinDef where
  keywords (CoinDef d v y) = keywords d ++ keywords y ++ maybe [] keywords v

gr1Coins :: [CoinDef]
gr1Coins = fmap (CoinDef GR1 Nothing) [1925, 1926, 1927]

gr2Coins :: [CoinDef]
gr2Coins = fmap (CoinDef GR2 Nothing) [1925, 1926, 1927]

knownCoins :: [CoinDef]
knownCoins =
  gr1Coins
    ++ gr2Coins
    ++ [ CoinDef ZL5 (Just $ Jewels 81) 1925,
         CoinDef ZL5 (Just $ Jewels 100) 1925
       ]

data Coin = Coin {coindDef :: CoinDef, features :: [CoinFeature]}

-- bedziemy mogli obliczyc klucze naturalne ?

-- should return Coin ultimately
identifyCoin :: [CoinDef] -> Text -> [CoinDef]
identifyCoin defs q = filter (`matchesClassifier` q) defs

-- potem typ który opisuje "instancje" - z ficzerami i gradingiem (wspólne dla każdej monety)

foo :: String
foo = "witam"