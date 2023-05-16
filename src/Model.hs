{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Model (identifyCoin, rp2CoinDefs, Coin (..), CoinDef (..), CoinType (..), CoinFeature (..)) where

import Core (NormalizedWords, containsAll, containsNone, toNormalizedWords)
import Data.Maybe (mapMaybe)
import Data.Text (Text, unwords)
import TextShow (TextShow, fromText, showb, showt)

data ClassifierRule = MustContain Text | MustNotContain Text deriving (Show, Eq)

class (Eq a) => Classifier a where
  classifierRule :: a -> [ClassifierRule]

newtype Year = Year {getYear :: Int} deriving (Eq, Ord, Num, TextShow, Show)

instance Classifier Year where
  classifierRule (Year x) = [MustContain $ " " `mappend` showt x]

data CoinType
  = ZL20Chrobry
  | ZL10Polonia
  | ZL10PoloniaBezZnaku
  | ZL10Sobieski
  | ZL10Traugutt
  | ZL10PilsudskiStrzelecki
  | ZL10Pilsudski
  | ZL10Chrobry
  | ZL5Konstytucja100Perel
  | ZL5Konstytucja81Perel
  | ZL5Nike
  | ZL5NikeBezZnaku
  | ZL5Sztandar
  | ZL5SztandarGleboki
  | ZL5Polonia
  | ZL5PoloniaBezZnaku
  | ZL5PilsudskiStrzelecki
  | ZL5Pilsudski
  | ZL5Zaglowiec
  | ZL2Zniwiarka
  | ZL2ZniwiarkaParyz
  | ZL2ZniwiarkaFiladelfia
  | ZL2ZniwiarkaLondyn
  | ZL2Polonia
  | ZL2Pilsudski
  | ZL2Zaglowiec
  | ZL1Zniwiarka
  | ZL1
  | GR50
  | GR20
  | GR10
  | GR5
  | GR2
  | GR1
  deriving (Eq, Ord, Bounded, Enum, Show)

instance TextShow CoinType where
  showb ZL20Chrobry = fromText "20 złotych (Chrobry)"
  showb ZL10Polonia = fromText "10 złotych (Polonia)"
  showb ZL10PoloniaBezZnaku = fromText "10 złotych (Polonia) bez znaku mennicy"
  showb ZL10Sobieski = fromText "10 złotych (Sobieski)"
  showb ZL10Traugutt = fromText "10 złotych (Traugutt)"
  showb ZL10PilsudskiStrzelecki = fromText "10 złotych (Piłsudski Strzelecki)"
  showb ZL10Pilsudski = fromText "10 złotych"
  showb ZL10Chrobry = fromText "10 złotych (Chrobry)"
  showb ZL5Konstytucja100Perel = fromText "5 złotych (Konstytucja 100 perełek)"
  showb ZL5Konstytucja81Perel = fromText "5 złotych (Konstytucja 81 perełek)"
  showb ZL5Nike = fromText "5 złotych (Nike)"
  showb ZL5NikeBezZnaku = fromText "5 złotych (Nike) bez znaku mennicy"
  showb ZL5Sztandar = fromText "5 złotych (Sztandar)"
  showb ZL5SztandarGleboki = fromText "5 złotych (Sztandar) stempel głęboki"
  showb ZL5Polonia = fromText "5 złotych (Polonia)"
  showb ZL5PoloniaBezZnaku = fromText "5 złotych (Polonia) bez znaku mennicy"
  showb ZL5PilsudskiStrzelecki = fromText "5 złotych (Piłsudski Strzelecki)"
  showb ZL5Pilsudski = fromText "5 złotych (Piłsudski)"
  showb ZL5Zaglowiec = fromText "5 złotych (Żaglowiec)"
  showb ZL2Zniwiarka = fromText "5 złotych (Żniwiarka)"
  showb ZL2ZniwiarkaParyz = fromText "5 złotych (Żniwiarka) mennica Paryż"
  showb ZL2ZniwiarkaFiladelfia = fromText "5 złotych (Żniwiarka) mennica Filadelfia"
  showb ZL2ZniwiarkaLondyn = fromText "5 złotych (Żniwiarka) mennica Londyn"
  showb ZL2Polonia = fromText "2 złotych (Polonia)"
  showb ZL2Pilsudski = fromText "2 złotych (Piłsudski)"
  showb ZL2Zaglowiec = fromText "2 złotych (Żaglowiec)"
  showb ZL1Zniwiarka = fromText "1 złotych (Żniwiarka)"
  showb ZL1 = fromText "1 złotych"
  showb GR50 = fromText "50 groszy"
  showb GR20 = fromText "20 groszy"
  showb GR10 = fromText "10 groszy"
  showb GR5 = fromText "5 groszy"
  showb GR2 = fromText "2 groszy"
  showb GR1 = fromText "1 grosz"

instance Classifier CoinType where
  classifierRule ZL20Chrobry = [MustContain "20 złotych", MustContain "chrobry"]
  classifierRule ZL10Chrobry = [MustContain "10 złotych", MustContain "chrobry"]
  classifierRule ZL10Pilsudski = [MustContain "10 złotych", MustContain "piłsudski", MustNotContain "strzelecki"]
  classifierRule ZL10PilsudskiStrzelecki = [MustContain "10 złotych", MustContain "strzelecki"]
  classifierRule ZL10Traugutt = [MustContain "10 złotych", MustContain "traugutt"]
  classifierRule ZL10Sobieski = [MustContain "10 złotych", MustContain "sobieski"]
  classifierRule ZL10PoloniaBezZnaku = [MustContain "10 złotych", MustContain "bez znaku"]
  classifierRule ZL10Polonia = [MustContain "10 złotych", MustNotContain "bez znaku"]
  classifierRule ZL5Zaglowiec = [MustContain "5 złotych", MustContain "żaglowiec"]
  classifierRule ZL5Pilsudski = [MustContain "5 złotych", MustContain "piłsudski", MustNotContain "strzelecki"]
  classifierRule ZL5PilsudskiStrzelecki = [MustContain "5 złotych", MustContain "strzelecki"]
  classifierRule ZL5PoloniaBezZnaku = [MustContain "5 złotych", MustContain "głowa kobiety", MustContain "bez znaku"]
  classifierRule ZL5Polonia = [MustContain "5 złotych", MustContain "głowa kobiety"]
  classifierRule ZL5SztandarGleboki = [MustContain "5 złotych", MustContain "sztandar", MustContain "głeboki"]
  classifierRule ZL5Sztandar = [MustContain "5 złotych", MustContain "sztandar", MustNotContain "głeboki"]
  classifierRule ZL5NikeBezZnaku = [MustContain "5 złotych", MustContain "nike", MustContain "bez znaku"]
  classifierRule ZL5Nike = [MustContain "5 złotych", MustContain "nike", MustNotContain "bez znaku"]
  classifierRule ZL5Konstytucja81Perel = [MustContain "5 złotych", MustContain "konstytucja", MustContain "81", MustNotContain "100"]
  classifierRule ZL5Konstytucja100Perel = [MustContain "5 złotych", MustContain "konstytucja", MustContain "100", MustNotContain "81"]
  classifierRule ZL2Zaglowiec = [MustContain "2 złote", MustContain "żaglowiec"]
  classifierRule ZL2ZniwiarkaLondyn = [MustContain "2 złote", MustContain "Londyn"]
  classifierRule ZL2ZniwiarkaFiladelfia = [MustContain "2 złote", MustContain "Filadelfia"]
  classifierRule ZL2ZniwiarkaParyz = [MustContain "2 złote", MustContain "Paryż"]
  classifierRule ZL2Zniwiarka = [MustContain "2 złote", MustNotContain "Paryż", MustNotContain "Londyn", MustNotContain "Filadelfia"]
  classifierRule ZL2Polonia = [MustContain "2 złote", MustContain "głowa kobiety"]
  classifierRule ZL2Pilsudski = [MustContain "2 złote", MustContain "piłsudski"]
  classifierRule ZL1Zniwiarka = [MustContain "1 złoty"]
  classifierRule ZL1 = [MustContain "1 złoty", MustNotContain "kobieta", MustNotContain "kłosy", MustNotContain "żniwiarka"]
  classifierRule GR50 = [MustContain "50 groszy"]
  classifierRule GR20 = [MustContain "20 groszy"]
  classifierRule GR10 = [MustContain "10 groszy"]
  classifierRule GR5 = [MustContain "5 groszy"]
  classifierRule GR2 = [MustContain "2 grosze"]
  classifierRule GR1 = [MustContain "1 grosz"]

class EmissionYears a where
  getEmissionYears :: a -> [Year]

instance EmissionYears CoinType where
  getEmissionYears ZL20Chrobry = [1925]
  getEmissionYears ZL10Chrobry = [1925]
  getEmissionYears ZL10Pilsudski = [1934, 1935, 1936, 1937, 1938, 1939]
  getEmissionYears ZL10PilsudskiStrzelecki = [1934]
  getEmissionYears ZL10Traugutt = [1933]
  getEmissionYears ZL10Sobieski = [1933]
  getEmissionYears ZL10PoloniaBezZnaku = [1932]
  getEmissionYears ZL10Polonia = [1932, 1933]
  getEmissionYears ZL5Zaglowiec = [1936]
  getEmissionYears ZL5Pilsudski = [1934, 1935, 1936, 1938]
  getEmissionYears ZL5PilsudskiStrzelecki = [1934]
  getEmissionYears ZL5PoloniaBezZnaku = [1932]
  getEmissionYears ZL5Polonia = [1932, 1933, 1934]
  getEmissionYears ZL5SztandarGleboki = [1930]
  getEmissionYears ZL5Sztandar = [1930]
  getEmissionYears ZL5NikeBezZnaku = [1928]
  getEmissionYears ZL5Nike = [1928, 1930, 1931, 1932]
  getEmissionYears ZL5Konstytucja81Perel = [1925]
  getEmissionYears ZL5Konstytucja100Perel = [1925]
  getEmissionYears ZL2Zaglowiec = [1936]
  getEmissionYears ZL2Pilsudski = [1934, 1936]
  getEmissionYears ZL2ZniwiarkaLondyn = [1925]
  getEmissionYears ZL2ZniwiarkaFiladelfia = [1924]
  getEmissionYears ZL2ZniwiarkaParyz = [1924]
  getEmissionYears ZL2Zniwiarka = [1924, 1925]
  getEmissionYears ZL2Polonia = [1932, 1933, 1934]
  getEmissionYears ZL1Zniwiarka = [1924, 1925]
  getEmissionYears ZL1 = [1929]
  getEmissionYears GR50 = [1923]
  getEmissionYears GR20 = [1923]
  getEmissionYears GR10 = [1923]
  getEmissionYears GR5 = [1923, 1925, 1928, 1930, 1931, 1934, 1935, 1936, 1937, 1938, 1939]
  getEmissionYears GR2 = [1923, 1925, 1927, 1928, 1930, 1931, 1932, 1933, 1934, 1935, 1936, 1937, 1938, 1939]
  getEmissionYears GR1 = [1923, 1925, 1927, 1928, 1930, 1931, 1932, 1933, 1934, 1935, 1936, 1937, 1938, 1939]

allCoinTypes :: [CoinType]
allCoinTypes = [minBound .. maxBound]

data CoinDef = CoinDef {coinType :: CoinType, year :: Year} deriving (Show, Eq)

instance Classifier CoinDef where
  classifierRule (CoinDef t y) = classifierRule t ++ classifierRule y

rp2CoinDefs :: [CoinDef]
rp2CoinDefs =
  concatMap (\ct -> CoinDef ct <$> getEmissionYears ct) allCoinTypes

matchesClassifier :: (Classifier a) => NormalizedWords -> a -> Bool
matchesClassifier input a = containsAll input mustContains && containsNone input mustNotContains
  where
    rules = classifierRule a
    getMustContain (MustContain x) = Just x
    getMustContain _ = Nothing
    getMustNotContain (MustNotContain x) = Just x
    getMustNotContain _ = Nothing
    mustContains = toNormalizedWords . Data.Text.unwords $ mapMaybe getMustContain rules
    mustNotContains = toNormalizedWords . Data.Text.unwords $ mapMaybe getMustNotContain rules

identifyCoinDef :: [CoinDef] -> NormalizedWords -> [CoinDef]
identifyCoinDef defs input = filter (matchesClassifier input) defs

data CoinFeature = Trial | Reversed | Fake | Struck deriving (Eq, Show, Enum, Bounded, TextShow)

instance Classifier CoinFeature where
  classifierRule Trial = [MustContain "PRÓBA"]
  classifierRule Reversed = [MustContain "ODWROTKA"]
  classifierRule Fake = [MustContain "FALSYFIKAT", MustContain "FALS"]
  classifierRule Struck = [MustContain "DESTRUKT"]

allCoinFeatures :: [CoinFeature]
allCoinFeatures = [minBound .. maxBound]

identifyCoinFeatures :: NormalizedWords -> [CoinFeature]
identifyCoinFeatures input = filter (matchesClassifier input) allCoinFeatures

data Coin = Coin {coinDef :: CoinDef, features :: [CoinFeature]} deriving (Eq, Show)

singleMay :: [a] -> Maybe a
singleMay [x] = Just x
singleMay _ = Nothing

identifyCoin :: [CoinDef] -> Text -> Maybe Coin
identifyCoin coinDefs input = do
  def <- singleMay matchingDefs
  let coinFeatures = identifyCoinFeatures normalizedInput

  pure $ Coin def coinFeatures
  where
    normalizedInput = toNormalizedWords input
    matchingDefs = identifyCoinDef coinDefs normalizedInput
