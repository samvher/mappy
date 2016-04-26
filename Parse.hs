{-|
Module      : Parse
Description : Code voor het verwerken van de data uit de DB
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>
-}

module Parse where

import Data.Bifunctor ( second )
import Data.Function ( on )
import Data.List ( groupBy )
import qualified Data.Map as M

import Graphics.Figgy.Colors
import Graphics.Figgy.PostScript

import GIS.NLCoords

import Init

-- | Type spreekt voor zich
collectHeads :: Eq a => [[a]] -> [(a, [[a]])]
collectHeads = map go . groupBy ((==) `on` head)
  where go xss = (head $ head xss, map tail xss)

-- | J van Ja!
readI2IBool :: String -> Bool
readI2IBool ('J':_) = True
readI2IBool  _      = False

-- | Postcode met kleur info. Flag = altijd in zicht houden
readPCodeRGB :: [String] -> (String, RGB1, Bool)
readPCodeRGB [pc, r, g, b, flag] =
  (pc, mkRGB1 (read r, read g, read b), readI2IBool flag)
readPCodeRGB vs = error $ "Failed to read PCodeRGB: " ++ show vs

-- | Postcode met een numerieke waarde. Flag = altijd in zicht houden
readPCodeVal :: [String] -> (String, Double, Bool)
readPCodeVal [pc, val, flag] = (pc, read $ fixval val, readI2IBool flag)
  where fixval v@('.':_) = '0' : v
        fixval ('-':'.':v) = '-' : '0' : '.' : v
        fixval v = v
readPCodeVal vs = error $ "Failed to read PCodeVal: " ++ show vs

-- | Legenda entry
readLegendVal :: [String] -> (String, RGB1)
readLegendVal [label, r, g, b] = (label, mkRGB1 (read r, read g, read b))
readLegendVal vs = error $ "Failed to read LegendVal: " ++ show vs

-- | Label. Flag geeft aan of hij met punt weergegeven moet
readLabel :: [String] -> (Point, String, Bool)
readLabel [lat, lon, label, flag] =
  (unRD . wgs2rd $ WGS (read lat, read lon), label, readI2IBool flag)
readLabel vs = error $ "Failed to read Label: " ++ show vs

-- | Verwerkte data uit de database voor 1 bestand (1 kaartje)
data FileData =
  FileData {
    dataValue :: [(String, Double, Bool)],
    dataColor :: [(String, RGB1, Bool)],
    labels    :: [(Point, String, Bool)],
    legend    :: [(String, RGB1)]
  } deriving ( Eq, Show )

-- | We willen makkelijk data aan elkaar kunnen plakken
instance Monoid FileData where
  mempty = FileData [] [] [] []
  FileData vs1 cs1 ls1 legs1 `mappend` FileData vs2 cs2 ls2 legs2 =
    FileData (vs1 ++ vs2) (cs1 ++ cs2) (ls1 ++ ls2) (legs1 ++ legs2)

-- | Data uit een RawBatch (zie Init.hs) inlezen
readRawBatch :: RawBatch -> [(String, FileData)]
readRawBatch raw = M.toList $ M.unionsWith mappend [fileValMap,
                                                    fileColors,
                                                    fileLabels,
                                                    fileLegends]
  where readToMapWith f = M.fromList . map (second f) . collectHeads
        fdv vs          = mempty { dataValue = map readPCodeVal vs}
        fileValMap      = readToMapWith fdv $ dataValueRaw raw
        fdc cs          = mempty { dataColor = map readPCodeRGB cs}
        fileColors      = readToMapWith fdc $ dataColorRaw raw
        fdl ls          = mempty { labels = map readLabel ls}
        fileLabels      = readToMapWith fdl $ labelsRaw raw
        fdleg legs      = mempty { legend = map readLegendVal legs}
        fileLegends     = readToMapWith fdleg $ legendRaw raw

{-
-- Unused
readPoint :: [String] -> Point
readPoint [lat, lon] = unRD . wgs2rd $ WGS (read lat, read lon)
-}
