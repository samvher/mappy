{-|
Module      : Batch
Description : Code voor het inlezen van een batch van kaart beschrijvingen
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Batch ( getBatchIDs,
               getBatch,
               MapData (..)
             ) where

import Control.Monad.Reader

import qualified Data.ConfigFile.Parser as CFP
import Data.Either.Utils

import Data.Bifunctor ( Bifunctor, bimap, second )
import Data.List ( partition, sort, stripPrefix )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe, mapMaybe )

import System.Directory
import System.FilePath

import Geometry.Shapefile
import Graphics.Figgy

import Init
import Parse
import ReadPCs

type BatchID = String
type Conf = [(String, [(String, String)])]

-- | Configuratie inlezen
readBatchConf :: BatchID -> IO Conf
readBatchConf bID = do path <- getBatchCfgPath bID
                       forceEither <$> CFP.parse_file path

-- | Context om batch eigenschappen uit te lezen (Reader monad)
newtype BatchEnv a =
  BatchEnv { unBatchEnv :: ReaderT (BatchID, Conf) IO a }
    deriving ( Applicative,
               Functor,
               Monad,
               MonadReader (BatchID, Conf),
               MonadIO )

batchID :: (BatchID, Conf) -> BatchID
batchID = fst

conf :: (BatchID, Conf) -> Conf
conf = snd

-- | Computatie uitvoering binnen context behorende bij gekozen BatchID
runBatchEnv :: BatchEnv a -> BatchID -> IO a
runBatchEnv m b = do bConf <- readBatchConf b
                     runReaderT (unBatchEnv m) (b, bConf)

-- | Map (soort dictionary) van postcode naar polygoon + bounding box
type PolyMap = M.Map String (Polygon, BBox)

-- | Map van postcode naar kleur
type DataMap = M.Map String RGB1

-- | Gebruiken we om bestandsextensie te strippen
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix xs ys = reverse <$> stripPrefix (reverse xs) (reverse ys)

-- | BatchIDs afleiden uit bestandsnamen in de gekozen b atch map
getBatchIDs :: IO [BatchID]
getBatchIDs = do
  dir   <- batchesDir
  files <- getDirectoryContents dir
  return . sort $ mapMaybe (stripSuffix ".cfg") files

-- | Pad van .cfg bestand behorende bij gegeven BatchID
getBatchCfgPath :: String -> IO FilePath
getBatchCfgPath bID = do
  dir     <- batchesDir
  return $ dir </> bID ++ ".cfg"

-- | Configuratie waarde uitlezen
getConfVal :: String -> String -> BatchEnv String
getConfVal section field = getField . getSection <$> asks conf
    where getSection = fromMaybe (error sectionError) . lookup section
          sectionError = "Config section not found: " ++ section
          getField = fromMaybe (error fieldError) . lookup field
          fieldError = "Config field not found: " ++ section ++ "-" ++ field

-- | Bounding box van de EPS bepalen (formaat van het plaatje)
getFrameBB :: BatchEnv BBox
getFrameBB = BBox <$> pure 0
                  <*> pure 0
                  <*> (read <$> getConfVal "outformat" "dim_x")
                  <*> (read <$> getConfVal "outformat" "dim_y")

-- | Combineert een color map met data (pc, waarde) tot een legenda en
--   gekleurde postcodes
type ScaleFun =
  ColorMap -> [(String, Double)] -> (LegendDesc, [(String, RGB1)])

-- | ScaleFun kiezen adhv configuratie
getScaleFun :: BatchEnv ScaleFun
getScaleFun = do
  mode <-        getConfVal "scale" "mode"
  n    <- read <$> getConfVal "scale" "bins"
  case mode of
    -- automatische binnen
    "auto"      -> return $ autoIntelligent n
    -- symmetrisch om 0 binnen
    "autoresp0" -> return $ autoResp0 n
    -- eigen minimum/maximum waarde van legendabereik
    "custom"    -> do lower <- read <$> getConfVal "scale" "min"
                      upper <- read <$> getConfVal "scale" "max"
                      return $ customScale lower upper n
    m           -> error $ "Unrecognized scale mode: " ++ show m

-- | Functie om polygonen mee te tekenen (met of zonder stroke)
getPolyFun :: MonadPS m => BatchEnv (RGB1 -> Polygon -> m ())
getPolyFun = do
  drawContours <- getConfVal "display" "draw_contours"
  case drawContours of
    "on"  -> return $ showPolyS 1.0 -- met stroke
    "off" -> return $ showPoly -- zonder stroke
    dc    -> error $ "Unrecognized draw_contours parameter: " ++ show dc

-- | Postcode-polygonen data inlezen (GIS data)
getPolyMap :: BatchEnv PolyMap
getPolyMap = do
  shpdata <- liftIO . readShpWithDbf =<< getConfVal "gisdata" "shpfile"
  return $ pcPolyMap shpdata

-- | Colormap bepalen op basis van configuratie
getColorMap :: BatchEnv ColorMap
getColorMap = do
  cmName <- getConfVal "scale" "colormap"
  return $ case cmName of
        "ice"    -> iceCM
        "i2i"    -> i2iCM
        "i3i"    -> i3iCM
        "parula" -> parulaCM
        "jet"    -> jetCM
        "hsv"    -> hsvCM
        "hot"    -> hotCM
        "cool"   -> coolCM
        "spring" -> springCM
        "summer" -> summerCM
        "autumn" -> autumnCM
        "winter" -> winterCM
        "gray"   -> grayCM
        "bone"   -> boneCM
        "copper" -> copperCM
        "pink"   -> pinkCM
        cm       -> error $ "Unrecognized colormap identifier: " ++ show cm

-- | Kleur voor ontbrekende datapunten
getColorMissing :: BatchEnv RGB1
getColorMissing = mkRGB1 . read <$> getConfVal "display" "fill_missing"

-- | bimap met 2x zelfde type in de Bifunctor
both :: Bifunctor p => (a -> b) -> p a a -> p b b
both f = bimap f f

-- | Datatype om alle informatie voor 1 kaartje te bevatten
data MapData =
  MapData {
    frameBB :: BBox,
    polyMap :: PolyMap,
    dataMap :: DataMap,
    flaggedPCs :: [String],
    pointLabels :: [Label],
    floatingLabels :: [Label],
    legendDesc :: LegendDesc,
    polyFun :: RGB1 -> Polygon -> EPSM,
    colorMissing :: RGB1
  }

-- | FileData (ingelezen data uit de database) omzetten naar MapData
--   (complete set informatie nodig voor het maken van een kaartje)
fdToMd :: PolyMap -> FileData -> BatchEnv MapData
fdToMd polymap fd = do
  adjustView            <- getConfVal "display" "adjust_view"
  let dropFlag (a, b, _) = (a, b)
      flag (_, _, f)     = f
      pc (a, _, _)       = a
      -- postcodes om op uit te snijden
      flaggedPCs'        = if adjustView == "on"
                           then (map pc . filter flag $ dataValue fd) ++
                                (map pc . filter flag $ dataColor fd)
                           else M.keys polymap -- alle pcs weergeven
      -- pointLabels worden rechts naast een stip weergegeven,
      -- floatingLabels gecentreerd op het coordinaat zonder stip
      (floatingLabels',
       pointLabels')     = both (map dropFlag) .  partition flag $ labels fd

  cMode                 <- getConfVal "data" "color_mode"
  (legend', dataColors) <- case cMode of
    -- kleuren worden in de data meegegeven
    "custom" -> return (legend fd, map dropFlag $ dataColor fd)
    -- kleuren worden met colormap bepaald
    "auto"   -> getScaleFun <*> getColorMap
                            <*> pure (map dropFlag $ dataValue fd)
    m        -> error $ "Unrecognized color mode: " ++ show m

  -- MapData opbouwen (dit is alles dat nodig is om een kaart te maken)
  MapData <$> getFrameBB -- grootte van het plaatje
          <*> pure polymap -- postcode/polygoon paren
          <*> pure (M.fromList dataColors) -- postcode/kleur paren
          <*> pure flaggedPCs' -- postcodes om op uit te snijden
          <*> pure pointLabels' -- labels voor weergave met punt
          <*> pure floatingLabels' -- labels voor weergave zonder punt
          <*> pure legend' -- legenda informatie
          <*> getPolyFun -- functie om polygoon te tekenen (stroke of niet)
          <*> getColorMissing -- kleur voor ontbrekende postcodes

-- | Type magic
seqTupList :: Applicative f => [(a, f b)] -> f [(a, b)]
seqTupList = sequenceA . map sequenceA

-- | Alle informatie voor 1 batch lezen
getBatch :: BatchID -> IO (FilePath, [(String, MapData)])
getBatch = runBatchEnv $ do
  outpath   <- getConfVal "outformat" "outdir"
  polymap   <- getPolyMap
  bID       <- asks batchID
  filedatas <- liftIO . fmap readRawBatch $ getRawBatch bID
  batchdata <- seqTupList $ map (second $ fdToMd polymap) filedatas
  return (outpath, batchdata)

