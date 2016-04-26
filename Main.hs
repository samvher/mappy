{-|
Module      : Main
Description : Hier komt alles samen
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>
-}

{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad.Reader
import Control.Concurrent.ParallelIO.Global ( parallel_ )
import Data.Bifunctor ( first )
import Data.List ( isSuffixOf )
import qualified Data.Map as M
import Data.Maybe ( fromJust, mapMaybe )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory ( createDirectoryIfMissing )
import System.FilePath

import Graphics.Figgy.Legend
import Graphics.Figgy.PostScript

import Batch

main :: IO ()
main = do batchIDs <- getBatchIDs -- batch directory doorzoeken voor batches
          mapM_ (\b -> getBatch b >>= runBatch) batchIDs

-- | Alle data voor een batch verwerken en als kaartjes wegschrijven
runBatch :: (FilePath, [(String, MapData)]) -> IO ()
runBatch (outpath, mapdatas) = do
  putStrLn $ "Outpath is: " ++ outpath
  -- Map + bovenliggende mappen aanmaken als ze niet bestaan
  createDirectoryIfMissing True outpath
  putStrLn $ "Number of files: " ++ (show $ length mapdatas)
  -- Equivalent without parallelization:
  -- mapM_ (writeMap outpath) mapdatas
  parallel_ $ map (writeMap outpath) mapdatas

-- | Gegeven out-dir en info voor een kaartje, kaartje genereren
writeMap :: FilePath -> (String, MapData) -> IO ()
writeMap outpath (fname, md) = do putStrLn $ "Working on " ++ fname
                                  TIO.writeFile filepath kaartje
    where filename = fname <.> ".eps"
          filepath = outpath ++ filename
          kaartje  = runReader render md -- EPS genereren uit MapData

-- | Postscript genereren uit MapData
render :: Reader MapData T.Text
render = do
  -- Postcodes om op bij te snijden
  cropConsider    <- asks flaggedPCs
  -- Functie om polygoon bij PC te vinden
  getPoly         <- flip M.lookup <$> asks polyMap
  -- De te passen ruimte (bereik van landcoordinaten waarin we tekenen)
  let spaceBB     =  mconcat $ mapMaybe (fmap snd . getPoly) cropConsider
  -- Door de dimensies van het plaatje is het zicht misschien groter
  visibleBB       <- flip fitBB spaceBB <$> asks frameBB
  -- Functie om te testen of een polygoon in beeld is
  let inView      =  intersect visibleBB
  -- Postcodes die ergens in beeld kunnen zijn en getekend moeten
  pcsToPlot       <- M.keys . M.filter (inView . snd) <$> asks polyMap
  -- Kleur bij een postcode vinden
  getData         <- flip M.lookup <$> asks dataMap
  -- Default kleur voor postcodes waarvan geen data is
  defColor        <- asks colorMissing
  -- Alle polygonen die getekend moeten
  let polys       =  zip (map (maybe defColor id . getData) pcsToPlot)
                         (map (fst . fromJust . getPoly) pcsToPlot)
  -- Functie om van landcoordinaten naar figuurcoordinaten te transformeren
  convertCoords   <- flip toFigureCoords spaceBB <$> asks frameBB
  -- Labels voor weergave met/zonder punt
  pointLabels'    <- map (first $ convertCoords) <$> asks pointLabels
  floatingLabels' <- map (first $ convertCoords) <$> asks floatingLabels
  -- Gebruiken we niet
  -- points'         <- map convertCoords <$> asks points
  -- Functie om polygoon te tekenen
  drawPoly        <- uncurry <$> asks polyFun
  -- Legenda data
  legend'         <- asks legendDesc
  -- Procedure om 1 EPS te genereren
  let mkEps bb    =  runEps bb $ do
                       fitting spaceBB $ mapM_ drawPoly polys
                       -- mapM_ (\p -> circle p 3 >> fill black) points'
                       mapM_ showLabel pointLabels'
                       mapM_ (\((x, y), l) -> centerText l (x, y))
                         floatingLabels'
                       drawDefaultLegend legend'
  -- Procedure uitvoeren
  mkEps <$> asks frameBB
