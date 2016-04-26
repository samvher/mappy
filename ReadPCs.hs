{-|
Module      : ReadPCs
Description : Code voor het inlezen van de GIS data
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>
-}

module ReadPCs where

import qualified Data.Map as M
import Data.Maybe ( fromJust, mapMaybe )

import Geometry.Shapefile

import Graphics.Figgy.PostScript

-- | Postcode van een record bepalen
pCode :: ShpRec -> String
pCode r = fromDbf $ shpRecLabel r
  where fromDbf (Just (DbfString s:_)) = s
        fromDbf _ = error $ "Geen postcode bij record gevonden:\n" ++ show r

-- | Ingelezen shapefile omzetten naar een postcode - polygon/bbox dict
pcPolyMap :: ShpData -> M.Map String (Polygon, BBox)
pcPolyMap shpdata = M.fromList . zip labels $ zip points bbs
  where polygon r = shpRecType r == ShpPolygon
        getPoints = recPolPoints . fromJust . shpRecContents
        recs      = shpRecs shpdata
        points    = map getPoints $ filter polygon recs
        bbs       = mapMaybe recbb $ filter polygon recs
        labels    = map pCode recs

-- | Bounding box van een shapefile inlezen
shpbb :: ShpData -> BBox
shpbb shpdata = BBox { xMin = shpXMin bb,
                       yMin = shpYMin bb,
                       xMax = shpXMax bb,
                       yMax = shpYMax bb }
  where bb = shpBB . shpHeader $ shpdata

-- | Bounding box van een record converteren naar het figgy BBox formaat
recbb2bb :: RecBBox -> BBox
recbb2bb rbb = BBox { xMin = recXMin rbb,
                      yMin = recYMin rbb,
                      xMax = recXMax rbb,
                      yMax = recYMax rbb }

-- | Bouding box van een record inlezen (als het geen null-record is)
recbb :: ShpRec -> Maybe BBox
recbb rec = getbb <$> shpRecContents rec
  where getbb r = case r of
          RecPoint p             -> BBox { xMin = fst p,
                                           yMin = snd p,
                                           xMax = fst p,
                                           yMax = snd p }
          RecPolygon rbb _ _ _ _ -> recbb2bb rbb
          _ -> error $ "recbb: Not implemented for record " ++ show r
