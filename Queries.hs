{-|
Module      : Queries
Description : Code voor het genereren van de queries
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>
-}

module Queries where

import Data.List ( intersperse )

-- | Newlines toevoegen tussen de regels
buildQuery :: [String] -> String
buildQuery = concat . intersperse "\n"

-- | WITH statement om context tabel aan het begin toe te voegen
withBestanden :: String -> String
withBestanden dataTabel = buildQuery
  ["WITH bestanden AS",
   "  ( SELECT",
   "      batch_id,",
   "      CAST(agb_code AS varchar)",
   "        + CASE WHEN specialisme_code IS NULL",
   "                    THEN ''",
   "                    ELSE '_' + CAST(specialisme_code AS varchar)",
   "          END",
   "        AS bestandsnaam,",
   "      agb_code,",
   "      specialisme_code,",
   "      rapportage_periode,",
   "      tijd_selectie_code,",
   "      verzekeraar_groep_code",
   "    FROM",
   "      " ++ dataTabel,
   "    GROUP BY",
   "      batch_id,",
   "      agb_code,",
   "      specialisme_code,",
   "      rapportage_periode,",
   "      tijd_selectie_code,",
   "      verzekeraar_groep_code)"]

-- | SELECT statement voor labels query
selectLabels :: String
selectLabels = buildQuery
  ["SELECT",
   "  bestanden.bestandsnaam,",
   "  tbl.Lat,",
   "  tbl.Long,",
   "  tbl.label,",
   "  tbl.punt_weglaten_ind"]

-- | SELECT statement voor legenda query
selectLegend :: String
selectLegend = buildQuery
  ["SELECT",
   "  bestanden.bestandsnaam,",
   "  tbl.legenda_label,",
   "  CAST(tbl.r AS int),",
   "  CAST(tbl.g AS int),",
   "  CAST(tbl.b AS int)"]

-- | Join op de bestanden data
joinBestanden :: String -> String
joinBestanden tabel = buildQuery
  ["FROM",
   "  bestanden",
   "  INNER JOIN " ++ tabel ++ " AS tbl",
   "  ON",
   "    bestanden.batch_id = tbl.batch_id",
   "    AND (   bestanden.agb_code = tbl.agb_code",
   "         OR tbl.agb_code IS NULL)",
   "    AND (   bestanden.specialisme_code = tbl.specialisme_code",
   "         OR tbl.specialisme_code IS NULL)",
   "    AND (   bestanden.rapportage_periode = tbl.rapportage_periode",
   "         OR tbl.rapportage_periode IS NULL)",
   "    AND (   bestanden.tijd_selectie_code = tbl.tijd_selectie_code",
   "         OR tbl.tijd_selectie_code IS NULL)",
   "    AND (   bestanden.verzekeraar_groep_code = \
                  \ tbl.verzekeraar_groep_code",
   "         OR tbl.verzekeraar_groep_code IS NULL)"]

-- | WHERE statement voor specificeren van rapportagerun info
whereRapRun :: String -> String -> String -> String -> String -> String
whereRapRun tblAlias batchID rapPeriode tijdSelectie vzGroep = buildQuery
  ["WHERE",
   "  " ++ tblAlias ++ ".batch_id = " ++ batchID ++ " AND",
   "  " ++ tblAlias ++ ".rapportage_periode = " ++ rapPeriode ++ " AND",
   "  " ++ tblAlias ++ ".tijd_selectie_code = '" ++ tijdSelectie ++ "' AND",
   "  " ++ tblAlias ++ ".verzekeraar_groep_code = '" ++ vzGroep ++ "'"]

-- | ORDER BY statement maken
orderBy :: [String] -> String
orderBy cols = "ORDER BY " ++ (concat $ intersperse ", " cols)

-- | Complete labels query
labelsQuery :: String -> String -> String -> String
            -> String -> String -> String
labelsQuery dataTabel labelTabel
            batchID rapPeriode tijdSelectie vzGroep = buildQuery
  [withBestanden dataTabel,
   selectLabels,
   joinBestanden labelTabel,
   whereRapRun "bestanden" batchID rapPeriode tijdSelectie vzGroep,
   orderBy ["bestandsnaam", "label"]]

-- | Complete legenda query
legendQuery :: String -> String -> String -> String
            -> String -> String -> String
legendQuery dataTabel legendaTabel
            batchID rapPeriode tijdSelectie vzGroep = buildQuery
  [withBestanden dataTabel,
   selectLegend,
   joinBestanden legendaTabel,
   whereRapRun "bestanden" batchID rapPeriode tijdSelectie vzGroep,
   orderBy ["bestandsnaam", "legenda_volgorde"]]

-- | Complete query voor postcodes met custom kleuren
dataColorQuery :: String -> String -> String -> String
               -> String -> String -> String
dataColorQuery dataTabel legendaTabel
               batchID rapPeriode tijdSelectie vzGroep = buildQuery
  [selectDataColor,
   joinDataColor dataTabel legendaTabel,
   whereRapRun "kaartdata" batchID rapPeriode tijdSelectie vzGroep,
   orderBy ["bestandsnaam", "postcode_cijfers"]]

-- | Complete query voor postcodes met numerieke waarde
dataValueQuery :: String -> String -> String
               -> String -> String -> String
dataValueQuery dataTabel
               batchID rapPeriode tijdSelectie vzGroep = buildQuery
  [selectDataValue,
   "FROM " ++ dataTabel ++ " AS kaartdata",
   whereRapRun "kaartdata" batchID rapPeriode tijdSelectie vzGroep,
   orderBy ["bestandsnaam", "postcode_cijfers"]]

-- | SELECT statement voor postcodes met custom kleuren
selectDataColor :: String
selectDataColor = buildQuery
  ["SELECT",
   "  CAST(kaartdata.agb_code AS varchar)",
   "    + CASE WHEN kaartdata.specialisme_code IS NULL",
   "                THEN ''",
   "                ELSE '_' + CAST(kaartdata.specialisme_code AS varchar)",
   "      END",
   "    AS bestandsnaam,",
   "  postcode_cijfers,",
   "  CAST(legenda.r AS int),",
   "  CAST(legenda.g AS int),",
   "  CAST(legenda.b AS int),",
   "  postcode_weergeven_ind"]

-- | SELECT statement voor postcodes met numerieke waarde
selectDataValue :: String
selectDataValue = buildQuery
  ["SELECT",
   "  CAST(kaartdata.agb_code AS varchar)",
   "    + CASE WHEN kaartdata.specialisme_code IS NULL",
   "                THEN ''",
   "                ELSE '_' + CAST(kaartdata.specialisme_code AS varchar)",
   "      END",
   "    AS bestandsnaam,",
   "  postcode_cijfers,",
   "  waarde,",
   "  postcode_weergeven_ind"]

-- | JOIN statement voor de query met custom kleuren (kleuren worden uit
--   de legenda gehaald)
joinDataColor :: String -> String -> String
joinDataColor dataTabel legendaTabel = buildQuery
  ["FROM",
   "  " ++ dataTabel ++ " AS kaartdata",
   "  INNER JOIN " ++ legendaTabel ++ " AS legenda",
   "    ON      kaartdata.waarde = legenda.legenda_waarde",
   "       AND  kaartdata.batch_id = legenda.batch_id",
   "       AND (   kaartdata.agb_code = legenda.agb_code",
   "            OR legenda.agb_code IS NULL)",
   "       AND (   kaartdata.specialisme_code = legenda.specialisme_code",
   "            OR legenda.specialisme_code IS NULL)",
   "       AND (   kaartdata.rapportage_periode = \
                     \ legenda.rapportage_periode",
   "            OR legenda.rapportage_periode IS NULL)",
   "       AND (   kaartdata.tijd_selectie_code = \
                     \ legenda.tijd_selectie_code",
   "            OR legenda.tijd_selectie_code IS NULL)",
   "       AND (   kaartdata.verzekeraar_groep_code = \
                     \ legenda.verzekeraar_groep_code",
   "            OR legenda.verzekeraar_groep_code IS NULL)"]
