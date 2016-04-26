{-|
Module      : Init
Description : Code voor verwerken van het hoofd-config-bestand init.cfg
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Init ( batchesDir,
              getRawBatch,
              RawBatch ( .. )
            ) where

import Control.Monad.Reader

import qualified Data.ConfigFile.Parser as CFP

import Data.Either.Utils

import Data.Maybe ( fromMaybe )

import Database.HDBC
import Database.HDBC.ODBC

import System.FilePath

import Queries

-- | Context om configuratie te lezen (Reader monad)
newtype InitEnv a = InitEnv { unInitEnv :: ReaderT Conf IO a }
                    deriving ( Applicative,
                               Functor,
                               Monad,
                               MonadReader Conf,
                               MonadIO )

-- | Context doorlopen
runInitEnv :: InitEnv a -> Conf -> IO a
runInitEnv m = runReaderT (unInitEnv m)

-- | In deze vorm worden alle configuratie opties opgeslagen
type Conf = [(String, [(String, String)])]

-- | Configuratie inlezen
readInit :: IO Conf
readInit = forceEither <$> CFP.parse_file "init.cfg"

-- | Setting uit configuratie lezen
getConfVal :: String -> String -> InitEnv String
getConfVal section field = getField . getSection <$> ask
  where getField = fromMaybe (error fieldError) . lookup field
        fieldError = "Config field not found: " ++ section ++ "-" ++ field
        getSection = fromMaybe (error sectionError) . lookup section
        sectionError = "Config section not found: " ++ section

-- | Map waarin de batch cfgs staan
batchesDir :: IO FilePath
batchesDir = readInit >>= (runInitEnv $ getConfVal "batches" "dir")

-- | Database verbinding
conn :: InitEnv Connection
conn = do
  odbcMode <- getConfVal "db-conn" "mode"
  case odbcMode of
    "auth" -> do driver <- getConfVal "db-conn" "driver"
                 server <- getConfVal "db-conn" "server"
                 db     <- getConfVal "db-conn" "database"
                 uid    <- getConfVal "db-conn" "uid"
                 pwd    <- getConfVal "db-conn" "pwd"
                 liftIO . connectODBC $ "DRIVER={"  ++ driver ++ "};" ++
                                    "SERVER="   ++ server ++ ";" ++
                                    "DATABASE=" ++ db     ++ ";" ++
                                    "UID="      ++ uid    ++ ";" ++
                                    "PWD="      ++ pwd    ++ ";" ++
                                    "MultipleActiveResultSets=True;"
    "dsn"  -> do dsn    <- getConfVal "db-conn" "name"
                 liftIO . connectODBC $ "DSN=" ++ dsn ++ ";" ++
                                    "MultipleActiveResultSets=True;"
    m      -> error $ "Unsupported DB connection mode: " ++ show m

-- | Hierin komt alle data uit de DB
data RawBatch =
  RawBatch {
    dataValueRaw :: [[String]],
    dataColorRaw :: [[String]],
    labelsRaw :: [[String]],
    legendRaw :: [[String]]
  } deriving ( Eq, Show )

-- | Data uit de DB lezen
getRawBatch :: String -> IO RawBatch
getRawBatch batchID = readInit >>= (runInitEnv $ do
  dbConn       <- conn
  -- Shorthand om query uit te voeren
  let getVal = fromMaybe "" . fromSql
      runQuery q = map (map getVal) <$> quickQuery' dbConn q []
  dataTable    <- getConfVal "tables"   "data"
  labelsTable  <- getConfVal "tables"   "labels"
  legendTable  <- getConfVal "tables"   "legenda"
  rapPeriode   <- getConfVal "run-info" "rapportage_periode"
  tijdSelectie <- getConfVal "run-info" "tijd_selectie_code"
  vzGroep      <- getConfVal "run-info" "verzekeraar_groep_code"
  let qDataValue = dataValueQuery dataTable
                                  batchID rapPeriode tijdSelectie vzGroep
      qDataColor = dataColorQuery dataTable legendTable
                                  batchID rapPeriode tijdSelectie vzGroep
      qLabels = labelsQuery dataTable labelsTable
                            batchID rapPeriode tijdSelectie vzGroep
      qLegend = legendQuery dataTable legendTable
                            batchID rapPeriode tijdSelectie vzGroep
  liftIO $ do
    dir <- batchesDir
    -- Write the queries to be run for reference
    writeFile (dir </> batchID ++ "-q1.sql") qDataValue
    writeFile (dir </> batchID ++ "-q2.sql") qDataColor
    writeFile (dir </> batchID ++ "-q3.sql") qLabels
    writeFile (dir </> batchID ++ "-q4.sql") qLegend
    -- Run the queries
    a <- runQuery qDataValue
    putStrLn $ "Retrieved values for batch " ++ show batchID
    b <- runQuery qDataColor
    putStrLn $ "Retrieved colors for batch " ++ show batchID
    c <- runQuery qLabels
    putStrLn $ "Retrieved labels for " ++ show batchID
    d <- runQuery qLegend
    putStrLn $ "Retrieved legend data for " ++ show batchID
    -- Put everything together
    return $ RawBatch a b c d
  )
