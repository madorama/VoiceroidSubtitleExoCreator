{-# LANGUAGE Strict #-}

module Main where

import System.IO
import System.Directory
import System.FilePath
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.ByteString as BS
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Control.Monad
import Control.Lens
import Codec.Audio.Wave
import Util
import Config
import Config.Parser
import qualified Exo.Exo as Exo
import Exo.Printer
import TextObject
import AudioObject

configFile :: String
configFile = "config.dat"

saveDir :: IO a -> IO a
saveDir io = do
  current <- getCurrentDirectory
  ret <- io
  setCurrentDirectory current
  return ret

main :: IO ()
main = do
  existConfig <- doesFileExist configFile
  if existConfig then do
    ast <- parseConfig configFile <$> BS.readFile configFile
    case ast of
      Left err -> print err
      Right ast -> do
        config <- makeConfig ast
        saveDir $ run config
        return ()
  else
    hPutStrLn stderr "config.datが存在しません。"

run :: Config -> IO ()
run config = do
  setCurrentDirectory (config ^. workspace)
  work <- getCurrentDirectory

  dirFiles <- listDirectory work
  let files = map dropExtension $ filter (isExtension "txt") dirFiles
  fileData <- forM files $ \file -> do
    text <- readFile (addExtension file "txt")
    return (file, text)

  case genPresetMap fileData of
    Left errs -> mapM_ print errs
    Right presetMap -> do
      newDir <- formatCurrentLocalTime "%Y%m%d%H%M%S"
      createDirectory newDir
      moveWaveAndTextFiles newDir files
      saveDir $ createExo newDir presetMap (config ^. configs)

  where
    formatCurrentLocalTime str = do
      lt <- utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
      pure $ formatTime defaultTimeLocale str lt
    moveWaveAndTextFiles path files =
      forM_ files $ \file -> do
        let wavFile = addExtension file "wav"
        let textFile = addExtension file "txt"
        renameFile wavFile (path </> wavFile)
        renameFile textFile (path </> textFile)

type PresetMap = Map String [(FilePath, String)]

createExo :: FilePath -> PresetMap -> Configs -> IO ()
createExo path pm confs = do
  setCurrentDirectory path
  current <- getCurrentDirectory
  exos <- concat <$> mapM (uncurry $ makeExos current) (Map.toList pm)
  mapM_ (\(file, exo) -> writeFile (addExtension file "exo") $ printExo exo) exos
  where
    makeExos path preset files = forM files (makeExo path preset)
    makeExo path preset (file, value) = do
      let wavPath = path </> addExtension file "wav"
      wave <- readWaveFile wavPath
      let duration = floor (60 * waveDuration wave)
      let configData = Map.findWithDefault emptyConfigData preset confs
      let textObj =
            createTextObject value configData
              & Exo.end .~ duration
              & Exo.group .~ 1
      let audioObj =
            createAudioObject wavPath
              & Exo.end .~ duration
              & Exo.layer .~ 2
              & Exo.group .~ 1
      let object =
            Exo.emptyExo
              & Exo.length .~ duration
              & Exo.objects .~ [ textObj, audioObj ]
      return (file, object)

genPresetMap :: [(FilePath, String)] -> Either [ParseError] PresetMap
genPresetMap = aux [] Map.empty
  where
    aux [] m [] = Right m
    aux errs m [] = Left errs
    aux errs m ((path, text):xs) =
      case parse parseText path text of
        Left err -> aux (err : errs) m xs
        Right (presetName, text) ->
          let list = (path, text) : fromMaybe [] (Map.lookup presetName m) in
          aux errs (Map.insert presetName list m) xs

parseText :: Parser (String, String)
parseText =
  (,) <$> manyTill anyChar (try $ string "＞") <*> manyTill anyChar eof
