{-# LANGUAGE Strict #-}
{-# LANGUAGE UnicodeSyntax #-}

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
  ----------------------------------------
  -- Workspace内の全てのtxtファイルを読み、
  -- プリセット名をkey、[(ファイル名,文章)]をvalueとしたMapを作成
  ----------------------------------------
  setCurrentDirectory (config ^. workspace)
  work <- getCurrentDirectory
  files <- filter (isExtension "txt") <$> listDirectory "."
  -- ファイル名から拡張子を取り除く
  files <- pure $ map dropExtension files
  presetMap <- genPresetMap files
  ----------------------------------------
  -- 現在の時間を基にフォルダを作成
  ----------------------------------------
  lt <- utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
  let newDir = formatTime defaultTimeLocale "%Y%m%d%H%M%S" lt
  createDirectory newDir
  ----------------------------------------
  -- 全ての音声ファイル(と一応txtも)を先程作ったフォルダに移動
  ----------------------------------------
  forM_ files
    (\file -> do
      let wavFile = addExtension file "wav"
      copyFile wavFile (newDir </> wavFile)
      removeFile wavFile
      removeFile (addExtension file "txt")
    )
  ----------------------------------------
  -- Exo生成
  ----------------------------------------
  saveDir $ createExo newDir presetMap (config ^. configs)
  return ()

type PresetMap = Map String [(FilePath, String)]

createExo :: FilePath -> PresetMap -> Configs -> IO ()
createExo path pm confs = do
  setCurrentDirectory path
  current <- getCurrentDirectory
  exos <- concat <$> forM (Map.toList pm) (uncurry (makeExos current))
  forM_ exos (\(file, exo) -> writeFile (addExtension file "exo") $ printExo exo)
  return ()
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

genPresetMap :: [FilePath] -> IO PresetMap
genPresetMap = aux Map.empty
  where
    aux m [] = return m
    aux m (path:xs) = do
      source <- readFile (path ++ ".txt")
      case parse parseText path source of
        Left err -> do
          hPrint stderr err
          aux m xs
        Right (presetName, text) -> do
          let list = (path, text) : fromMaybe [] (Map.lookup presetName m)
          aux (Map.insert presetName list m) xs

parseText :: Parser (String, String)
parseText =
  (,) <$> manyTill anyChar (try $ string "＞") <*> manyTill anyChar eof
