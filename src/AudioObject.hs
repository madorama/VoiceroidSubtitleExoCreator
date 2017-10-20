module AudioObject (createAudioObject, createAudioValues) where

import qualified Data.HashMap.Strict as HashMap
import Control.Lens
import qualified Exo.Exo as Exo
import Exo.Exo (Object, Values)
import Config

createAudioObject :: FilePath -> Object
createAudioObject path =
  Exo.emptyObject
    & Exo.audio .~ True
    & Exo.values .~ createAudioValues path

createAudioValues :: FilePath -> Values
createAudioValues path =
  [ [ ("_name", Exo.VString "音声ファイル")
    , ("再生位置", Exo.VFloat 0)
    , ("再生速度", Exo.VFloat 100)
    , ("ループ再生", Exo.VBool False)
    , ("動画ファイルと連携", Exo.VBool False)
    , ("file", Exo.VString path)
    ]
  , [ ("_name", Exo.VString "標準再生")
    , ("音量", Exo.VFloat 100)
    , ("左右", Exo.VFloat 0)
    ]
  ]
