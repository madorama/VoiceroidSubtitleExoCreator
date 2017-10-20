{-# LANGUAGE Strict #-}

module TextObject (createTextObject, createTextValues) where

import Data.Word
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HashMap
import Text.Printf
import Control.Lens
import qualified Exo.Exo as Exo
import Exo.Exo (Object, Values)
import Config

createTextObject :: String -> ConfigData -> Object
createTextObject text cd =
  Exo.emptyObject
    & Exo.values .~ createTextValues text cd

createTextValues :: String -> ConfigData -> Values
createTextValues text cd =
  [ [ ("_name", Exo.VString "テキスト")
    , ("サイズ", Exo.VInt $ cd ^. size)
    , ("表示速度", Exo.VFloat 0.0)
    , ("文字毎に個別オブジェクト", Exo.VBool False)
    , ("移動座標上に表示する", Exo.VBool False)
    , ("自動スクロール", Exo.VBool False)
    , ("B", Exo.VBool False)
    , ("I", Exo.VBool False)
    , ("type", Exo.VInt 3)
    , ("autoadjust", Exo.VBool False)
    , ("soft", Exo.VBool True)
    , ("monospace", Exo.VBool False)
    , ("align", Exo.VInt $ toInteger $ cd ^. align)
    , ("spacing_x", Exo.VInt 0)
    , ("spacing_y", Exo.VInt 0)
    , ("precision", Exo.VBool True)
    , ("color", Exo.VString $ cd ^. color)
    , ("color2", Exo.VString $ cd ^. shadow)
    , ("font", Exo.VString $ cd ^. font)
    , ("text", Exo.VString $ makeText text)
    ]
  , [ ("_name", Exo.VString "標準描画")
    , ("X", Exo.VFloat $ cd ^. x)
    , ("Y", Exo.VFloat $ cd ^. y)
    , ("Z", Exo.VFloat 0)
    , ("拡大率", Exo.VFloat 100)
    , ("透明度", Exo.VFloat 0)
    , ("回転", Exo.VFloat 0)
    , ("blend", Exo.VInt 0)
    ]
  ]

makeText :: String -> String
makeText text =
  let words = BS.unpack $ encodeUtf16LE $ T.pack text in
  printWords words ++ replicate (4096 - length words * 2) '0'

printWords :: [Word8] -> String
printWords [] = ""
printWords [x] = printf "%02x" x
printWords (x:xs) = printf "%02x" x ++ printWords xs
