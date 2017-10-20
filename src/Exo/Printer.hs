{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}

module Exo.Printer where

import qualified Data.HashMap.Strict as HashMap
import Text.Printf
import Control.Lens
import qualified Exo.Exo as Exo
import Util

class PrintValue a where
  printVal :: String -> a -> String

instance PrintValue Int where printVal = printf "%s=%d\n"
instance PrintValue Integer where printVal = printf "%s=%d\n"
instance PrintValue Float where printVal = printf "%s=%f\n"
instance PrintValue Double where printVal = printf "%s=%f\n"
instance PrintValue String where printVal = printf "%s=%s\n"
instance PrintValue Bool where
  printVal name True = printf "%s=1\n" name
  printVal name False = printf "%s=0\n" name
instance PrintValue Exo.Value where
  printVal name (Exo.VInt n) = printVal name n
  printVal name (Exo.VString s) = printVal name s
  printVal name (Exo.VFloat f) = printVal name f
  printVal name (Exo.VBool bool) = printVal name bool

printIf :: PrintValue a => Bool -> String -> a -> String
printIf cond name a = if cond then printVal name a else ""

printExo :: Exo.Exo -> String
printExo exo =
  printf "[exedit]\n"
  ++ printVal "width" (exo ^. Exo.width)
  ++ printVal "height" (exo ^. Exo.height)
  ++ printVal "rate" (exo ^. Exo.rate)
  ++ printVal "scale" (exo ^. Exo.scale)
  ++ printVal "length" (exo ^. Exo.length)
  ++ printVal "audio_rate" (exo ^. Exo.audioRate)
  ++ printVal "audio_ch" (exo ^. Exo.audioCh)
  ++ concatMapi printObject (exo ^. Exo.objects)

printObject :: Int -> Exo.Object -> String
printObject n obj =
  printf "[%d]\n" n
  ++ printVal "start" (obj ^. Exo.start)
  ++ printVal "end" (obj ^. Exo.end)
  ++ printVal "layer" (obj ^. Exo.layer)
  ++ printIf (obj ^. Exo.group >= 1) "group" (obj ^. Exo.group)
  ++ printVal "overlay" (obj ^. Exo.overlay)
  ++ printVal "camera" (obj ^. Exo.camera)
  ++ printIf (obj ^. Exo.audio) "audio" (obj ^. Exo.audio)
  ++ concatMapi (printValues n) (obj ^. Exo.values)

printValues :: Int -> Int -> [(String, Exo.Value)] -> String
printValues n m values =
  printf "[%d.%d]\n" n m
  ++ concatMap (uncurry printVal) values
