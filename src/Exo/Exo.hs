{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Exo.Exo where

import Data.HashMap.Strict
import Control.Lens

data Value
  = VInt Integer
  | VString String
  | VFloat Double
  | VBool Bool
  deriving (Show)

type Values = [[(String, Value)]]

data Object =
  Object
    { _objectStart :: Int
    , _objectEnd :: Int
    , _objectLayer :: Int
    , _objectGroup :: Int
    , _objectOverlay :: Int
    , _objectCamera :: Int
    , _objectAudio :: Bool
    , _objectValues :: Values
    }
    deriving (Show)

makeFields ''Object

emptyObject :: Object
emptyObject =
  Object 1 100 1 0 1 0 False []

data Exo =
  Exo
    { _exoWidth :: Int
    , _exoHeight :: Int
    , _exoRate :: Float
    , _exoScale :: Float
    , _exoLength :: Int
    , _exoAudioRate :: Int
    , _exoAudioCh :: Int
    , _exoObjects :: [Object]
    }
    deriving (Show)

makeFields ''Exo

emptyExo :: Exo
emptyExo =
  Exo 1280 720 60 1 100 44100 2 []
