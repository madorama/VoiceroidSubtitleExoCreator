{-# LANGUAGE TemplateHaskell #-}

module Config.Ast where

import Text.Parsec.Pos
import Control.Lens

data Loc =
  Loc
    { _fileName :: SourceName
    , _line :: Line
    , _column :: Column
    }
  deriving (Show, Eq)

makeLenses ''Loc

positionToLoc :: SourcePos -> Loc
positionToLoc pos = Loc (sourceName pos) (sourceLine pos) (sourceColumn pos)

data Ast
  = Config
      { _loc :: Loc
      , _name :: String
      , _extend :: Maybe String
      , _data :: [Value]
      }
  | Workspace
      { _loc :: Loc
      , path :: FilePath
      }
  deriving (Show, Eq)

data Value
  = X Double
  | Y Double
  | Size Integer
  | Font String
  | Color String
  | Shadow String
  | Position Horizontal Vertical
  deriving (Show, Eq)

data Vertical
  = VTop
  | VMiddle
  | VBottom
  deriving (Show, Eq)

data Horizontal
  = HLeft
  | HCenter
  | HRight
  deriving (Show, Eq)
