{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Config where

import System.IO
import Text.Printf
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.List as List
import Control.Lens
import qualified Config.Ast as Ast

data ConfigData =
  ConfigData
    { _x :: Double
    , _y :: Double
    , _size :: Integer
    , _font :: String
    , _color :: String
    , _shadow :: String
    , _align :: Int
    }
    deriving (Show)

makeLenses ''ConfigData

emptyConfigData :: ConfigData
emptyConfigData =
  ConfigData 0 0 24 "" "FFFFFF" "000000" 0

type Configs = Map String ConfigData

emptyConfigs :: Configs
emptyConfigs = Map.empty

data Config =
  Config
    { _configWorkspace :: String
    , _configConfigs :: Configs
    }
    deriving (Show)

makeFields ''Config

printError :: HPrintfType r => Ast.Loc -> String -> r
printError loc s = hPrintf stderr ("%d:%d : " ++ s ++ "\n") (loc ^. Ast.line) (loc ^. Ast.column)

makeConfig :: [Ast.Ast] -> IO Config
makeConfig ast = do
  let newAst = filter (\ast -> case ast of Ast.Config{} -> True; _ -> False) ast
  Config path <$> makeConfigs emptyConfigs newAst
  where
    path =
      case List.find (\Ast.Workspace{} -> True) ast of
        Just (Ast.Workspace _ path) -> path
        Nothing -> "./workspace"

    makeConfigs :: Configs -> [Ast.Ast] -> IO Configs
    makeConfigs cs [] = return cs
    makeConfigs cs [x] = makeConfigData cs x
    makeConfigs cs (x:xs) = do
      cs <- makeConfigData cs x
      makeConfigs cs xs

    makeConfigData :: Configs -> Ast.Ast -> IO Configs
    makeConfigData cs (Ast.Config loc name extend values)
      | Map.member name cs = do
          printError loc "設定 \"%s\" は既に存在します。" name
          return cs
      | (Just extend) <- extend, Map.member extend cs = do
          let baseConfigData = Map.findWithDefault emptyConfigData extend cs
          let configData = List.foldl' makeValue baseConfigData values
          return $ Map.insert name configData cs
      | (Just extend) <- extend = do
          printError loc "設定 \"%s\" の継承元設定 \"%s\" は存在しません。" name extend
          return cs
      | otherwise = do
          let configData = List.foldl' makeValue emptyConfigData values
          return $ Map.insert name configData cs

    makeValue :: ConfigData -> Ast.Value -> ConfigData
    makeValue cd (Ast.X n) = cd & x .~ n
    makeValue cd (Ast.Y n) = cd & y .~ n
    makeValue cd (Ast.Size n) = cd & size .~ n
    makeValue cd (Ast.Font s) = cd & font .~ s
    makeValue cd (Ast.Color s) = cd & color .~ s
    makeValue cd (Ast.Shadow s) = cd & shadow .~ s
    makeValue cd (Ast.Position h v) = cd & align .~ makeAlign h v

    makeAlign :: Ast.Horizontal -> Ast.Vertical -> Int
    makeAlign h v = horizontal + 3 * vertical
      where
        horizontal =
          case h of
            Ast.HLeft -> 0
            Ast.HCenter -> 1
            Ast.HRight -> 2
        vertical =
          case v of
            Ast.VTop -> 0
            Ast.VMiddle -> 1
            Ast.VBottom -> 2
