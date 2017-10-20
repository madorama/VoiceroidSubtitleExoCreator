module MakeExo where

import Control.Lens
import Exo.Exo
import Exo.Printer
import Config
import AudioObject
import TextObject

makeExo :: Config -> Exo
makeExo config =
  emptyExo
