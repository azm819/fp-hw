module Runner where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Val = Int Int
         | Str String
         | Bool Bool
         | None
         deriving (Eq, Show)

type Scope = Map String Val
