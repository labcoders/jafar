module Main where

import Data.Time.Clock
import Control.Monad.State
import Programs (emaProg)
import Interpreter

main = print emaProg
