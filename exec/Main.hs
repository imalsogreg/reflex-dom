module Main where

import Data.Default
import Reflex.Dom.Widget.Basic
import Reflex.Dom

svg :: (MonadWidget t m) => m a -> m a
svg child =
  elC (defElConfig {_elConfig_namespace= Just "http://www.w3.org/2000/svg"})
    "svg" child

circCfg = defElConfig {_elConfig_namespace= Just "http://www.w3.org/2000/svg"
                         ,_elConfig_attrs = "r" =: "100"}

circCfg' = defElConfig {_elConfig_namespace=Nothing
                         ,_elConfig_attrs = "r" =: "100"}
main :: IO ()
main = mainWidget $ do
  el "ol" $ do
    el "li" (text "Test")
    el "li" (text "Test")
  svg (elC circCfg "circle"  (return ()))
