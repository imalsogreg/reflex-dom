module Main where

import Data.Default
import Reflex.Dom.Widget.Basic
import Reflex.Dom
import Reflex

svg :: (MonadWidget t m) => m a -> m a
svg child =
  elC (defElConfig {_elConfig_namespace= Just "http://www.w3.org/2000/svg"})
    "svg" child

circCfg = defElConfig {_elConfig_namespace= Just "http://www.w3.org/2000/svg"
                         ,_elConfig_attrs = "r" =: "150"}

circCfg' = defElConfig {_elConfig_namespace=Nothing
                         ,_elConfig_attrs = "r" =: "159"}
main :: IO ()
main = mainWidget $ do
  el "ol" $ do
    el "li" (text "Tast")
    el "li" (text "Test")
  s <- svg $ do
    c <- elC' circCfg "circle"  (return ())
    return c
  text "Test1"
  display =<< toggle False (_el_clicked (fst s))
  text "Laste"
