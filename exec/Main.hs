{-# LANGUAGE RecursiveDo #-}

module Main where

import Data.Monoid
import Data.Bool
import Data.Default
import Reflex.Dom.Widget.Basic
import Reflex.Dom
import Reflex
import Debug.Trace

svg :: (MonadWidget t m) => m a -> m a
svg child =
  elC (defElConfig {_elConfig_namespace= Just "http://www.w3.org/2000/svg"})
    "svg" child

circCfg ats = defElConfig {_elConfig_namespace= Just "http://www.w3.org/2000/svg"
                          ,_elConfig_attrs = ats}

circCfg' = defElConfig {_elConfig_namespace=Nothing
                         ,_elConfig_attrs = "r" =: "159" <> "fill" =: "green" <> "x" =: "50"}
main :: IO ()
main = mainWidget $ mdo
  trace "DEBUGG" (return ())
  el "ol" $ do
    el "li" (text "Tast")
    el "li" (text "Test")
  tgl <- toggle False (_el_clicked (fst s))
  ca <- forDyn tgl (\b -> "r" =: (bool "150" "100" b))
  ca' <- forDyn tgl (\b -> "r" =: (bool "80" "90" b) <> "fill" =: "green" <> "x" =: "50")
  s <- svg $ do
    c <- elC' (circCfg ca) "circle"  (return ())
    elC' (circCfg ca') "circle" (return ())
    return c
  --display =<< toggle False (Reflex.traceEvent "S event" $ _el_clicked (fst s))
  el "p" $ text "I've got a better idea. let's make a reaaly really long string to see if there's REALLY nothing being rendered!"
  trace "DEBUG" (return ())
