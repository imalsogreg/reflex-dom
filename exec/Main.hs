{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid
import Data.Bool
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Lucid.Svg
import qualified Lucid.Svg as L
import qualified Lucid.Svg.Attributes as A
import qualified Lucid.Svg.Elements as E
import Reflex.Dom.Widget.Basic
import Reflex.Dom
import Reflex

svgNS = "http://www.w3.org/2000/svg"

svg :: (MonadWidget t m) => m a -> m a
svg child =
  elC
  (defElConfig {_elConfig_namespace= Just svgNS})
  "svg" child

-- Attribute-building helper for svg's
circCfg ats =
  defElConfig {_elConfig_namespace= Just svgNS ,_elConfig_attrs = ats}

main :: IO ()
main = mainWidget $ mdo

  tb <- toggle True =<<
        button "Click ME to hide svg. Click black circle to change size"
  tgl <- toggle False (_el_clicked (fst c)) -- Track circle-clicks

  svgAttrs <- forDyn tb (\b -> s "display" =: bool (s "none") "normal" b)
  --svgAttrs <- forDyn tgl (\b -> "display" =: "normal")

  -- Big circle's attributes. hrinkable
  ca  <- forDyn tgl (\b -> s "r"       =: bool (s "150") "100" b )

  -- Little circle's attributes. shrinkable
  ca' <- forDyn tgl (\b -> s "r"       =: bool (s "80") "70" b
                        <> s "fill"    =: "green"
                        <> s "cx"      =: "100"  <> s "cy"   =:     "100")

  c <- elC defElConfig{ _elConfig_attrs     = svgAttrs
                 , _elConfig_namespace = Just svgNS} "svg" $ do
    c <- elC' (circCfg ca) "circle"  (return ())
    _ <- elC' (circCfg ca') "circle" (return ())
    return c

  el "p" $ text "Helloooooooooooooooooooooooo"
  return ()

s :: String -> String
s = id

-- Nothing but copy-pasted dropshadow stuff from here down
shadowDefs :: Double -> Double -> Double -> T.Text -> T.Text -> Svg ()
shadowDefs x y blur color filtId = defs_ $ do
  (term "filter") fParams $ do
    feOffset_       [result_ "offOut", in_ "SourceAlpha"
                    , dx_ (f x), dy_ (f y)]
    feFlood_        [result_ "floodOut"
                    , flood_color_ color
                    , flood_opacity_ "1"]
    feGaussianBlur_ [result_ "blurOut", in_ "offOut"
                    , stdDeviation_ (f blur)]
    feComposite_    [result_ "shadowOut"
                    ,in_ "floodOut", in2_ "blurOut", operator_ "in"]
    feBlend_        [in_ "SourceGraphic", in2_ "shadowOut", mode_ "normal"]
--    feGaussianBlur_ [result_ "blurOut", in_ "floodOut"
--                    , stdDeviation_ (f blur)]
--    feBlend_        [in_ "SourceGraphic", in2_ "blurOut"
--                    , mode_ "normal"]
  where
    f :: (RealFrac a, Show a) => a -> T.Text
    f = T.pack . show . realToFrac
    fParams = [ id_ filtId , x_ "-0.5" , y_ "-0.5"
              , width_ "200%" , height_ "200%"]


elShadow :: Double -> Double -> Double -> String -> AttributeMap -> AttributeMap
elShadow x y blur color attrs =
  let sDefs = L.renderText $ shadowDefs x y blur (T.pack color) filtName
  in "test" =: (LT.unpack sDefs)
  -- with el [A.filter_ filtUrl]
  where filtName = mconcat [ "shadowFiltX", f x, "Y", f y
                           , "B", f blur, "C", (T.pack color)]
        filtUrl = T.concat ["url(#", filtName, ")"]
        f :: (RealFrac a, Show a) => a -> T.Text
        f = T.pack . show . realToFrac
