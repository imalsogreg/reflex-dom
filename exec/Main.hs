{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad
import Data.Monoid
import Data.Bool
import Data.Colour
import Data.Colour.SRGB
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Lucid.Svg
import qualified Lucid.Svg as L
import qualified Lucid.Svg.Attributes as A
import qualified Lucid.Svg.Elements as E
import Reflex.Dom.Widget.Basic
import Reflex.Dom
import Reflex
import Safe

svgNS = "http://www.w3.org/2000/svg"

svgTag :: (MonadWidget t m) => m a -> m a
svgTag child =
  elC
  (defElConfig {_elConfig_namespace= Just svgNS})
  "svg" child

--svg :: (MonadWidget t m, Attributes m attrs) => ElConfig attrs -> ElConfig attrs
svg :: ElConfig a -> ElConfig a
svg e = e {_elConfig_namespace = Just svgNS}

elAttrSvg :: (MonadWidget t m, Attributes m attrs) => String -> attrs -> m a -> m a
elAttrSvg string ats child = let cfg = defElConfig { _elConfig_namespace = Just svgNS
                                                   , _elConfig_attrs     = ats
                                                   }
                             in  elC cfg string child

-- Attribute-building helper for svg's
circCfg ats =
  defElConfig {_elConfig_namespace= Just svgNS ,_elConfig_attrs = ats}

{-
main' :: IO ()
main' = mainWidget $ mdo

  tb <- toggle True =<< button "Click ME to hide svg. Click black circle to change size"
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

  el "p" $ text "Hello"
  return ()
-}

s :: String -> String
s = id

-- No-- thing but copy-pasted dropshadow stuff from here down
-- shadowDefs :: Double -> Double -> Double -> T.Text -> T.Text -> Svg ()
-- shadowDefs x y blur color filtId = defs_ $ do
--   (term "filter") fParams $ do
--     feOffset_       [result_ "offOut", in_ "SourceAlpha"
--                     , dx_ (f x), dy_ (f y)]
--     feFlood_        [result_ "floodOut"
--                     , flood_color_ color
--                     , flood_opacity_ "1"]
--     feGaussianBlur_ [result_ "blurOut", in_ "offOut"
--                     , stdDeviation_ (f blur)]
--     feComposite_    [result_ "shadowOut"
--                     ,in_ "floodOut", in2_ "blurOut", operator_ "in"]
--     feBlend_        [in_ "SourceGraphic", in2_ "shadowOut", mode_ "normal"]
-- --    feGaussianBlur_ [result_ "blurOut", in_ "floodOut"
-- --                    , stdDeviation_ (f blur)]
-- --    feBlend_        [in_ "SourceGraphic", in2_ "blurOut"
-- --                    , mode_ "normal"]
--   where
--     f :: (RealFrac a, Show a) => a -> T.Text
--     f = T.pack . show . realToFrac
--     fParams = [ id_ filtId , x_ "-0.5" , y_ "-0.5"
--               , width_ "200%" , height_ "200%"]


-- elShadow :: Double -> Double -> Double -> String -> AttributeMap -> AttributeMap
-- elShadow x y blur color attrs =
--   let sDefs = L.renderText $ shadowDefs x y blur (T.pack color) filtName
--   in "test" =: (LT.unpack sDefs)
--   -- with el [A.filter_ filtUrl]
--   where filtName = mconcat [ "shadowFiltX", f x, "Y", f y
--                            , "B", f blur, "C", (T.pack color)]
--         filtUrl = T.concat ["url(#", filtName, ")"]
--         f :: (RealFrac a, Show a) => a -> T.Text
--         f = T.pack . show . realToFrac



main :: IO ()
main = mainWidget $ do
  let head0 = 8
      stem0 = 2.8
      numSheets0 = 2
      gap0 = 1
      sheet0 = 1
      r0 = 255
      g0 = 0
      b0 = 0
      label t child = el "div" $ el "label" $ do
        result <- child
        text t
        return result
      widthStyle = "style" =: "width:30em"
  head <- label "Head size" $ do
    textInput $ def & textInputConfig_initialValue .~ show head0
                    & textInputConfig_inputType .~ "range"
                    & attributes .~ constDyn (widthStyle <> "min" =: "2" <> "max" =: "20" <> "step" =: "0.01")
  stem <- label "Stem length" $ do
    textInput $ def & textInputConfig_initialValue .~ show stem0
                    & textInputConfig_inputType .~ "range"
                    & attributes .~ constDyn (widthStyle <> "min" =: "1" <> "max" =: "5" <> "step" =: "0.01")
  numSheets <- label "Number of sheets" $ do
    textInput $ def & textInputConfig_initialValue .~ show gap0
                    & textInputConfig_inputType .~ "range"
                    & attributes .~ constDyn (widthStyle <> "min" =: "0" <> "max" =: "6" <> "step" =: "1")
  gap <- label "Gap width" $ do
    textInput $ def & textInputConfig_initialValue .~ show gap0
                    & textInputConfig_inputType .~ "range"
                    & attributes .~ constDyn (widthStyle <> "min" =: "0" <> "max" =: "4" <> "step" =: "0.01")
  sheet <- label "Sheet width" $ do
    textInput $ def & textInputConfig_initialValue .~ show sheet0
                    & textInputConfig_inputType .~ "range"
                    & attributes .~ constDyn (widthStyle <> "min" =: "0" <> "max" =: "4" <> "step" =: "0.01")
  el "div" $ text "Color:"
  r <- label "Red" $ do
    textInput $ def & textInputConfig_initialValue .~ show r0
                    & textInputConfig_inputType .~ "range"
                    & attributes .~ constDyn (widthStyle <> "min" =: "0" <> "max" =: "255" <> "step" =: "1")
  g <- label "Green" $ do
    textInput $ def & textInputConfig_initialValue .~ show g0
                    & textInputConfig_inputType .~ "range"
                    & attributes .~ constDyn (widthStyle <> "min" =: "0" <> "max" =: "255" <> "step" =: "1")
  b <- label "Blue" $ do
    textInput $ def & textInputConfig_initialValue .~ show b0
                    & textInputConfig_inputType .~ "range"
                    & attributes .~ constDyn (widthStyle <> "min" =: "0" <> "max" =: "255" <> "step" =: "1")
  logoConfig <- $(qDyn [| LogoConfig
    { _logoConfig_head = fromMaybe head0 $ do
         n <- readMay $(unqDyn [| value head |])
         guard $ n > 1
         return n
    , _logoConfig_stem = fromMaybe stem0 $ readMay $(unqDyn [| value stem |])
    , _logoConfig_numSheets = fromMaybe numSheets0 $ readMay $(unqDyn [| value numSheets |])
    , _logoConfig_gap = fromMaybe gap0 $ readMay $(unqDyn [| value gap |])
    , _logoConfig_sheet = fromMaybe sheet0 $ readMay $(unqDyn [| value sheet |])
    , _logoConfig_color = sRGB24 (fromMaybe r0 $ readMay $(unqDyn [| value r |]))
                                 (fromMaybe g0 $ readMay $(unqDyn [| value g |]))
                                 (fromMaybe b0 $ readMay $(unqDyn [| value b |]))
    } |])
  logoWidget logoConfig
  display logoConfig
  return ()


data LogoConfig
   = LogoConfig { _logoConfig_head :: Double
                , _logoConfig_stem :: Double
                , _logoConfig_numSheets :: Int
                , _logoConfig_gap :: Double
                , _logoConfig_sheet :: Double
                , _logoConfig_color :: Colour Double
                }
   deriving (Show, Read, Eq)

-- combineDynWith3 :: (a -> b -> c -> d) -> Dynamic t m a -> Dynamic t m b -> Dynamic t m c -> m (Dynamic t m d)

logoWidget :: MonadWidget t m => Dynamic t LogoConfig -> m ()
logoWidget l = do

  n <- forDyn l (_logoConfig_head)
  -- m <- forDyn l (show . _logoConfig_stem)
  g <- forDyn l (_logoConfig_gap)
  -- s <- forDyn l (show . _logoConfig_sheet)
  c <- forDyn l (_logoConfig_color)

  let circAttrs' :: Int -> Int -> Double -> Colour Double -> Map.Map String String
      circAttrs' x y r col  = s "cx" =: show x <> s "cy" =: show y <> s "fill" =: sRGB24show col <> s "r" =: show r

      --circAtts dynX dynY dynC = mapDyn circAttrs =<< combineDyn (flip (,)) dynC =<< combineDyn (,) dynX dynY

  --dynIndList <- forDyn l ((\n -> [0..n-1]) . _logoConfig_numSheets)

  gAttrs <- forDyn n $ \n' -> s "transform" =: s "skewX(20)"
  svgTag $ elAttrSvg "g" gAttrs $ do
    cAtts  <- combineDyn (\r c -> circAttrs' (200 :: Int) (200 :: Int) (r*10) c) n c
    cAtts' <- combineDyn (\r gp -> circAttrs' (200 :: Int) (200 :: Int) (r*10 - gp*2 :: Double) (sRGB24 255 255 255)) n g

    --cAtts <- forDyn c (\col -> circAttrs ((200 :: Int,200 :: Int),col)

    elAttrSvg "circle" cAtts  (return ())
    elAttrSvg "circle" cAtts' (return ())

  --sheetIndList <- forDyn l ((\n -> [0..n-1]) . _logoConfig_numSheets) -- :: Dynamic t [Int]
  --actions <- forDyn sheetIndList (\ns -> mapM (text . show) ns)  -- :: Dynamic t (m ())
  --forDyn sheetIndList undefined

  return ()


    -- $ \ns -> forM_ ns $ \i -> do
    -- ats <- forDyn l ( ("cx" =: show (i*100) <>) . (s "r" =:) . show . _logoConfig_head)
    -- svgTag $ elAttrSvg "circle" ats (return ())

-- logo :: LogoConfig -> Diagram B
-- logo cfg = shearX 0.1 $ lw 0 $ stack $ _logoConfig_numSheets cfg
--   where n = _logoConfig_head cfg -- 8 -- Head radius
--         m = _logoConfig_stem cfg -- 2.8 -- Ratio of stem length to head radius
--         g = _logoConfig_gap cfg -- 1 -- The width of the gaps between the "sheets" in the logo and the gap at the bottom-left of the head of the P
--         s = _logoConfig_sheet cfg -- 1 -- The thickness of the "sheets" after the main P

--         c :: Colour Double
--         c = _logoConfig_color cfg -- red

--         head :: Double -> Diagram B
--         head thickness = fc white (circle $ n - thickness)
--                          `atop` fc c (circle n)
--                          `atop` fc white (circle $ n + g)

--         p :: Double -> Diagram B
--         p thickness = translate (r2 (-n + thickness/2, -(m*n)/2)) (fc c (rect thickness (m*n)))
--                       `atop` translate (r2 (-n + thickness + g/2, -n/2)) (fc white (rect g n))
--                       `atop` head thickness

--         stack :: Int -> QDiagram B V2 Double Any
--         stack 0 = p 1
--         stack x = translate (r2 (s + g, 0)) (stack (x-1)) `atop` translate (r2 (s/2, -n/2)) (fc white (rect (n*2 - s) n)) `atop` p s

-- circlesWidget :: MonadWidget t m => [Int] -> Dynamic t LogoConfig -> m ()
-- circlesWidget ns l = forM_ ns $ \n -> do
--   ats <- forDyn l ( ("cx" =: show (n*100) <>) . (s "r" =:) . show . _logoConfig_head)
--   svgTag $ elAttrSvg "circle" ats (return ())

