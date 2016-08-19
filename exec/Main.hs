{-# language OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Reflex.Dom

url = "https://api.travis-ci.org/repos/snapframework"
url' = "http://www.test-cors.org"
url'' = "http://18.93.13.11/"

main = mainWidget $ do
  text "Hello"
  b <- button "Go"
  r <- performRequestAsync $ ffor b $ \() ->
         XhrRequest "GET" url (def { _xhrRequestConfig_responseHeaders = OnlyHeaders ["hello-reflex"]})
  dynText =<< holdDyn "Nothing" (fmap (T.pack . show . _xhrResponse_status) r)
  dynText =<< holdDyn "Nothing" (fmap (T.pack . show . _xhrResponse_headers) r)
