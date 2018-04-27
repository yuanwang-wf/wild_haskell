{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO        as T
import           Network.HTTP.Simple (httpSink)
import           Text.HTML.DOM       (sinkDoc)
import           Text.XML.Cursor     (attributeIs, content, element,
                                      fromDocument, ($//), (&/), (&//))


main :: IO ()
main = do
    doc <- httpSink "http://www.yesodweb.com/book" $ const sinkDoc
    let cursor = fromDocument doc
    T.putStrLn "Chapters in the Yesod book:\n"
    mapM_ T.putStrLn
        $ cursor
        $// attributeIs "class" "main-listing"
        &// element "li"
        &/ element "a"
        &/ content
