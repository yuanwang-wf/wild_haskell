# Second Program: Upload Spreadsheet to google bigquery

In this chapter, we are going to write a command line program to upload a spreadsheet file's content to Google BigQuery. There is awesome library Gogol provides Haskell binding to Google API.

```bash
stack new xlsx-bg simple
```

We need gogol 0.3.0 or higher, Make sure your project's resolver version is lts-9.0 or later. You can verify it by doing `stack list-dependencies`.

## Lens

```haskell
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens

data Point = Point
  { _postionX :: Double
  , _postionY :: Double} deriving (Show)

makeLenses ''Point

data Segment = Segment {
  _segmentStart :: Point,
  _segmentEnd   :: Point
} deriving (Show)
makeLenses ''Segment

makePoint :: (Double , Double) -> Point
makePoint = uncurry Point

makeSegment :: (Double, Double) -> (Double, Double) -> Segment
makeSegment start end = Segment (makePoint start) (makePoint end)

updateSegment :: Segment -> Segment
updateSegment = (segmentStart .~ makePoint (10, 10)) . (segmentEnd .~ makePoint (10, 10))
```

## Catch Exception

http://www.scs.stanford.edu/16wi-cs240h/slides/concurrency-slides.html#(1)

Add exception handle in the guess number program