# Second Program: Upload Spreadsheet to google bigquery

In this chapter, we are going to write a command line program to upload a spreadsheet file's content to Google BigQuery. There is awesome library Gogol provides Haskell binding to Google API.

```bash
stack new xlsx-bg simple
```

We need gogol 0.3.0 or higher, Make sure your project's resolver version is lts-9.0 or later. You can verify it by doing `stack list-dependencies`.

## Starting Point

So how to use Gogol library ? The lib provides an [example](https://github.com/brendanhay/gogol/blob/develop/examples/src/Example/Storage.hs) for Google Cloud Storage.

```haskell
module Main where

import Control.Lens                 ((&), (.~), (<&>), (?~))
import Control.Monad.Trans.Resource (liftResourceT, runResourceT)
import Control.Monad.IO.Class

import Data.Conduit (($$+-))
import Data.Text    (Text)

import System.IO (stdout)
import Network.Google.Auth   (Auth, Credentials (..), initStore)
import qualified Network.Google         as Google
import qualified Network.Google.BigQuery as BigQuery
import Network.HTTP.Conduit (Manager, newManager, tlsManagerSettings)
import           Network.Google.Auth.Scope       (AllowScopes (..),                                                  concatScopes)

example :: IO BigQuery.ProjectList
example = do
    lgr  <- Google.newLogger Google.Debug stdout
    m <- liftIO (newManager tlsManagerSettings) :: IO Manager
    c <- Google.getApplicationDefault m
 -- Create a new environment which will discover the appropriate
 -- AuthN/AuthZ credentials, and explicitly state the OAuth scopes
 -- we will be using below, which will be enforced by the compiler:
     env  <- Google.newEnvWith c lgr m <&>
           (Google.envLogger .~ lgr)
         . (Google.envScopes .~ BigQuery.bigQueryScope)
    runResourceT . Google.runGoogle env $ Google.send BigQuery.projectsList

 main :: IO ()
 main = do
     projects <- example
     print projects
 ``` 

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