# Second Program `tr`

In this chapter, we are going to work on the [lab1](http://www.scs.stanford.edu/16wi-cs240h/labs/lab1.html).


# Third Program: Upload Spreadsheet to google bigquery

In this chapter, we are going to write a command line program to upload a spreadsheet file's content to Google BigQuery. There is awesome library Gogol provides Haskell binding to Google API.

```bash
stack new xlsx-bg simple
```

We need gogol 0.3.0 or higher, Make sure your project's resolver version is lts-9.0 or later. You can verify it by doing `stack list-dependencies`.

## Starting Point

So how to use Gogol library ? The lib provides an [example](https://github.com/brendanhay/gogol/blob/develop/examples/src/Example/Storage.hs) for Google Cloud Storage.

Let's do a little change, so it fetch all the bigquery project the current default google credential has access to.

You need to install gcloud, and setup default
```bash
gcloud init
gcloud auth application-default login
```

and let's put the following code into our `Main.hs`.

```haskell
module Main where

import Control.Lens                 ((&), (.~), (<&>), (?~))
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class

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

You need add following build depends to make stack build successed.

```yaml
  build-depends:       base >= 4.7 && < 5
                     , bytestring
                     , conduit
                     , conduit-extra
                     , gogol
                     , gogol-bigquery
                     , gogol-core
                     , http-conduit
                     , lens
                     , resourcet
```

Quite few things need to unpack here.

## Create a bigquery dataset

### Crash Course for Lens

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

[catchJust](https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Exception.html#v:catchJust)

TODO: Add exception handle in the guess number program
