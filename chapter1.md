# Install haskell and your first program Guess Number

## Install Stack

[https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)

make sure `stack version` outputs latest stack version, currently `Version 1.5.0`.

`stack upgrade`

## Editor Integration

### Atom

### Emacs

[intero](https://commercialhaskell.github.io/intero/)

### VScode with haskell-ide-engine

Haskell Syntax Highlight

[haskell-ide-engine](https://github.com/haskell/haskell-ide-engine)

Install on MacOS

```bash
brew install icu4c && brew link icu4c --force
stack install text-icu --extra-lib-dirs=/usr/local/opt/icu4c/lib --extra-include-dirs=/usr/local/opt/icu4c/include
```

```bash
git clone https://github.com/haskell/haskell-ide-engine

stack install hoogle

hoogle generate
stack install
```

## Stack

```bash
stack version

stack upgrade
```

## Your first haskell program

```bash
stack new guessNumber simple
```

```haskell
module Main where

import           Control.Monad
import           Data.Ord      (compare)
import           System.IO     (readLn)
import           System.Random (randomRIO)


guess :: Int -> IO ()
guess secretNumber = do
    print "guess a number"
    guessNumber <- readLn :: IO Int
    case compare guessNumber secretNumber of
        LT -> (print "Too Small!") >> (guess secretNumber)
        EQ -> print "You Win!"
        GT -> (print "Too big!") >> (guess secretNumber)


main :: IO ()
main = do
    secretNumber <- randomRIO (0, 100) :: IO Int
    guess secretNumber
```