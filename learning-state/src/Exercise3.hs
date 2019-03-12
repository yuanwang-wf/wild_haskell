module Exercise3 where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Text.Read (readMaybe)
import System.IO

prompt :: Read a => String -> MaybeT IO a
prompt question = do
  lift $ putStr question
  lift $ putStr ": "
  lift $ hFlush stdout
  answer <- lift getLine
  MaybeT (return $ readMaybe answer)

ageInYear :: MaybeT IO Int
ageInYear = do
  birthYear <- prompt "Birth year"
  futureYear <- prompt "Future year"
  return $ futureYear - birthYear

main' :: IO ()
main' = do
  mage <- runMaybeT ageInYear
  case mage of
    Nothing -> putStrLn $ "Some problem with input, sorry"
    Just age -> putStrLn $ "In that year, age will be: " ++ show age