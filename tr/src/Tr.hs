-- | Haskell tr implementation. Just supports the swap and delete modes:
-- * tr string1 string2
-- * tr -d string1
--
-- PLEASE DON'T CHANGE THE INTERFACE OF THIS FILE AS WE WILL EXPECT IT TO BE
-- THE SAME WHEN TESTING!


module Tr
    ( CharSet
    , tr
    ) where

import qualified Data.Map.Strict as Map
import           Data.Maybe
-- | Just to give `tr` a more descriptive type
type CharSet = String



-- | 'tr' - the characters in the first argument are translated into characters
-- in the second argument, where first character in the first CharSet is mapped
-- to the first character in the second CharSet. If the first CharSet is longer
-- than the second CharSet, the last character found in the second CharSet is
-- duplicated until it matches in length.
--
-- If the second CharSet is a `Nothing` value, then 'tr' should run in delete
-- mode where any characters in the input string that match in the first
-- CharSet should be removed.
--
-- The third argument is the string to be translated (i.e., STDIN) and the
-- return type is the output / translated-string (i.e., STDOUT).
--
-- translate mode: tr "eo" (Just "oe") "hello" -> "holle"
-- delete mode: tr "e" Nothing "hello" -> "hllo"
--
-- It's up to you how to handle the first argument being the empty string, or
-- the second argument being `Just ""`, we will not be testing this edge case.
tr :: CharSet -> Maybe CharSet -> String -> String
tr _inset _outset xs = case _outset of
    Just value -> translates (leftZip _inset value) xs
    Nothing    -> delete _inset xs

leftZip :: [a] -> [b] -> [(a,b)]
leftZip (x:xs) [y]    = (x, y) : leftZip xs [y]
leftZip (x:xs) (y:ys) = (x, y) : leftZip xs ys
leftZip [] _          = []
leftZip (_:_) []      = []


translates :: [(Char, Char)] -> String -> String
translates pairs = map (translate (Map.fromList pairs))
    where translate :: Map.Map Char Char -> Char -> Char
          translate charMap _in = case Map.lookup _in charMap of
            Just value -> value
            Nothing    -> _in


delete :: CharSet -> String -> String
delete _inset = mapMaybe (\x -> if x `elem` _inset then Nothing else Just x )
