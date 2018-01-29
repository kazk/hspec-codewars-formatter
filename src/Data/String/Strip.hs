module Data.String.Strip
    (
      strip
    , stripDigits
    ) where

import Data.Char (isSpace, isHexDigit)

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- failure
stripDigits :: String -> String
stripDigits = dropWhile isHexDigit . reverse . dropWhile isHexDigit . reverse
