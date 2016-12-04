-- | Common utility functions.
module Utils where

-- | Given `substring a b`, determine if `a` is a substring of `b`.
--   source: http://stackoverflow.com
substring :: String -- ^ String for which to look.
          -> String -- ^ String to be searched.
          -> Bool   -- ^ Result, `True` if substring, `False` otherwise
substring (x:xs) [] = False
substring xs ys
  | prefix xs ys = True
  | substring xs (tail ys) = True
  | otherwise = False

-- | Given `prefix a b`, determines if `a` is a prefix of `b`.
--   source: http://stackoverflow.com
prefix :: String -- ^ Possible prefix.
       -> String -- ^ String to check.
       -> Bool   -- ^ `True` if prefixed, `False` otherwise.
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys
