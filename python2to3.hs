{-
This program adds brackets around the expression following
any occurance of the python keyword 'print ' that follows
a new line and whitespace. 

Default output location is directory of this program.
-}
module Python2to3 where

import Data.String
import System.IO
import Text.Regex.Posix
import Data.List

brack fileLoc = do
  fileString <- readFile fileLoc
  outputData.(intercalate "\n").(map processData).lines $ fileString
--intercalate is intersperse for lists (Data.List)

processData :: String -> String
processData line = let printpart = (line =~ print' :: String) in
                        if ((line =~ print' :: Bool) && not (line =~ print'' :: Bool) )
                            then printpart ++ "(" ++ drop (length printpart) line ++ ")"
                            else line
                            
outputData :: String -> IO ()
outputData processed = writeFile "output.py" processed

print' = "[\t ]*print "
print'' = "[\t ]*print[\t ]*[(]"
