module SHA256

where


import Functions
import Data.Word
import Data.Bits

import Numeric (showIntAtBase)
import Data.Char
import Data.String

hash :: String -> String
hash x = do
         let asInt = map fromEnum x
         let messageString = map fromIntegral asInt
         let concatString = concat $ map (printLeft 8) messageString ++ ["1"]
         let paddedString = padding concatString
         let message = paddedString ++ printLeft 64 (fromIntegral $ length concatString - 1)
         let blocks = cutBlocks message
         let schedules = map (messageSchedule 16) blocks
         let compressed = map compression schedules
         print2 compressed
         



print2 :: [[Word32]] -> String
print2 (x:xs) = (print1 x) ++ (print2 xs)
print2 [] = []


print1 :: [Word32] -> String
print1 (x:xs) = (printLeft 32 x) ++ "\n" ++ print1 xs
print1 [] = []
