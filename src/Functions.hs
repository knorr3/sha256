module Functions

where


import Data.Word
import Data.Bits

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

toBinary :: Integer -> String
toBinary x = showIntAtBase 2 intToDigit x ""

printLeft :: Int -> Word32  -> String
printLeft i word = expandLeft i $ toBinary $ fromIntegral word

expandLeft :: Int -> String -> String
expandLeft i s = if length s == i
                 then s
                 else expandLeft i ('0':s)

printRight :: Int -> Word32  -> String
printRight i word = expandRight i $ toBinary $ fromIntegral word

expandRight :: Int -> String -> String
expandRight i s = if length s == i
                 then s
                 else expandRight i ('0':s)

padding :: String -> String
padding s = if (length s + 64) `mod` 512 == 0 && length s > 64
            then s
            else padding (s ++ ['0'])

cutBlocks :: String -> [[String]]
cutBlocks [] = []
cutBlocks s  = cutWords (take 512 s) : cutBlocks (drop 512 s)

cutWords :: String -> [String]
cutWords [] = []
cutWords s  = take 32 s : cutWords (drop 32 s)

messageSchedules :: [[String]] -> [[String]]
messageSchedules = map (messageSchedule 0)

messageSchedule :: Integer -> [String] -> [String]
messageSchedule [0..15] (s:rest) = undefined 


primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311]

zero :: Word32
zero = 0

exampleWord1 :: Word32
exampleWord1 = 4278255360

exampleWord2 :: Word32
exampleWord2 = 16383

shr :: Word32 -> Int -> Word32
shr = shiftR

rotr :: Word32 -> Int -> Word32
rotr = rotateR

add :: Word32 -> Word32 -> Word32
add a b = a + b

andW :: Word32 -> Word32 -> Word32
andW a b = (.&.) a b

σ0 :: Word32 -> Word32
σ0 word = xor (xor a b) c
                where
                a = rotr word 7
                b = rotr word 18
                c = shr word 3

σ1 :: Word32 -> Word32
σ1 word = xor (xor a b) c
                where
                a = rotr word 17
                b = rotr word 19
                c = shr word 10

bσ0 :: Word32 -> Word32
bσ0 word =  xor (xor a b) c
                where
                a = rotr word 2
                b = rotr word 13
                c = rotr word 22

bσ1 :: Word32 -> Word32
bσ1 word =  xor (xor a b) c
                where
                a = rotr word 6
                b = rotr word 11
                c = rotr word 25

ch :: Word32 -> Word32 -> Word32 -> Word32
ch x y z = xor (x `andW` y) (complement x `andW` z)

maj :: Word32 -> Word32 -> Word32 -> Word32
maj x y z = xor (xor a b) c
                where
                a = x `andW` y
                b = x `andW` z
                c = y `andW` z

root :: Int -> Double
root t = (primes !! t) ** (1/3)

constant :: Int -> Word32
constant t = floor ((root t - fromIntegral (floor (root t))) * 2^32)