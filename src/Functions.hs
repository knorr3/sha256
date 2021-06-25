module Functions

where


import Data.Word
import Data.Bits

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

-- Some default values
primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311]

zero :: Word32
zero = 0

exampleWord1 :: Word32
exampleWord1 = 4278255360

exampleWord2 :: Word32
exampleWord2 = 16383

-- Some useful util functions
bin2dec :: String -> Integer
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

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

-- Message Functions
cutBlocks :: String -> [[Word32]]
cutBlocks [] = []
cutBlocks s  = cutWords (take 512 s) : cutBlocks (drop 512 s)

cutWords :: String -> [Word32]
cutWords [] = []
cutWords s  = fromIntegral (bin2dec $ take 32 s) : cutWords (drop 32 s)

messageSchedule :: Int -> [Word32] -> [Word32]
messageSchedule 64 _ = []
messageSchedule 16 original = original ++ temporary : messageSchedule (i+1) (original ++ [temporary])
        where temporary = σ1 (original !! (i-2)) + (original !! (i-7)) + σ0 (original !! (i-15)) + (original !! (i-16))
              i = 16 
messageSchedule i original = temporary : messageSchedule (i+1) (original ++ [temporary])
        where temporary = σ1 (original !! (i-2)) + (original !! (i-7)) + σ0 (original !! (i-15)) + (original !! (i-16))

-- Hashing Functions
initial :: [Word32]
initial = [ constant2 0
          , constant2 1
          , constant2 2
          , constant2 3
          , constant2 4
          , constant2 5
          , constant2 6
          , constant2 7
          ]

t1 :: Word32
t1 = undefined

t2 :: Word32
t2 = undefined 

compression :: [Word32] -> [Word32]
compression ms = undefined  

-- Word Operations
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

root3 :: Int -> Double
root3 t = (primes !! t) ** (1/3)

constant3 :: Int -> Word32
constant3 t = floor ((root3 t - fromIntegral (floor (root3 t))) * 2^32)

root2 :: Int -> Double
root2 t = (primes !! t) ** (1/2)

constant2 :: Int -> Word32
constant2 t = floor ((root2 t - fromIntegral (floor (root2 t))) * 2^32)