module Lib
    ( fibonacci, rev, median,
    distance, guests, -- hw0
    ) where

{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

fibonacci :: Int -> Integer
fibonacci n = mem !! n
    where
        mem :: [Integer]
        mem = [fib i | i <- [0..]]

        fib :: Int -> Integer
        fib 0 = 0
        fib 1 = 1
        fib i = mem !! (i - 1) + mem !! (i - 2)

rev :: [a] -> [a]
rev = foldl (flip (:)) []

median :: [String] -> Double
median sl = med il
    where
        il = sort $ map length sl

        med :: [Int] -> Double
        med [] = 0
        med l
            | odd i = fromIntegral (sorted !! middle)
            | otherwise = (fromIntegral (sorted !! middle) + fromIntegral (sorted !! (middle - 1))) / 2
            where
                i = length l
                sorted = sort l
                middle = div i 2

distance :: IO ()
distance = do
    input <- getLine
    let [rows, cols] = map read (words input) :: [Int] -- Always two inputs, ignore the warning

    putStrLn $ rectangle rows cols
    where
        rectangle :: Int -> Int -> String
        rectangle rows cols = unlines [concat [symbol (distanceToEdge row col rows cols) | col <- [0..cols-1]] | row <- [0..rows-1]]
            
        symbol :: Int -> String
        symbol n = if n < 10 then show n else "."
            
        distanceToEdge :: Int -> Int -> Int -> Int -> Int
        distanceToEdge row col rows cols
            | d <- min (rows - row) (row + 1)
            , d' <- min (cols - col) (col + 1)
            = min d d'

guests :: IO ()
guests = do
    input <- TextIO.getContents
    let l = Text.lines input
    let h = read (Text.unpack (head l)) :: Int
    let t = tail l
    let n = HashSet.fromList (zip t (drop h t))

    print $ HashSet.size n