{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString qualified as BS (readFile)
import Day14
import Grid (render)

main :: IO ()
main =
  parseFromFile parser "input/day14.txt" >>= \case
    Left err -> print err
    Right input -> putStr ("\n" ++ render (const ((\x -> if x == 0 then "." else show x) . length)) input) >> putStrLn ("Solution:\n" ++ show (partTwo @103 @101 input))
  where
    parseFromFile p f = parseOnly p <$> BS.readFile f
