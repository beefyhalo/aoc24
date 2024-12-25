{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString qualified as BS (readFile)
import Day15
import Grid (render)

main :: IO ()
main =
  parseFromFile (parser @10 ) "input/day15.txt" >>= \case
    Left err -> print err
    Right input -> putStr ("\n" ++ show input) >> putStrLn ("Solution:\n" ++ show (partTwo input))
  where
    parseFromFile p f = parseOnly p <$> BS.readFile f
