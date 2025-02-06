{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString qualified as BS (readFile)
import Day16

main :: IO ()
main =
  parseFromFile (parser @15) "input/day16.txt" >>= \case
    Left err -> print err
    Right input -> putStr ("\n" ++ show input) >> putStrLn ("Solution:\n" ++ show (solution input))
  where
    parseFromFile p f = parseOnly p <$> BS.readFile f
