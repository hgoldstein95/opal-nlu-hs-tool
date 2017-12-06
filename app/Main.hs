module Main where

import System.Environment (getArgs)
import Text.Parsec (parse)
import Data.List (intercalate)
import Ast
import TypeParser
import Serialize
import Data.Aeson (encode)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) =
  case f x of
    Just y -> y:(mapMaybe f xs)
    Nothing -> mapMaybe f xs

printContent :: [Decl] -> IO ()
printContent ds = do
  putStrLn $ intercalate "\n" $ map serializeType ds
  putStrLn ""
  putStrLn $ intercalate "\n" $ map serializeRuntype ds

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let decls = parse parseDecls "" content
  case decls of
    Right ds -> do
      putStrLn "// ------- GENERATED CODE -------"
      printContent ds
      putStrLn "// ----- END GENERATED CODE -----"
      putStrLn "\n"
      putStrLn $ intercalate "\n"
        $ map (show . encode)
        $ mapMaybe serializeConfig ds
    Left e -> putStrLn $ show e
