{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Text.Parsec (parse)
import Data.List (intercalate)
import Ast
import TypeParser
import Serialize
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as B

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

declPath :: Decl -> Maybe FilePath
declPath (FreeText s) = Just $ "out/" ++ s ++ ".json"
declPath (Keywords s _) = Just $ "out/" ++ s ++ ".json"
declPath (Trait s _) = Just $ "out/" ++ s ++ ".json"
declPath (Alias s _) = Nothing

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
      let jsons = map (\x -> "{\"data\":" `B.append` x `B.append` "}")
            $ map encode $ mapMaybe serializeConfig ds
      mapM_ (\(path, json) -> B.writeFile path json)
        $ zip (mapMaybe declPath ds) jsons
    Left e -> putStrLn $ show e
