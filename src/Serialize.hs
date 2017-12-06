{-# LANGUAGE DeriveGeneric #-}

module Serialize where

import Ast
import Data.Aeson
import GHC.Generics

data EValue = EValue
  { value :: String
  , expressions :: [String]
  } deriving (Generic, Show)

data Entity = Entity
  { lookups :: [String]
  , name :: String
  , lang :: String
  , exotic :: Bool
  , uid :: String
  , values :: [EValue]
  } deriving (Generic, Show)

instance ToJSON EValue where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Entity where
  toEncoding = genericToEncoding defaultOptions

mkEValue :: String -> EValue
mkEValue v = EValue {value = v, expressions = []}

mkEntity :: String -> String -> [String] -> Entity
mkEntity l n vs =
  Entity
  { lookups = [l]
  , name = n
  , lang = "en"
  , exotic = False
  , uid = n
  , values = map mkEValue vs
  }

serializeConfig :: Decl -> Maybe Entity
serializeConfig (FreeText s) = Just $ mkEntity "free-text" s []
serializeConfig (Keywords s ls) = Just $ mkEntity "keywords" s ls
serializeConfig (Trait s ps) = Just $ mkEntity "trait" s (map fst ps)
serializeConfig (Alias _ _) = Nothing
