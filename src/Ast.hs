module Ast where

import Data.List
import Text.Printf

data Decl
  = FreeText String
  | Keywords String [String]
  | Trait String [(String, Ty)]
  | Alias String Ty
  deriving (Show)

data Ty
  = Def String
  | Lit String
  | Rec [(String, Ty)]
  deriving (Show)

quote :: String -> String
quote s = "\"" ++ s ++ "\""

tyToRuntype :: Ty -> String
tyToRuntype (Def l) = printf "RT_%s" l
tyToRuntype (Lit l) = printf "Literal(%s)" (quote l)
tyToRuntype (Rec rs) =
  let ps' = map (\(s, t) -> s ++ ": " ++ tyToRuntype t) rs
  in "Record({" ++ intercalate ", " ps' ++ "})"

serializeRuntype :: Decl -> String
serializeRuntype (FreeText s) = printf "const RT_%s = FreeText(%s);" s (quote s)
serializeRuntype (Keywords s ls) =
  let ls' = map quote ls
  in printf "const RT_%s = Keywords(%s, [%s]);" s (quote s) (intercalate ", " ls')
serializeRuntype (Trait tag ps) =
  let ts = map (\(l, t) -> printf "%s: %s" l (tyToRuntype t)) ps
  in printf "const RT_%s = Trait(%s, {%s});" tag (quote tag) (intercalate ", " ts)
serializeRuntype (Alias tag ty) = printf "const RT_%s = %s;" tag (tyToRuntype ty)

tyToType :: Ty -> String
tyToType (Def l) = l
tyToType (Lit l) = printf "%s" (quote l)
tyToType (Rec rs) =
  let ps' = map (\(s, t) -> s ++ ": " ++ tyToType t) rs
  in "{" ++ intercalate ", " ps' ++ "}"

serializeType :: Decl -> String
serializeType (FreeText s) = printf "type %s = string;" s
serializeType (Keywords s ls) =
  let ls' = map quote ls
  in printf "type %s = %s;" s (intercalate " | " ls')
serializeType (Trait tag ps) =
  let ts = map (\(l, t) -> printf "{tag: %s, data: %s}"
                 (quote l) (tyToType t)) ps
  in printf "type %s = %s;" tag (intercalate " | " ts)
serializeType (Alias tag ty) = printf "type %s = %s;" tag (tyToType ty)
