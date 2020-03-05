module DiffChecker.Types
  ( Json(..)
  , int , string , number , boolean , array , strmap , onlyUndefined
  , Diff
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Lens (Prism', only, preview, prism')
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap (StrMap)
import Data.Tuple (Tuple)

data Json
  = I Int
  | S String
  | N Number
  | B Boolean
  | A (Array Json)
  | Map (StrMap Json)
  | Undefined

type Diff = Tuple Json Json

-- *** Prisms *** --

int :: Prism' Json Int
int = prism' I case _ of
  (I a) → Just a
  _     → Nothing

string :: Prism' Json String
string = prism' S case _ of
  (S a) → Just a
  _     → Nothing

number :: Prism' Json Number
number = prism' N case _ of
  (N a) → Just a
  _     → Nothing

boolean :: Prism' Json Boolean
boolean = prism' B case _ of
  (B a) → Just a
  _     → Nothing

array :: Prism' Json (Array Json)
array = prism' A case _ of
  (A a) → Just a
  _     → Nothing

strmap :: Prism' Json (StrMap Json)
strmap = prism' Map case _ of
  (Map a) → Just a
  _      → Nothing

onlyUndefined :: Prism' Json Unit
onlyUndefined = only Undefined

eval :: ∀ a. (a → a → Boolean) → Prism' Json a → Json → Json → Maybe Boolean
eval f g a b = f <$> (preview g a) <*> (preview g b)

instance eqJson :: Eq Json where
  eq Undefined Undefined = true
  eq a b = fromMaybe false $
    eval eq int     a b <|>
    eval eq string  a b <|>
    eval eq number  a b <|>
    eval eq boolean a b <|>
    eval eq array   a b <|>
    eval eq strmap  a b

instance encodeJson :: Encode Json where
  encode (I v) = encode v
  encode (S v) = encode v
  encode (N v) = encode v
  encode (B v) = encode v
  encode (Map v) = encode v
  encode (A v) = encode v
  encode Undefined = encode "undefined"

instance decodeJson :: Decode Json where
  decode json =
    (I <$> (decode json))
    <|> (A <$> (decode json))
    <|> (S <$> (decode json))
    <|> (N <$> (decode json))
    <|> (B <$> (decode json))
    <|> (Map <$> (decode json))
    <|> (pure Undefined)
