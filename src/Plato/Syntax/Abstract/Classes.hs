module Plato.Syntax.Abstract.Classes where

class CParsing a
class CResolved a
class CTyping a
class CTyped a

data Parsing
instance CParsing Parsing

data Typing
instance CTyping Typing