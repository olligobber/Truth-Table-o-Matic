{-# LANGUAGE OverloadedStrings #-}

module WFFParser (parseWFF) where

import Text.Parsec
	( Parsec, ParseError
	, string, spaces, alphaNum, char, (<?>), between, many1, parse, eof, choice
	)
import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.String (fromString)
import Data.Function ((&))

import WFF
	( NullarySymbol(Falsum, Verum)
	, UnarySymbol(Not)
	, BinarySymbol(And, Or, Implies, Equals, Greater, Xor, Nand, Nor)
	, WFF(Proposition, Nullary, Unary, Binary)
	)

type TextParser = Parsec Text ()
type WFFParser = TextParser (WFF Text)

-- Parse nullary symbols
nullarySymbolP :: TextParser NullarySymbol
nullarySymbolP = choice
	[ Falsum <$ (string "⊥" <|> string "_|_")
	, Verum <$ (string "⊤" <|> string "%")
	] <?> "nullary symbol"

-- Parse unary symbols
unarySymbolP :: TextParser UnarySymbol
unarySymbolP =
	Not <$
	(string "-" <|> string "~" <|> string "!" <|> string "¬") <?>
	"unary symbol"

{-
	Binary symbols have the following ways of being inputted:
		And =
			"/\" or
			"*" or
			"∧" or
			one or more "&"
		Or =
			"\/" or
			"∨" or
			one or more "|"
		Implies =
			one or more "-" followed by ">" or
			one or more "=" followed by ">" or
			"→"
		Equals =
			one or more "=" or
			"<" followed by one or more "-" followed by ">" or
			"<" followed by one or more "=" followed by ">" or
			"↔"
		Greater =
			">"
		Xor =
			"+" or
			"<>" or
			"⊻" or
			"~=" or
			"!=" or
			"/="
		Nand =
			"/|\" or
			"~&" or
			"!&" or
			"↑"
		Nor =
			"\|/" or
			"~|" or
			"!|" or
			"↓"
	Unfortunately some of these share the same first character(s) so the
	parser is not simple
-}

binarySymbolP :: TextParser BinarySymbol
binarySymbolP = choice
	[ string "/" *> choice
		[ Nand <$ string "|\\"
		, And <$ string "\\"
		, Xor <$ string "="
		]
	, string "\\" *> choice
		[ Nor <$ string "|/"
		, Or <$ string "/"
		]
	, many1 (string "=") *> choice
		[ Implies <$ string ">"
		, pure Equals
		]
	, string "<" *> choice
		[ Xor <$ string ">"
		, Equals <$ ((many1 (string "-") <|> many1 (string "=")) *> string ">")
		]
	, string "~" *> choice
		[ Nand <$ string "&"
		, Nor <$ string "|"
		, Xor <$ string "="
		]
	, string "!" *> choice
		[ Nand <$ string "&"
		, Nor <$ string "|"
		, Xor <$ string "="
		]
	, And <$ many1 (string "&")
	, And <$ (string "*" <|> string "∧")
	, Or <$ many1 (string "|")
	, Or <$ string "∨"
	, Implies <$ ((many1 (string "-") *> string ">") <|> string "→")
	, Equals <$ string "↔"
	, Greater <$ string ">"
	, Xor <$ (string "+" <|> string "⊻")
	, Nand <$ string "↑"
	, Nor <$ string "↓"
	] <?> "binary symbol"

-- Parse a proposition, made of alpha-numeric characters
propositionP :: WFFParser
propositionP = Proposition . fromString <$> many1 alphaNum <?> "proposition"

-- Make a parser work inside brackets
bracketted :: TextParser x -> TextParser x
bracketted p = sb '(' ')' <|> sb '[' ']' <|> sb '{' '}' where
	sb o c = between (char o) (char c) $ spaces *> p <* spaces

{-
	The grammer used here is
	S -> T R
	T -> ( S ) | U T | N | P
	R -> B T | empty
	P -> any proposition
	B -> any binary operator
	U -> any unary operator
	N -> any nullary operator
-}

-- Parse an expression, this is S in the grammar
expressionP :: WFFParser
expressionP = (&) <$> safeExpressionP <*> restExpressionP

-- Parse an expression that can be safely nested, this is T in the grammar
safeExpressionP :: WFFParser
safeExpressionP = choice
	[ bracketted expressionP
	, Unary <$> unarySymbolP <*> (spaces *> safeExpressionP)
	, Nullary <$> nullarySymbolP
	, propositionP
	]

restExpressionP :: TextParser (WFF Text -> WFF Text)
restExpressionP = spaces *> choice
	[ do
		b <- binarySymbolP
		spaces
		e <- safeExpressionP
		pure $ \x -> Binary b x e
	, pure id
	]

-- Given a source and formula in text, parse it
parseWFF :: String -> Text -> Either ParseError (WFF Text)
parseWFF = parse $ spaces *> expressionP <* spaces <* eof
