{-# LANGUAGE OverloadedStrings #-}

module WFF
	( NullarySymbol(Falsum, Verum)
	, UnarySymbol(Not)
	, BinarySymbol(And, Or, Implies, Equals, Greater, Xor, Nand, Nor)
	, WFF(Proposition, Nullary, Unary, Binary)
	, applyMap
	) where

import Data.Function (on)
import Control.Applicative (liftA2)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Control.Monad (ap)
import Data.Traversable (foldMapDefault)
import Data.Text (Text)

import Render (Renderable(render))

-- Nullary Symbols
data NullarySymbol = Falsum | Verum deriving (Eq, Ord, Show)

instance Renderable NullarySymbol where
	render Falsum = "⊥"
	render Verum = "⊤"

-- Unary Symbols
data UnarySymbol = Not deriving (Eq, Ord, Show)

instance Renderable UnarySymbol where
	render Not = "¬"

-- Binary Symbols
data BinarySymbol = And | Or | Implies | Equals | Greater | Xor | Nand | Nor
	deriving (Eq, Ord, Show)

instance Renderable BinarySymbol where
	render And = "∧"
	render Or = "∨"
	render Implies = "→"
	render Equals = "↔"
	render Greater = ">"
	render Xor = "<>"
	render Nand = "↑"
	render Nor = "↓"

-- Logical formula datatype
data WFF c
	= Proposition c
	| Nullary NullarySymbol
	| Unary UnarySymbol (WFF c)
	| Binary BinarySymbol (WFF c) (WFF c)
	deriving (Eq, Ord, Show)

instance Functor WFF where -- Use Monad instance to define this
	fmap f m = m >>= (pure . f)

instance Applicative WFF where -- Use Monad instance to define this
	pure = Proposition
	(<*>) = ap

{-
	The Monad and Applicative instances basically nest later structures into
	earlier structures. The main point of this is that >>= can be used to
	substitute propositions for formulas
-}
instance Monad WFF where
	(Proposition p) >>= f = f p
	(Nullary n) >>= _ = Nullary n
	(Unary u w) >>= f = Unary u $ w >>= f
	(Binary b w1 w2) >>= f = (Binary b `on` (>>= f)) w1 w2

instance Foldable WFF where -- Use Traversable instance to define this
	foldMap = foldMapDefault

-- Traversable on propositions
instance Traversable WFF where
	sequenceA (Proposition p) = Proposition <$> p
	sequenceA (Nullary n) = pure $ Nullary n
	sequenceA (Unary u w) = Unary u <$> sequenceA w
	sequenceA (Binary b w1 w2) = (liftA2 (Binary b) `on` sequenceA) w1 w2

-- Text version of ShowS functions
showParenT :: Bool -> (Text -> Text) -> Text -> Text
showParenT False t = t
showParenT True t = ("(" <>) . t . (")" <>)

showText :: Text -> (Text -> Text)
showText = (<>)

-- Nice rendering for the user
rendersPrec :: Int -> (c -> Text) -> WFF c -> Text -> Text
rendersPrec _ f (Proposition p) = showText $ f p
rendersPrec _ _ (Nullary n) = showText $ render n
rendersPrec p f (Unary u w) = showParenT (p>2) $
	showText (render u) . rendersPrec 2 f w
rendersPrec p f (Binary b w1 w2) = showParenT (p>1) $
	rendersPrec 2 f w1 . showText (render b) . rendersPrec 2 f w2

instance Renderable x => Renderable (WFF x) where
	render wff = rendersPrec 1 render wff ""

-- Apply a mapping from match to some formula
applyMap :: Ord x => Map x (WFF x) -> WFF x -> WFF x
applyMap m w = w >>= \p -> case M.lookup p m of
	Nothing -> pure p
	Just n -> n
