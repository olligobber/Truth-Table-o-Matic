module Logical
	( Logical(evalNullary, evalUnary, evalBinary)
	, evaluate
	) where

import Data.Function (on)

import WFF
	( NullarySymbol(Falsum, Verum)
	, UnarySymbol(Not)
	, BinarySymbol(And, Or, Implies, Equals, Greater, Xor, Nand, Nor)
	, WFF(Proposition, Nullary, Unary, Binary)
	)

-- Boolean-like types
class Eq x => Logical x where
	evalNullary :: NullarySymbol -> x
	evalUnary :: UnarySymbol -> x -> x
	evalBinary :: BinarySymbol -> x -> x -> x

instance Logical Bool where
	evalNullary Falsum = False
	evalNullary Verum = True
	evalUnary Not x = not x
	evalBinary And x y = x && y
	evalBinary Or x y = x || y
	evalBinary Implies x y = x <= y
	evalBinary Equals x y = x == y
	evalBinary Greater x y = x > y
	evalBinary Xor x y = x /= y
	evalBinary Nand x y = not $ x && y
	evalBinary Nor x y = not $ x || y

-- Evaluate a formula whose propositions are booleans
evaluate :: Logical x => WFF x -> x
evaluate (Proposition x) = x
evaluate (Nullary n) = evalNullary n
evaluate (Unary u w) = evalUnary u $ evaluate w
evaluate (Binary b w1 w2) = (evalBinary b `on` evaluate) w1 w2
