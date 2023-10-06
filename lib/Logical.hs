module Logical
	( Logical(meet, join, neg, top, bot)
	, evaluate
	) where

import Data.Function (on)

import WFF(WFF(Prop, Not, (:|:), (:&:), (:>:), (:=:)))

-- Boolean-like types
class Eq x => Logical x where
	meet :: x -> x -> x -- Logical and
	join :: x -> x -> x -- Logical or
	neg :: x -> x -- Logical not
	top :: x -- Logical true
	bot :: x -- Logical false

instance Logical Bool where
	meet = (&&)
	join = (||)
	neg = not
	top = True
	bot = False

-- Evaluate a formula whose propositions are booleans
evaluate :: Logical x => WFF x -> x
evaluate (Prop x) = x
evaluate (Not w) = neg $ evaluate w
evaluate (left :|: right) = (join `on` evaluate) left right
evaluate (left :&: right) = (meet `on` evaluate) left right
evaluate (left :>: right) = evaluate $ Not left :|: right
evaluate (left :=: right) = evaluate $ (left :>: right) :&: (right :>: left)
