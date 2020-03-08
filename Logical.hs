module Logical (
    Logical(..),
    evaluate
) where

import Data.Function (on)

import WFF(WFF(..))

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

evaluate :: Logical x => WFF x -> x
evaluate (Prop x) = x
evaluate (Not w) = neg $ evaluate w
evaluate (left :|: right) = (join `on` evaluate) left right
evaluate (left :&: right) = (meet `on` evaluate) left right
evaluate (left :>: right) = evaluate $ Not left :|: right
evaluate (left :=: right) = evaluate $ (left :>: right) :&: (right :>: left)
