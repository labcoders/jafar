module Sugar where

import Types

-- | Constructs an expression to compare two expressions, evaluating to true if
-- the first expression evaluates to a greater value than the second.
(#>#) :: Action -> Action -> Action
a1 #># a2 = Compare GT a1 a2

-- | Constructs an expression to compare two expressions, evaluating to true if
-- the expressions evaluate to the same value.
(#==#) :: Action -> Action -> Action
a1 #==# a2 = Compare EQ a1 a2

-- | Constructs an expression to compare two expressions, evaluating to true if
-- the expressions do not evaluate to the same value.
(#!=#) :: Action -> Action -> Action
a1 #!=# a2 = Not (Compare EQ a1 a2)

-- | Constructs an expression to compare two expressions, evaluating to true if
-- the first expressions evaluates to a smaller value than the second.
(#<#) :: Action -> Action -> Action
a1 #<# a2 = Compare LT a1 a2

-- | Emits a "Buy" signal for a given position with a given urgency.
buy :: Position -> Urgency -> Action
buy pos urg = Emit (Buy pos urg)

-- | Emits a "Sell" signal for a given position with a given urgency.
sell :: Position -> Urgency -> Action
sell pos urg = Emit (Sell pos urg)

-- | Emits a "DoNothing" signal.
pass :: Action
pass = Emit DoNothing

-- | A literal for a given "Position".
pos :: Position -> Action
pos a = Lit (Position a)

-- | A literal for a given "Trend".
trnd :: Trend -> Action
trnd a = Lit (Trend a)
