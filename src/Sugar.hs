module Sugar where

import Types

a1 #># a2 = Compare GT a1 a2
a1 #==# a2 = Compare EQ a1 a2
a1 #!=# a2 = Not (Compare EQ a1 a2)
a1 #<# a2 = Compare LT a1 a2
buy pos urg = Emit (Buy pos urg)
sell pos urg = Emit (Sell pos urg)
pass = Emit DoNothing
pos a = Lit (Position a)
trnd a = Lit (Trend a)
