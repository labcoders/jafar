module Programs where

import Types
import Sugar

emaProg = If (GetPosition #==# (pos Long))
             (If (GetPrice #># GetStopLoss)
                 (If (GetTrend #==# (trnd UpTrend))
                     pass
                     (sell Long NotUrgent))
                 (sell Long Urgent))
             (If (GetPosition #==# (pos Short))
                 (If (GetPrice #<# GetStopLoss)
                     (If (GetTrend #==# (trnd DownTrend))
                         pass
                         (buy Short NotUrgent))
                     (buy Short Urgent))
                 (If (GetTrend #!=# GetCrossover)
                     (If (GetCrossover #==# (trnd UpTrend))
                         (buy Long NotUrgent)
                         (sell Short NotUrgent))
                     pass))

