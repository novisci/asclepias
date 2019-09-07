module Hasklepias.Events.EventsSpec where

import Hasklepias.IntervalAlgebra
import Hasklepias.Events
import Hasklepias.Events.MedicalDomain

y = eventContext ( domain $ Insurance "x" "y" )  Nothing
x1 = event ( period 4 5 ) y
x2 = event ( period 5 6 ) y

z = events [x1, x2]