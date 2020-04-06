module Analysis.Sign 

import Lattice

--------------------------------------------------
---- Sign lattice
--------------------------------------------------

data Sign = Plus | Minus | Zero

implementation Enumeratable Sign where
  toList = [Plus, Minus, Zero]
  indexOf Plus  = 0
  indexOf Minus = 1
  indexOf Zero  = 2

SignLattice : Type 
SignLattice = Flat Sign
