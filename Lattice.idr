module Lattice

import Data.SortedSet
import Data.SortedMap

--- l is the type of the lattice elements,
interface Lattice l where
  lub : l -> l -> l 
  glb : l -> l -> l
  top : l
  bottom : l


interface Enumeratable a where 
  toList : List a
--------------------------------------------------
--- Flat lattice
--------------------------------------------------

data Flat : Type -> Type where
  MkFlat : (Eq a) => a -> Flat a
  FlatTop : Flat a
  FlatBottom : Flat a

implementation Lattice (Flat a) where
  lub f@(MkFlat _) FlatBottom = f
  lub FlatBottom f@(MkFlat _) = f
  lub f@(MkFlat v) (MkFlat w) = 
    if (v == w) then f else FlatTop
  lub _ _ = FlatTop
  glb _ _ = FlatBottom

  top = FlatTop
  bottom = FlatBottom

--------------------------------------------------
--- Powerset lattice
--------------------------------------------------

-- The elements of the powerset lattice are just
-- sets, so we will use the `SortedSet` datatype 
-- to represent this. The ordering is given by
-- the subset operation, as such the lub and glb
-- operators should reflect this ordering.

Powerset : Type -> Type 
Powerset a = SortedSet a

implementation (Enumeratable a, Ord a) => Lattice (Powerset a) where
  lub s1 s2 = union s1 s2
  glb s1 s2 = intersection s1 s2

  top = fromList $ toList
  bottom = SortedSet.empty

--------------------------------------------------
--- Map lattice
--------------------------------------------------

-- A map lattice maps a key K to a another 
-- lattice L. The semantics are that one map
-- is more specific than another when all its
-- elements are pointwise more specific than the 
-- elements of the other map.

Map : Type -> (v: Type) -> Type
Map k v = SortedMap k v

-- utility functionsa

pointwiseMap : Lattice v => (v -> v -> v) -> Map k v -> Map k v -> Map k v -> k -> Map k v
pointwiseMap f x y acc k = 
  insert k (f v1 v2) acc 
  where v1 = fromMaybe bottom (lookup k x)
        v2 = fromMaybe bottom (lookup k y)


implementation (Enumeratable k, Ord k, Lattice v) => Lattice (Map k v) where
  lub x y =  foldl (pointwiseMap lub x y) x (keys x)
  glb x y = foldl (pointwiseMap glb x y) x (keys x)
  top = SortedMap.fromList $ map (flip MkPair top) toList 
  bottom = SortedMap.fromList $ map (flip MkPair bottom) toList

--------------------------------------------------
---- Sign lattice
--------------------------------------------------

data Sign = Plus | Minus | Zero

implementation Eq Sign where
  (==) Plus Plus = True
  (==) Minus Minus = True
  (==) Zero Zero = True
  (==) _ _ = False

SignLattice : Type 
SignLattice = Flat Sign

