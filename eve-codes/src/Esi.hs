module Esi where
 
type RegionId = Integer
type SystemId = Integer
type LocationId = Integer
type TypeId = Integer

data EsiSource = Tranquility | Singularity
instance Show EsiSource where
  show Singularity = "singularity"
  show Tranquility = "tranquility"

data EsiVersion = Latest
instance Show EsiVersion where
  show Latest = "latest"
