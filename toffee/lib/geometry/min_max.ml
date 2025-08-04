type ('min, 'max) t = { min : 'min; max : 'max }

(* Creation *)

let make min max = { min; max }

(* Mapping *)

let map f min_max = { min = f min_max.min; max = f min_max.max }
let map_min f min_max = { min_max with min = f min_max.min }
let map_max f min_max = { min_max with max = f min_max.max }
