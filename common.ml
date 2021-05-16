module Int_map = Map.Make (Int)
module Id = Protype.Id
module Id_set = Protype.Id_set
module Id_map = Protype.Id_map

let id_some = Id.make "Some"
let toplevel_version_tag = 0
let toplevel_id_table_tag = 1
let toplevel_value_tag = 2
