open Common

(* These toplevel identifiers are not actually used as identifiers
   since there is no id table at this level, but it is convenient in
   error locations to have actual names. *)
let toplevel_id_version = Id.make "version"
let toplevel_id_id_table = Id.make "id_table"
let toplevel_id_value = Id.make "value"

type error_kind =
  | Robin_error of Robin.Decode.error
  | Invalid_toplevel
  | Invalid_char of int
  | Unsupported_large_tag of string
  | Unsupported_large_int of string
  | Unsupported_float_size of int
  | Unknown_tag of int
  | Incompatible_types of string * string
  | Unknown_field of Id.t
  | Unknown_enum of Id.t
  | Unknown_variant of Id.t
  | Missing_field of Id.t
  | Cannot_convert

let show_error_kind = function
  | Robin_error error -> Robin.Decode.show_error error
  | Invalid_toplevel -> "invalid toplevel"
  | Invalid_char i -> "invalid char code: " ^ string_of_int i
  | Unsupported_large_tag s -> "unsupported large tag: \"" ^ String.escaped s ^ "\""
  | Unsupported_large_int s -> "unsupported large int: \"" ^ String.escaped s ^ "\""
  | Unsupported_float_size i -> "unsupported float size: " ^ string_of_int i
  | Unknown_tag i -> "unknown tag: " ^ string_of_int i
  | Incompatible_types (expected, found) ->
      "incompatible types (expected " ^ expected ^ ", found " ^ found ^ ")"
  | Unknown_field id -> "unknown field: " ^ Id.name id
  | Unknown_enum id -> "unknown enum: " ^ Id.name id
  | Unknown_variant id -> "unknown variant: " ^ Id.name id
  | Missing_field id -> "missing field: " ^ Id.name id
  | Cannot_convert -> "cannot convert"

type error =
  {
    location: Protype.location;
    error: error_kind;
  }

let show_error { location; error } =
  Protype.show_location location ^ ": " ^ show_error_kind error

exception Error of error

let () =
  Printexc.register_printer @@ function
  | Error error -> Some (show_error error)
  | _ -> None

let error error = raise (Error { location = This; error })

let incompatible ~expected found =
  error (Incompatible_types (expected, found))

let is_tag tag (field_tag: Robin.Value.tag) =
  match field_tag with
    | Tag x -> x = tag
    | Tag64 x -> x = Int64.of_int tag
    | Tag_large x -> error (Unsupported_large_tag x)

let int_of_tag (field_tag: Robin.Value.tag) =
  match field_tag with
    | Tag x -> x
    | Tag64 x -> Int64.to_int x (* TODO: handle overflows? *)
    | Tag_large x -> error (Unsupported_large_tag x)

let decode_int_tag ?(rename = Id_map.empty) id_table tag =
  match Int_map.find_opt tag id_table with
    | None -> error (Unknown_tag tag)
    | Some id ->
        match Id_map.find_opt id rename with
          | None -> id
          | Some id -> id

let decode_tag ?rename id_table tag =
  decode_int_tag ?rename id_table (int_of_tag tag)

let get_field_from_array tag (fields: (Robin.Value.tag * Robin.Value.t) array) =
  let rec loop n =
    if n >= Array.length fields then
      None
    else
      let field_tag, value = fields.(n) in
      if is_tag tag field_tag then
        Some value
      else
        loop (n + 1)
  in
  loop 0

let get_field_from_list tag fields =
  List.find_map
    (fun (field_tag, value) -> if is_tag tag field_tag then Some value else None)
    fields

let with_location update f =
  try
    f ()
  with Error { location; error } ->
    raise (Error { location = update location; error })

let decode_unit (value: Robin.Value.t) =
  match value with
    | UInt64 0L | Int64 0L ->
        ()
    | UInt64 _ | Int64 _ ->
        incompatible ~expected: "unit" "non-zero int"
    | UIntLE i | IntLE i | UIntBE i | IntBE i ->
        error (Unsupported_large_int i)
    | Bool _ ->
        incompatible ~expected: "unit" "bool"
    | Float3 _ | Float _ | Float_other _ ->
        incompatible ~expected: "unit" "float"
    | String _ ->
        incompatible ~expected: "unit" "string"
    | Array _ | List _ ->
        incompatible ~expected: "unit" "array"
    | Record_array _ | Record_list _ ->
        incompatible ~expected: "unit" "record"

let decode_bool (value: Robin.Value.t) =
  match value with
    | UInt64 _ | Int64 _ | UIntLE _ | IntLE _ | UIntBE _ | IntBE _ ->
        incompatible ~expected: "bool" "int"
    | Bool b ->
        b
    | Float3 _ | Float _ | Float_other _ ->
        incompatible ~expected: "bool" "float"
    | String _ ->
        incompatible ~expected: "bool" "string"
    | Array _ | List _ ->
        incompatible ~expected: "bool" "array"
    | Record_array _ | Record_list _ ->
        incompatible ~expected: "bool" "record"

let decode_int (value: Robin.Value.t) =
  match value with
    | UInt64 i ->
        if i > Int64.of_int max_int || i < 0L then
          incompatible ~expected: "int" "int64"
        else
          Int64.to_int i
    | Int64 i ->
        if i > Int64.of_int max_int || i < Int64.of_int min_int then
          incompatible ~expected: "int" "int64"
        else
          Int64.to_int i
    | UIntLE i | IntLE i | UIntBE i | IntBE i ->
        error (Unsupported_large_int i)
    | Bool _ ->
        incompatible ~expected: "int" "bool"
    | Float3 _ | Float _ | Float_other _ ->
        incompatible ~expected: "int" "float"
    | String _ ->
        incompatible ~expected: "int" "string"
    | Array _ | List _ ->
        incompatible ~expected: "int" "array"
    | Record_array _ | Record_list _ ->
        incompatible ~expected: "int" "record"

let decode_int32 (value: Robin.Value.t) =
  match value with
    | UInt64 i when Int64.logand i 0xFFFF_FFFF_0000_0000L = 0L ->
        Int64.to_int32 i
    | Int64 i when Int64.logand i 0xFFFF_FFFF_0000_0000L = 0L ->
        Int64.to_int32 i
    | Int64 i when Int64.logand i 0xFFFF_FFFF_0000_0000L = 0xFFFF_FFFF_0000_0000L ->
        Int64.to_int32 i
    | UInt64 _ | Int64 _ ->
        incompatible ~expected: "int32" "int64"
    | UIntLE i | IntLE i | UIntBE i | IntBE i ->
        error (Unsupported_large_int i)
    | Bool _ ->
        incompatible ~expected: "int32" "bool"
    | Float3 _ | Float _ | Float_other _ ->
        incompatible ~expected: "int32" "float"
    | String _ ->
        incompatible ~expected: "int32" "string"
    | Array _ | List _ ->
        incompatible ~expected: "int32" "array"
    | Record_array _ | Record_list _ ->
        incompatible ~expected: "int32" "record"

let decode_int64 (value: Robin.Value.t) =
  match value with
    | UInt64 i | Int64 i ->
        i
    | UIntLE i | IntLE i | UIntBE i | IntBE i ->
        error (Unsupported_large_int i)
    | Bool _ ->
        incompatible ~expected: "int64" "bool"
    | Float3 _ | Float _ | Float_other _ ->
        incompatible ~expected: "int64" "float"
    | String _ ->
        incompatible ~expected: "int64" "string"
    | Array _ | List _ ->
        incompatible ~expected: "int64" "array"
    | Record_array _ | Record_list _ ->
        incompatible ~expected: "int64" "record"

let decode_float (value: Robin.Value.t) =
  match value with
    | UInt64 _ | Int64 _ | UIntLE _ | IntLE _ | UIntBE _ | IntBE _ ->
        incompatible ~expected: "float" "int"
    | Bool _ ->
        incompatible ~expected: "float" "bool"
    | Float3 _ ->
        error (Unsupported_float_size 3)
    | Float f ->
        f
    | Float_other s ->
        error (Unsupported_float_size (String.length s * 8))
    | String _ ->
        incompatible ~expected: "float" "string"
    | Array _ | List _ ->
        incompatible ~expected: "float" "array"
    | Record_array _ | Record_list _ ->
        incompatible ~expected: "float" "record"

let decode_string (value: Robin.Value.t) =
  match value with
    | UInt64 _ | Int64 _ | UIntLE _ | IntLE _ | UIntBE _ | IntBE _ ->
        incompatible ~expected: "string" "int"
    | Bool _ ->
        incompatible ~expected: "string" "bool"
    | Float3 _ | Float _ | Float_other _ ->
        incompatible ~expected: "string" "float"
    | String s ->
        s
    | Array _ | List _ ->
        incompatible ~expected: "string" "array"
    | Record_array _ | Record_list _ ->
        incompatible ~expected: "string" "record"

let rec decode_option: 'a. ignore_unknown_fields: bool -> version: Protype.version ->
  _ -> 'a Protype.t -> _ -> 'a option =
  fun (type a) ~ignore_unknown_fields ~version id_table
    (typ: a Protype.t) (value: Robin.Value.t): a option ->
    match value with
      | UInt64 0L | Int64 0L ->
          None
      | UInt64 _ | Int64 _ ->
          incompatible ~expected: "unit" "non-zero int"
      | UIntLE i | IntLE i | UIntBE i | IntBE i ->
          error (Unsupported_large_int i)
      | Bool _ ->
          incompatible ~expected: "option" "bool"
      | Float3 _ | Float _ | Float_other _ ->
          incompatible ~expected: "option" "float"
      | String _ ->
          incompatible ~expected: "option" "string"
      | Array _ | List _ ->
          incompatible ~expected: "option" "array"
      | Record_array [| tag, value |] | Record_list [ tag, value ] ->
          let id = decode_tag id_table tag in
          if Id.compare id id_some <> 0 then
            incompatible ~expected: "option" "record"
          else
            with_location (fun x -> Option_value x) @@ fun () ->
            Some (decode_value ~ignore_unknown_fields ~version id_table typ value)
      | Record_array _ | Record_list _ ->
          incompatible ~expected: "option" "record"

and decode_array: 'a. ignore_unknown_fields: bool -> version: Protype.version ->
  _ -> 'a Protype.t -> _ -> 'a array =
  fun (type a) ~ignore_unknown_fields ~version id_table
    (typ: a Protype.t) (value: Robin.Value.t): a array ->
    match value with
      | UInt64 _ | Int64 _ | UIntLE _ | IntLE _ | UIntBE _ | IntBE _ ->
          incompatible ~expected: "array" "int"
      | Bool _ ->
          incompatible ~expected: "array" "bool"
      | Float3 _ | Float _ | Float_other _ ->
          incompatible ~expected: "array" "float"
      | String _ ->
          incompatible ~expected: "array" "string"
      | Array a ->
          (* TODO: [Array_item] could take the index as parameter. *)
          with_location (fun x -> Array_item x) @@ fun () ->
          Array.map
            (decode_value ~ignore_unknown_fields ~version id_table typ)
            a
      | List l ->
          (* TODO: [Array_item] could take the index as parameter. *)
          with_location (fun x -> Array_item x) @@ fun () ->
          Array.map
            (decode_value ~ignore_unknown_fields ~version id_table typ)
            (Array.of_list l)
      | Record_array _ | Record_list _ ->
          incompatible ~expected: "array" "record"

and decode_list: 'a. ignore_unknown_fields: bool -> version: Protype.version ->
  _ -> 'a Protype.t -> _ -> 'a list =
  fun (type a) ~ignore_unknown_fields ~version id_table
    (typ: a Protype.t) (value: Robin.Value.t): a list ->
    match value with
      | UInt64 _ | Int64 _ | UIntLE _ | IntLE _ | UIntBE _ | IntBE _ ->
          incompatible ~expected: "list" "int"
      | Bool _ ->
          incompatible ~expected: "list" "bool"
      | Float3 _ | Float _ | Float_other _ ->
          incompatible ~expected: "list" "float"
      | String _ ->
          incompatible ~expected: "list" "string"
      | Array a ->
          (* TODO: [List_item] could take the index as parameter. *)
          with_location (fun x -> List_item x) @@ fun () ->
          Array.map (decode_value ~ignore_unknown_fields ~version id_table typ) a
          |> Array.to_list
      | List l ->
          (* TODO: [List_item] could take the index as parameter. *)
          with_location (fun x -> List_item x) @@ fun () ->
          List.rev @@
          List.rev_map (decode_value ~ignore_unknown_fields ~version id_table typ) l
      | Record_array _ | Record_list _ ->
          incompatible ~expected: "list" "record"

and decode_record: 'r 'f. ignore_unknown_fields: bool -> version: Protype.version ->
  Id.t Int_map.t -> ('r, 'f) Protype.fields -> 'f -> Id_set.t ->
  Id.t Id_map.t -> Robin.Value.t -> 'r =
  fun (type r) (type f) ~ignore_unknown_fields ~version id_table
    (fields: (r, f) Protype.fields) (make: f) can_ignore rename (value: Robin.Value.t) ->
    let add_field_to_value_map acc (tag, value) =
      let id = decode_int_tag ~rename id_table (int_of_tag tag) in
      Id_map.add id value acc
    in
    let rec decode_fields: 'f. Robin.Value.t Id_map.t -> (r, 'f) Protype.fields -> 'f -> r =
      fun (type f) value_map (fields: (r, f) Protype.fields) (make: f) ->
        let decode_field id typ default =
          match Id_map.find_opt id value_map with
            | None ->
                (
                  match default with
                    | None -> error (Missing_field id)
                    | Some x -> x, value_map
                )
            | Some value ->
                with_location (fun x -> Record_field (id, x)) @@ fun () ->
                decode_value ~ignore_unknown_fields ~version id_table typ value,
                Id_map.remove id value_map
        in
        match fields with
          | R_field { id; typ; get = _; default; next } ->
              let field_value, value_map = decode_field id typ default in
              decode_fields value_map next (make field_value)
          | R_last { id; typ; get = _; default } ->
              let field_value, value_map = decode_field id typ default in
              if not ignore_unknown_fields then (
                let check id _ =
                  if not (Id_set.mem id can_ignore) then
                    error (Unknown_field id)
                in
                Id_map.iter check value_map;
              );
              make field_value
    in
    match value with
      | UInt64 _ | Int64 _ | UIntLE _ | IntLE _ | UIntBE _ | IntBE _ ->
          incompatible ~expected: "record" "int"
      | Bool _ ->
          incompatible ~expected: "record" "bool"
      | Float3 _ | Float _ | Float_other _ ->
          incompatible ~expected: "record" "float"
      | String _ ->
          incompatible ~expected: "record" "string"
      | Array _ | List _ ->
          incompatible ~expected: "record" "array"
      | Record_array a ->
          let value_map = Array.fold_left add_field_to_value_map Id_map.empty a in
          decode_fields value_map fields make
      | Record_list l ->
          let value_map = List.fold_left add_field_to_value_map Id_map.empty l in
          decode_fields value_map fields make

and decode_enum: 'a. _ -> 'a Id_map.t -> _ -> _ -> 'a =
  fun (type a) id_table (cases: a Id_map.t) rename (value: Robin.Value.t) ->
  match value with
    | UInt64 i | Int64 i ->
        (* TODO: what if i is too large for an int? *)
        let id = decode_int_tag ~rename id_table (Int64.to_int i) in
        (
          match Id_map.find_opt id cases with
            | None -> error (Unknown_enum id)
            | Some x -> x
        )
    | UIntLE i | IntLE i | UIntBE i | IntBE i ->
        error (Unsupported_large_int i)
    | Bool _ ->
        incompatible ~expected: "enum" "bool"
    | Float3 _ | Float _ | Float_other _ ->
        incompatible ~expected: "enum" "float"
    | String _ ->
        incompatible ~expected: "enum" "string"
    | Array _ | List _ ->
        incompatible ~expected: "enum" "array"
    | Record_array _ | Record_list _ ->
        incompatible ~expected: "enum" "record"

and decode_variant: 'a. ignore_unknown_fields: _ -> version: Protype.version ->
  _ -> 'a Protype.packed_case Id_map.t -> _ -> _ -> 'a =
  fun (type a) ~ignore_unknown_fields ~version id_table
    (cases: a Protype.packed_case Id_map.t)
    rename (value: Robin.Value.t) ->
    match value with
      | UInt64 i | Int64 i ->
          let id = decode_int_tag ~rename id_table (Int64.to_int i) in
          (
            match Id_map.find_opt id cases with
              | None ->
                  error (Unknown_enum id)
              | Some (Case { typ; make; _ }) ->
                  match typ with
                    | Unit ->
                        make ()
                    | _ ->
                        incompatible ~expected: "variant" "int"
          )
      | UIntLE i | IntLE i | UIntBE i | IntBE i ->
          error (Unsupported_large_int i)
      | Bool _ ->
          incompatible ~expected: "variant" "bool"
      | Float3 _ | Float _ | Float_other _ ->
          incompatible ~expected: "variant" "float"
      | String _ ->
          incompatible ~expected: "variant" "string"
      | Array _ | List _ ->
          incompatible ~expected: "variant" "array"
      | Record_array [| tag, parameter |] | Record_list [ tag, parameter ] ->
          let id = decode_tag ~rename id_table tag in
          with_location (fun x -> Variant_parameter (id, x)) @@ fun () ->
          (
            match Id_map.find_opt id cases with
              | None ->
                  error (Unknown_variant id)
              | Some (Case { id = _; typ; make }) ->
                  make (decode_value ~ignore_unknown_fields ~version id_table typ parameter)
          )
      | Record_array _ | Record_list _ ->
          incompatible ~expected: "variant" "record"

and decode_value: 'a. ignore_unknown_fields: bool -> version: Protype.version ->
  Id.t Int_map.t -> 'a Protype.t -> Robin.Value.t -> 'a =
  fun (type a) ~ignore_unknown_fields ~version id_table (typ: a Protype.t) value ->
  match typ with
    | Unit -> (decode_unit value: a)
    | Bool -> decode_bool value
    | Char ->
        let i = decode_int value in
        if i < 0 || i > 255 then error (Invalid_char i) else Char.chr i
    | Int -> decode_int value
    | Int32 -> decode_int32 value
    | Int64 -> decode_int64 value
    | Float -> decode_float value
    | String -> decode_string value
    | Option t -> decode_option ~ignore_unknown_fields ~version id_table t value
    | Array t -> decode_array ~ignore_unknown_fields ~version id_table t value
    | List t -> decode_list ~ignore_unknown_fields ~version id_table t value
    | Record { fields; make; can_ignore; rename } ->
        decode_record ~ignore_unknown_fields ~version id_table
          fields make can_ignore rename value
    | Enum { cases; id = _; rename } -> decode_enum id_table cases rename value
    | Variant { cases; get = _; rename } ->
        decode_variant ~ignore_unknown_fields ~version id_table cases rename value
    | Convert { typ; decode; encode = _ } ->
        (
          match decode (decode_value ~ignore_unknown_fields ~version id_table typ value) with
            | Some x -> x
            | None -> error Cannot_convert
        )
    | Versions { old; from; current } ->
        let typ = if version < from then old else current in
        decode_value ~ignore_unknown_fields ~version id_table typ value

let add_tag_id acc ((tag: Robin.Value.tag), (id: Robin.Value.t)) =
  match id with
    | UInt64 _ | Int64 _ | UIntLE _ | IntLE _ | UIntBE _ | IntBE _ ->
        incompatible ~expected: "id name" "int"
    | Bool _ ->
        incompatible ~expected: "id name" "bool"
    | Float3 _ | Float _ | Float_other _ ->
        incompatible ~expected: "id name" "float"
    | Array _ | List _ ->
        incompatible ~expected: "id name" "array"
    | Record_list _ | Record_array _ ->
        incompatible ~expected: "id name" "record"
    | String name ->
        Int_map.add (int_of_tag tag) (Id.make name) acc

let decode_toplevel_record ~ignore_unknown_fields typ (version: Robin.Value.t option)
    (id_table: Robin.Value.t option) (value: Robin.Value.t option) =
  let version =
    match version with
      | None ->
          0
      | Some version ->
          with_location (fun x -> Record_field (toplevel_id_version, x)) @@ fun () ->
          (* TODO: what if version is too large for an int? *)
          decode_int64 version |> Int64.to_int
  in
  let id_table =
    match id_table with
      | None ->
          Int_map.empty
      | Some id_table ->
          with_location (fun x -> Record_field (toplevel_id_id_table, x)) @@ fun () ->
          match id_table with
            | UInt64 _ | Int64 _ | UIntLE _ | IntLE _ | UIntBE _ | IntBE _ ->
                incompatible ~expected: "id table" "int"
            | Bool _ ->
                incompatible ~expected: "id table" "bool"
            | Float3 _ | Float _ | Float_other _ ->
                incompatible ~expected: "id table" "float"
            | String _ ->
                incompatible ~expected: "id table" "string"
            | Array _ | List _ ->
                incompatible ~expected: "id table" "array"
            | Record_array fields ->
                Array.fold_left add_tag_id Int_map.empty fields
            | Record_list fields ->
                List.fold_left add_tag_id Int_map.empty fields
  in
  match value with
    | None ->
        error (Missing_field toplevel_id_value)
    | Some value ->
        with_location (fun x -> Record_field (toplevel_id_value, x)) @@ fun () ->
        decode_value ~ignore_unknown_fields ~version id_table typ value

let decode_toplevel_exn ~ignore_unknown_fields typ (value: Robin.Value.t) =
  match value with
    | UInt64 _ | Int64 _ | UIntLE _ | IntLE _ | UIntBE _ | IntBE _
    | Bool _ | Float3 _ | Float _ | Float_other _ | String _ | Array _ | List _ ->
        error Invalid_toplevel
    | Record_array fields ->
        decode_toplevel_record ~ignore_unknown_fields typ
          (get_field_from_array toplevel_version_tag fields)
          (get_field_from_array toplevel_id_table_tag fields)
          (get_field_from_array toplevel_value_tag fields)
    | Record_list fields ->
        decode_toplevel_record ~ignore_unknown_fields typ
          (get_field_from_list toplevel_version_tag fields)
          (get_field_from_list toplevel_id_table_tag fields)
          (get_field_from_list toplevel_value_tag fields)

let decode_toplevel ~ignore_unknown_fields typ value =
  try
    Ok (decode_toplevel_exn ~ignore_unknown_fields typ value)
  with Error e ->
    Error e

let value ?(ignore_unknown_fields = false) typ read =
  match Robin.Decode.input read with
    | Ok value ->
        decode_toplevel ~ignore_unknown_fields typ value
    | Error e ->
        Error { location = This; error = Robin_error e }

let from_string ?(ignore_unknown_fields = false) typ string =
  match Robin.Decode.from_string string with
    | Ok value ->
        decode_toplevel ~ignore_unknown_fields typ value
    | Error e ->
        Error { location = This; error = Robin_error e }
