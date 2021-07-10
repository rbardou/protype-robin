open Common

let rec encode_value: 'a. Id_set.t ref -> version: Protype.version ->
  'a Protype.t -> 'a -> Robin.Encode.value =
  fun (type a) id_set ~version (typ: a Protype.t) (value: a) ->
  let id_tag id =
    id_set := Id_set.add id !id_set;
    Id.tag id
  in
  let encode_id id = Robin.Encode.tag_int (id_tag id) in
  match typ with
    | Unit ->
        Robin.Encode.int 0
    | Bool ->
        Robin.Encode.bool value
    | Char ->
        Robin.Encode.int (Char.code value)
    | Int ->
        Robin.Encode.int value
    | Int32 ->
        Robin.Encode.int32 value
    | Int64 ->
        Robin.Encode.int64 value
    | Float ->
        Robin.Encode.float value
    | String ->
        Robin.Encode.string value
    | Option t ->
        (
          match value with
            | None ->
                Robin.Encode.int 0
            | Some value ->
                Robin.Encode.record_list [
                  Robin.Encode.field
                    (encode_id id_some)
                    (encode_value id_set ~version t value);
                ]
        )
    | Array t ->
        Robin.Encode.array (Array.map (encode_value id_set ~version t) value)
    | List t ->
        Robin.Encode.list (List.rev @@ List.rev_map (encode_value id_set ~version t) value)
    | Record { fields; _ } ->
        let rec loop: 'f. _ -> (a, 'f) Protype.fields -> _ =
          fun (type f) acc (fields: (a, f) Protype.fields) ->
            (* It could be tempting to not encode fields with type [Option]
               and with value [None]. Essentially it means they would have a default
               value of [None]. But if there is already a default value which is not
               [None], we cannot do that. We could restrict this optimization to
               fields which do not have a default value but if the user adds a default
               value later, old encoded values will no longer represent the same values
               and this may be surprising: the user may expect old [None] values
               to stay [None] if he didn't explicitely gave a default value of [None].
               This is also why we don't try to remove fields whose value is the default
               value: if the default value changes later, it will create surprising results.
               The use case for default values is not to make encoded values smaller but to
               ensure that we can still read old values where some fields did not exist. *)
            match fields with
              | R_field { id; typ; get; default = _; next } ->
                  let field =
                    Robin.Encode.field
                      (encode_id id)
                      (encode_value id_set ~version typ (get value))
                  in
                  loop (field :: acc) next
              | R_last { id; typ; get; default = _ } ->
                  let field =
                    Robin.Encode.field
                      (encode_id id)
                      (encode_value id_set ~version typ (get value))
                  in
                  Robin.Encode.record_list (List.rev (field :: acc))
        in
        loop [] fields
    | Enum { id; cases = _; rename = _ } ->
        Robin.Encode.int (id_tag (id value))
    | Variant { get; cases = _; rename = _ } ->
        let Protype.Value (case, value) = get value in
        (
          match case.typ with
            | Unit ->
                Robin.Encode.int (id_tag case.id)
            | _ ->
                Robin.Encode.record_list [
                  Robin.Encode.field
                    (encode_id case.id)
                    (encode_value id_set ~version case.typ value);
                ]
        )
    | Convert { typ; encode; decode = _ } ->
        encode_value id_set ~version typ (encode value)
    | Versions { old; from; current } ->
        if version >= from then
          encode_value id_set ~version current value
        else
          encode_value id_set ~version old value
    | Annotate (_, typ) ->
        encode_value id_set ~version typ value
    | Recursive f | Expanded_recursive (_, f) ->
        encode_value id_set ~version (f typ) value

let encode_full ~version typ value =
  let id_set = ref Id_set.empty in
  let encoded_value = encode_value id_set ~version typ value in
  let value = Robin.Encode.field (Robin.Encode.tag_int toplevel_value_tag) encoded_value in
  let id_table =
    if Id_set.is_empty !id_set then
      []
    else
      let add_id id acc =
        Robin.Encode.field
          (Robin.Encode.tag_int (Id.tag id))
          (Robin.Encode.string (Id.name id))
        :: acc
      in
      [
        Robin.Encode.field
          (Robin.Encode.tag_int toplevel_id_table_tag)
          (Robin.Encode.record_list (List.rev (Id_set.fold add_id !id_set [])));
      ]
  in
  let version =
    Robin.Encode.field
      (Robin.Encode.tag_int toplevel_version_tag)
      (Robin.Encode.int version)
  in
  Robin.Encode.record_list (version :: id_table @ value :: [])

let value ~version typ value out =
  Robin.Encode.output (encode_full ~version typ value) out

let to_string ~version typ value =
  Robin.Encode.to_string (encode_full ~version typ value)
