module Int_map = Map.Make (Int)

let pp_tag ~tags fmt (tag: Robin.Value.tag) =
  let f x = Format.fprintf fmt x in
  match tag with
    | Tag i ->
        (
          match Int_map.find_opt i tags with
            | None ->
                f "#%d" i
            | Some tag ->
                f "%s" tag
        )
    | Tag64 i -> f "#%Ld" i
    | Tag_large s -> f "#%S" s

let pp_list ~indent left right pp_item fmt list =
  let f x = Format.fprintf fmt x in
  match list with
    | [] ->
        f "%s%s" left right
    | [ head ] ->
        f "%s %a %s" left pp_item head right
    | head :: tail ->
        let indent_string = String.make (indent + 2) ' ' in
        f "%s\n%s" left indent_string;
        pp_item fmt head;
        List.iter (f ";\n%s%a" indent_string pp_item) tail;
        f ";\n%s%s" (String.make indent ' ') right

let hex_char_of_int = function
  | 0 -> '0'
  | 1 -> '1'
  | 2 -> '2'
  | 3 -> '3'
  | 4 -> '4'
  | 5 -> '5'
  | 6 -> '6'
  | 7 -> '7'
  | 8 -> '8'
  | 9 -> '9'
  | 10 -> 'a'
  | 11 -> 'b'
  | 12 -> 'c'
  | 13 -> 'd'
  | 14 -> 'e'
  | 15 -> 'f'
  | i -> invalid_arg ("hex_char_of_int " ^ string_of_int i)

let hex_of_string s =
  String.init (String.length s * 2) @@ fun i ->
  if i mod 2 = 0 then
    hex_char_of_int (Char.code s.[i / 2] lsr 4)
  else
    hex_char_of_int (Char.code s.[i / 2] land 0xF)

let rec pp_value ~indent ~tags fmt (value: Robin.Value.t) =
  let f x = Format.fprintf fmt x in
  match value with
    | UInt64 i ->
        if i < 0L then
          f "very large int64 (unsupported)"
        else (
          match Int_map.find_opt (Int64.to_int i) tags with
            | None ->
                f "%Ld" i
            | Some tag ->
                f "%Ld (%s)" i tag
        )
    | Int64 i ->
        f "%Ld" i
    | UIntLE s ->
        f "UIntLE %S" s
    | IntLE s ->
        f "IntLE %S" s
    | UIntBE s ->
        f "UIntBE %S" s
    | IntBE s ->
        f "IntBE %S" s
    | Bool b ->
        f "%b" b
    | Float3 c ->
        f "%C" c
    | Float x ->
        f "%f" x
    | Float_other s ->
        f "Float_other %S" s
    | String s ->
        if
          let length = String.length s in
          let bin_count =
            let c = ref 0 in
            for i = 0 to length - 1 do
              match s.[i] with
                | '\032' .. '\126' -> ()
                | _ -> incr c
            done;
            !c
          in
          bin_count > length / 2
        then
          f "%s" (hex_of_string s)
        else
          f "%S" s
    | Array a ->
        pp_value_list ~indent ~tags fmt (Array.to_list a)
    | List l ->
        pp_value_list ~indent ~tags fmt l
    | Record_array a ->
        pp_value_record ~indent ~tags fmt (Array.to_list a)
    | Record_list l ->
        pp_value_record ~indent ~tags fmt l

and pp_value_list ~indent ~tags fmt l =
  pp_list ~indent "[" "]" (pp_value ~indent: (indent + 2) ~tags) fmt l

and pp_value_record ~indent ~tags fmt l =
  pp_list ~indent "{" "}"
    (fun fmt (tag, v) ->
       Format.fprintf fmt "%a = %a" (pp_tag ~tags)
         tag (pp_value ~indent: (indent + 2) ~tags) v)
    fmt l

let pp_value fmt value =
  let not_protype () = Int_map.empty, value in
  let tags, value =
    let from_record l =
      let id_table = List.assoc_opt (Robin.Value.Tag 1) l in
      let value = List.assoc_opt (Robin.Value.Tag 2) l in
      match value with
        | None ->
            not_protype ()
        | Some value ->
            let from_record l =
              let add_tag acc (tag, v) =
                match tag, v with
                  | Robin.Value.Tag i, Robin.Value.String s ->
                      Int_map.add i s acc
                  | _ ->
                      acc
              in
              let tags =
                List.fold_left add_tag
                  (
                    Int_map.empty
                    |> Int_map.add 0 "version"
                    |> Int_map.add 1 "id_table"
                    |> Int_map.add 2 "value"
                  )
                  l
              in
              tags, value
            in
            match id_table with
              | Some (Robin.Value.Record_array a) ->
                  from_record (Array.to_list a)
              | Some (Record_list l) ->
                  from_record l
              | Some _ ->
                  not_protype ()
              | None ->
                  Int_map.empty, value
    in
    match value with
      | Record_array a ->
          from_record (Array.to_list a)
      | Record_list l ->
          from_record l
      | _ ->
          not_protype ()
  in
  pp_value ~indent: 0 ~tags fmt value

let pp_string fmt string =
  match Robin.Decode.from_string string with
    | Error e ->
        Format.pp_print_string fmt (Robin.Decode.show_error e)
    | Ok value ->
        pp_value fmt value
