(** Binary encoding of values in Robin format. *)

(** Encode a value.

    For instance, [encode_string print_string ~version: 0 Protype.int 42]
    will output a binary representation of 42 on the standard output.
    The [version] is also encoded, and in the general case
    a table of identifiers is encoded too. *)
val value: version: Protype.version -> 'a Protype.t -> 'a -> (string -> unit) -> unit

(** Same as [encode], but encode to a string. *)
val to_string: version: Protype.version -> 'a Protype.t -> 'a -> string
