(** Binary decoding of values in Robin format. *)

(** Error messages for type errors that can occur when decoding. *)
type error_kind =
  | Robin_error of Robin.Decode.error
  | Invalid_toplevel
  | Invalid_char of int
  | Unsupported_large_tag of string
  | Unsupported_large_int of string
  | Unsupported_float_size of int
  | Unknown_tag of int
  | Incompatible_types of string * string (** expected type, actual type *)
  | Unknown_field of Protype.Id.t
  | Unknown_enum of Protype.Id.t
  | Unknown_variant of Protype.Id.t
  | Missing_field of Protype.Id.t
  | Cannot_convert (** a [Protype.Convert] decode function returned [None] *)

(** Convert an error kind to text. *)
val show_error_kind: error_kind -> string

(** Errors that can occur when decoding. *)
type error =
  {
    location: Protype.location;
    error: error_kind;
  }

(** Convert an error to text. *)
val show_error: error -> string

(** Decode a value.

    If [ignore_unknown_fields] is [true], all unknown record fields are ignored.
    If it is [false], record fields that are in the [can_ignore] set of the record type
    are ignored, but other fields cause [Unknown_field] errors.
    Default is [false].

    The function is given to [Robin.Decode] and follows the same conventions. *)
val value:
  ?ignore_unknown_fields: bool -> 'a Protype.t -> (int -> string) -> ('a, error) result

(** Same as [decode], but decode from a string.

    For instance, [from_string Protype.int (encode_string ~version: 0 Protype.int 42)]
    returns (Ok 42). *)
val from_string:
  ?ignore_unknown_fields: bool -> 'a Protype.t -> string -> ('a, error) result
