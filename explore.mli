(** Pretty-printer for Robin values using Protype conventions to display tags. *)

(** This module provides a way to explore values of unknown types for debugging. *)

(** Pretty-print a value of unknown type (general version). *)
val pp_value_gen:
  ?custom: (
    indent: int ->
    max_depth: int option ->
    Format.formatter ->
    Robin.Value.t ->
    bool
  ) ->
  ?indent: int ->
  ?max_depth: int ->
  unit ->
  Format.formatter -> Robin.Value.t -> unit

(** Pretty-print a value of unknown type (simple version). *)
val pp_value: Format.formatter -> Robin.Value.t -> unit

(** Decode a string and pretty-print it (general version). *)
val pp_string_gen:
  ?custom: (
    indent: int ->
    max_depth: int option ->
    Format.formatter ->
    Robin.Value.t ->
    bool
  ) ->
  ?indent: int ->
  ?max_depth: int ->
  unit ->
  Format.formatter -> string -> unit

(** Decode a string and pretty-print it (simple version). *)
val pp_string: Format.formatter -> string -> unit
