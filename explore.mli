(** Pretty-printer for Robin values using Protype conventions to display tags. *)

(** This module provides a way to explore values of unknown types for debugging. *)

(** Pretty-print a value of unknown type. *)
val pp_value: Format.formatter -> Robin.Value.t -> unit

(** Decode a string and pretty-print it. *)
val pp_string: Format.formatter -> string -> unit
