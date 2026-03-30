open! Core

(** Primitive types we provide basic support for: [string], [int], [char], [float],
    [bool], [sexp] *)

val roundtrippable_arg_type_bool : bool Roundtrippable_arg_type.t
val roundtrippable_arg_type_string : string Roundtrippable_arg_type.t
val roundtrippable_arg_type_int : int Roundtrippable_arg_type.t
val roundtrippable_arg_type_char : char Roundtrippable_arg_type.t
val roundtrippable_arg_type_float : float Roundtrippable_arg_type.t
val roundtrippable_arg_type_sexp : Sexp.t Roundtrippable_arg_type.t

(** Other common types we support. *)

module Date : sig
  val roundtrippable_arg_type : Date.t Roundtrippable_arg_type.t
end

module Percent : sig
  val roundtrippable_arg_type : Percent.t Roundtrippable_arg_type.t
end

module Host_and_port : sig
  val roundtrippable_arg_type : Host_and_port.t Roundtrippable_arg_type.t
end

module Time_ns : sig
  module Span : sig
    val roundtrippable_arg_type : Time_ns.Span.t Roundtrippable_arg_type.t
  end
end

module Filename : sig
  val roundtrippable_arg_type : Filename.t Roundtrippable_arg_type.t
end

module File_path : sig
  val roundtrippable_arg_type : File_path.t Roundtrippable_arg_type.t
end
