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
  include module type of struct
    include Core.Date
  end

  val roundtrippable_arg_type : Core.Date.t Roundtrippable_arg_type.t
end

module Percent : sig
  include module type of struct
    include Core.Percent
  end

  val roundtrippable_arg_type : Core.Percent.t Roundtrippable_arg_type.t
end

module Host_and_port : sig
  include module type of struct
    include Core.Host_and_port
  end

  val roundtrippable_arg_type : Core.Host_and_port.t Roundtrippable_arg_type.t
end

module Time_ns : sig
  include module type of struct
      include Core.Time_ns
    end
    with module Span := Core.Time_ns.Span

  module Span : sig
    include module type of struct
      include Core.Time_ns.Span
    end

    val roundtrippable_arg_type : Core.Time_ns.Span.t Roundtrippable_arg_type.t
  end
end

module Filename : sig
  include module type of struct
    include Core.Filename
  end

  val roundtrippable_arg_type : Core.Filename.t Roundtrippable_arg_type.t
end

module File_path : sig
  include module type of struct
    include File_path
  end

  val roundtrippable_arg_type : File_path.t Roundtrippable_arg_type.t
end
