(** ['a Or_default.t] is a type that is isomorphic to ['a Option.t]. It is intended to be
    used with roundtrippable command parameters. *)

open! Core

(* $MDX part-begin=type_definition *)

(** A type that is isomorphic to ['a Option.t]. It is intended to be used with
    roundtrippable command parameters. *)
type 'a t =
  | Custom of 'a
  | Default
[@@deriving compare ~localize, equal ~localize, sexp, sexp_grammar]
(* $MDX part-end *)

val return : 'a -> 'a t
val of_option : 'a option -> 'a t
val to_option : 'a t -> 'a option
val is_custom : _ t -> bool
val is_default : _ t -> bool

(** Equivalent to [Option.value]. *)
val resolve : 'a t -> default:'a -> 'a

(** The ['a t Roundtrippable_command_param.t] that this function returns will produce
    [Default] if the flag is not passed. The result of parsing the command line parameters
    should be passed into [resolve ~default] with the same value of [~default] that was
    passed into this function. *)
val create_optional_param_with_default_doc
  :  ?aliases:string list
  -> string
  -> 'a Command.Arg_type.t
  -> default:'a
       (** This value is only used in the doc string. If the flag is not passed, the value
           will be [Default]. *)
  -> to_string:('a -> string)
  -> doc:string
  -> 'a t Roundtrippable_command_param.t

(** Similar to [create_optional_param_with_default_doc], but a
    [Roundtrippable_command_param.T2.t] is returned. *)
val create_optional_param_with_default_doc'
  :  ?aliases:string list
  -> string
  -> 'a Command.Arg_type.t
  -> default:'a
       (** This value is only used in the doc string. If the flag is not passed, the value
           will be [Default]. *)
  -> to_string:('a -> string)
  -> doc:string
  -> ('a, 'a t) Roundtrippable_command_param.T2.t

(** Similar to [create_optional_param_with_default_doc], but the allowed values are
    displayed in the doc string. *)
val create_optional_param_with_default_doc_from_enum
  :  ?represent_choice_with:string
  -> ?list_values_in_help:bool
  -> ?aliases:string list
  -> ?key:'a Univ_map.Multi.Key.t
  -> string
  -> 'a Enum.t
  -> default:'a
       (** This value is only used in the doc string. If the flag is not passed, the value
           with be [Default]. *)
  -> doc:string
  -> 'a t Roundtrippable_command_param.t

(** Similar to [create_optional_param_with_default_doc_from_enum], but a
    [Roundtrippable_command_param.T2.t] is returned. *)
val create_optional_param_with_default_doc_from_enum'
  :  ?represent_choice_with:string
  -> ?list_values_in_help:bool
  -> ?aliases:string list
  -> ?key:'a Univ_map.Multi.Key.t
  -> string
  -> 'a Enum.t
  -> default:'a
       (** This value is only used in the doc string. If the flag is not passed, the value
           with be [Default]. *)
  -> doc:string
  -> ('a, 'a t) Roundtrippable_command_param.T2.t

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t
    [@@deriving bin_io, compare ~localize, equal ~localize, sexp, stable_witness]
  end
end
