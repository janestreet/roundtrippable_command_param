open! Base
open! Ppxlib

(** Uses [Roundtrippable_command_param.create_required] to assemble *)
module Required_param : sig
  type t =
    { roundtrippable_arg_type : expression
    ; flag_name : expression
    ; doc : expression
    ; open_runtime_lib : bool
    ; loc : Location.t
    }
end

(** Uses [Roundtrippable_command_param.create_optional] to assemble *)
module Optional_param : sig
  type t =
    { roundtrippable_arg_type : expression
    ; flag_name : expression
    ; doc : expression
    ; open_runtime_lib : bool
    ; loc : Location.t
    }
end

(** Create a parameter for a ['a list]. *)
module List_param : sig
  module List_method : sig
    type t =
      | Listed (** Uses [Roundtrippable_command_param.create_listed] *)
      | Comma_separated (** Uses a comma-separated optional param. *)
    [@@deriving to_string, of_string]
  end

  type t =
    { roundtrippable_arg_type : expression
    ; flag_name : expression
    ; doc : expression
    ; open_runtime_lib : bool
    ; list_method : List_method.t
    ; loc : Location.t
    }
end

(** Uses [Or_default.create_optional_param_with_default_doc] to assemble *)
module Default_param : sig
  type t =
    { roundtrippable_arg_type : expression
    ; flag_name : expression
    ; doc : expression
    ; default : expression
    ; open_runtime_lib : bool
    ; loc : Location.t
    }
end

(** Uses [Roundtrippable_command_param.create_no_arg] to assemble *)
module Bool_no_arg_param : sig
  type t =
    { flag_name : expression
    ; doc : expression
    ; loc : Location.t
    }
end

(** Accepts customized existing [roundtrippable_command_param] or infers where to find it.

    [Default_rcp] carries a [with_defaults] flag that selects between
    [Foo.roundtrippable_command_param] and
    [Foo.roundtrippable_command_param_with_defaults] when generating the reference. *)
module Existing_param : sig
  type t =
    | Default_rcp of
        { type_lid : Longident.t loc
        ; with_defaults : bool
        }
    | Custom_rcp of expression

  val of_expr
    :  type_lid:longident loc lazy_t
    -> with_defaults:bool
    -> expression Or_default.t
    -> t

  val to_rcp : t -> expression
end

(** A [Specification.t] indicates in which way the roundtrippable command param should be
    assembled. *)
type t =
  | Existing_param of Existing_param.t
  | Bool_no_arg_param of Bool_no_arg_param.t
  | Default_param of Default_param.t
  | Required_param of Required_param.t
  | Optional_param of Optional_param.t
  | List_param of List_param.t

val rcp_expression_of_t : t -> expression
