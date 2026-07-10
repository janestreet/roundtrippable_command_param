open! Base

(** This is where, by convention, we look for the [Roundtrippable_arg_type] value for a
    given type. *)
val roundtrippable_arg_type_name_of_type_name : string -> string

(** This is where, by convention, we look for the [Roundtrippable_command_param] value for
    a given type. *)
val rcp_variable_name_of_type_name : string -> with_defaults:bool -> string

(** Similar, but for a [Command.Param] value. *)
val param_variable_name_of_type_name : string -> string
