open! Base
open! Ppxlib

(** [Parsed_type] analyzes OCaml core types to automatically derive parameter
    specifications without requiring explicit user attributes (for example [option]). It
    is also responsible for retrieving the appropriate arg type when using pre-defined
    roundtrippable arg type. *)
type t =
  | Basic_type of { type_lid : Longident.t loc }
  | Option_type of { type_lid : Longident.t loc }
  | List_type of { type_lid : Longident.t loc }

val of_core_type : core_type -> t
val type_lid : t -> Longident.t loc
