open! Base
open! Ppxlib
open! Ast_builder.Default

(** A version of [Located.map] with a labeled [~f] argument. *)
val map_located : 'a loc -> f:('a -> 'b) -> 'b loc

val map_with_loc : 'a loc -> f:(loc:location -> 'a -> 'b) -> 'b
