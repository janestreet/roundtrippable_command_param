open! Base
open Ppxlib

(** Excludes attribute with name [exclude] from the list of attributes. *)
val remove_from_deriving_attributes : attributes -> exclude:string -> attributes
