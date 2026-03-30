open! Base
open! Ppxlib
open! Ast_builder.Default

let map_located located ~f = f located.txt |> Located.mk ~loc:located.loc
let map_with_loc located ~f = f ~loc:located.loc located.txt
