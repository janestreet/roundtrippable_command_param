open! Base
open! Ppxlib

type t =
  | Basic_type of { type_lid : Longident.t loc }
  | Option_type of { type_lid : Longident.t loc }
  | List_type of { type_lid : Longident.t loc }

let of_core_type core_type =
  let is_option lid =
    Longident.name lid
    |> List.mem
         [ "option"
         ; "Base.option"
         ; "Core.option"
         ; "Option.t"
         ; "Base.Option.t"
         ; "Core.Option.t"
         ]
         ~equal:String.equal
  in
  let is_list lid =
    Longident.name lid
    |> List.mem
         [ "list"; "Base.list"; "Core.list"; "List.t"; "Base.List.t"; "Core.List.t" ]
         ~equal:String.equal
  in
  match core_type.ptyp_desc with
  | Ptyp_constr (type_lid, []) -> Basic_type { type_lid }
  | Ptyp_constr ({ txt; _ }, [ { ptyp_desc = Ptyp_constr (type_lid, []); _ } ])
    when is_option txt -> Option_type { type_lid }
  | Ptyp_constr ({ txt; _ }, [ { ptyp_desc = Ptyp_constr (type_lid, []); _ } ])
    when is_list txt -> List_type { type_lid }
  | Ptyp_constr _ ->
    Location.raise_errorf
      ~loc:core_type.ptyp_loc
      !"ppx_roundtrippable_command_param only supports ['a] and ['a option], where ['a] \
        should be a non-parameterized type"
  | _ ->
    Location.raise_errorf
      ~loc:core_type.ptyp_loc
      "ppx_roundtrippable_command_param only support Ptyp_constr"
;;

let type_lid = function
  | Basic_type { type_lid } -> type_lid
  | Option_type { type_lid } -> type_lid
  | List_type { type_lid } -> type_lid
;;
