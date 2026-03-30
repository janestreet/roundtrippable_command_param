open! Base
open! Ppxlib

let unwrap_type_decls_exn = function
  | [ td ] -> td
  | _ ->
    Location.raise_errorf
      "ppx_roundtrippable_command_param only supports one type at a time"
;;

let rcp_str_declaration ~loc tds =
  let td = unwrap_type_decls_exn tds in
  let declared_type_name = td.ptype_name in
  match td.ptype_kind, td.ptype_manifest with
  | Ptype_abstract, Some ct ->
    Abstract.rcp_definition_for_abstract ~loc declared_type_name ct
  | Ptype_record fields, _ ->
    Record.rcp_definition_for_record ~loc declared_type_name fields
  | _ -> Location.raise_errorf ~loc "not implemented"
;;

let param_str_declaration ~loc tds =
  let open Ast_builder.Default in
  let open Ppxlib_helpers in
  let td = unwrap_type_decls_exn tds in
  let type_name = td.ptype_name in
  let name =
    map_located ~f:Naming.param_variable_name_of_type_name type_name |> ppat_var ~loc
  in
  let rcp_name =
    map_located ~f:Naming.rcp_variable_name_of_type_name type_name
    |> map_located ~f:lident
    |> pexp_ident ~loc
  in
  [%stri let [%p name] = Roundtrippable_command_param.param [%e rcp_name]]
;;

let rcp_sig_declaration ~loc tds =
  let td = unwrap_type_decls_exn tds in
  let t = core_type_of_type_declaration td in
  match td.ptype_kind, td.ptype_manifest with
  | Ptype_abstract, _ -> Abstract.rcp_declaration_for_abstract ~loc td.ptype_name t
  | Ptype_record fields, _ ->
    Record.rcp_declaration_for_record ~loc td.ptype_name t fields
  | _ -> Location.raise_errorf ~loc "not implemented"
;;

let param_sig_declaration ~loc tds =
  let open Ast_builder.Default in
  let open Ppxlib_helpers in
  let td = unwrap_type_decls_exn tds in
  let type_name = td.ptype_name in
  let name = map_located ~f:Naming.param_variable_name_of_type_name type_name in
  value_description
    ~loc
    ~name
    ~type_:[%type: [%t core_type_of_type_declaration td] Command.Param.t]
    ~prim:[]
  |> psig_value ~loc
;;

(** Workaround to work with [ppx_or_default]: [ppx_or_default] will copy the whole type
    declaration, including all the derivings, but dropping all the attributes. Given this
    behavior, we do not encourage or is able to generate a roundtrippable command param
    within the [With_defaults] module. Therefore, if we think we are inside the
    [With_defaults] module, we generate nothing. This means user cannot name their module
    name with [ith_defaults] suffix, or [ppx_roundtrippable_command_param] will refuse to
    work. *)
let within_with_defaults path = String.is_suffix path ~suffix:"ith_defaults"

(* Currently we do not support nested records with same field names. *)
let () =
  Deriving.add
    "roundtrippable_command_param"
    ~str_type_decl:
      (Deriving.Generator.make Deriving.Args.empty (fun ~loc ~path (_rec, tds) ->
         match within_with_defaults path with
         | true -> []
         | false -> [ rcp_str_declaration ~loc tds; param_str_declaration ~loc tds ]))
    ~sig_type_decl:
      (Deriving.Generator.make Deriving.Args.empty (fun ~loc ~path (_rec, tds) ->
         match within_with_defaults path with
         | true -> []
         | false -> [ rcp_sig_declaration ~loc tds; param_sig_declaration ~loc tds ]))
  |> Deriving.ignore
;;
