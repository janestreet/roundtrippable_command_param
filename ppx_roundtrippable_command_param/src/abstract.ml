open! Base
open! Ppxlib
open! Ast_builder.Default

let rcp_definition_for_abstract ~loc type_name ct =
  let rcp_expr =
    let type_lid = Parsed_type.of_core_type ct |> Parsed_type.type_lid in
    Specification.Existing_param.Default_rcp { type_lid; with_defaults = false }
    |> Specification.Existing_param.to_rcp
  in
  let rcp_name =
    Located.map (Naming.rcp_variable_name_of_type_name ~with_defaults:false) type_name
  in
  pstr_value
    ~loc
    Nonrecursive
    [ value_binding ~loc ~pat:(ppat_var ~loc:rcp_name.loc rcp_name) ~expr:rcp_expr ]
;;

let rcp_declaration_for_abstract ~loc declared_type_name t =
  let rcp_name =
    Located.map
      (Naming.rcp_variable_name_of_type_name ~with_defaults:false)
      declared_type_name
  in
  psig_value
    ~loc
    (value_description
       ~loc
       ~name:rcp_name
       ~type_:[%type: [%t t] Roundtrippable_command_param.t]
       ~prim:[])
;;
