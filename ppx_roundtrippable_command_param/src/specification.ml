open! Base
open! Ppxlib

let wrap_with_open_runtime_lib ~open_runtime_lib ~loc rcp_expr =
  match open_runtime_lib with
  | false -> rcp_expr
  | true ->
    [%expr
      let open! Ppx_roundtrippable_command_param_runtime in
      [%e rcp_expr]]
;;

module Required_param = struct
  type t =
    { roundtrippable_arg_type : expression
    ; flag_name : expression
    ; doc : expression
    ; open_runtime_lib : bool
    ; loc : Location.t
    }

  (* The convention of doc string used in roundtrippable_command_param is "INT port to
     serve on" -> "-flag INT . port to serve on", so we prepend arg name here. *)
  let to_rcp { roundtrippable_arg_type; flag_name; doc; open_runtime_lib; loc } =
    [%expr
      Roundtrippable_command_param.create_required
        [%e flag_name]
        (Roundtrippable_arg_type.arg_type [%e roundtrippable_arg_type])
        ~doc:
          (Roundtrippable_arg_type.arg_placeholder [%e roundtrippable_arg_type]
           ^ " "
           ^ [%e doc])
        ~to_string:
          (Roundtrippable_arg_type.to_string [%e roundtrippable_arg_type]
           |> Staged.unstage)]
    |> wrap_with_open_runtime_lib ~open_runtime_lib ~loc
  ;;
end

module Optional_param = struct
  type t =
    { roundtrippable_arg_type : expression
    ; flag_name : expression
    ; doc : expression
    ; open_runtime_lib : bool
    ; loc : Location.t
    }

  let to_rcp { roundtrippable_arg_type; flag_name; doc; open_runtime_lib; loc } =
    [%expr
      Roundtrippable_command_param.create_optional
        [%e flag_name]
        (Roundtrippable_arg_type.arg_type [%e roundtrippable_arg_type])
        ~doc:
          (Roundtrippable_arg_type.arg_placeholder [%e roundtrippable_arg_type]
           ^ " "
           ^ [%e doc])
        ~to_string:
          (Roundtrippable_arg_type.to_string [%e roundtrippable_arg_type]
           |> Staged.unstage)]
    |> wrap_with_open_runtime_lib ~open_runtime_lib ~loc
  ;;
end

module List_param = struct
  module List_method = struct
    type t =
      | Listed
      | Comma_separated
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

  let to_rcp
    { roundtrippable_arg_type; flag_name; doc; list_method; open_runtime_lib; loc }
    =
    let param_expr =
      match list_method with
      | Listed ->
        [%expr
          Roundtrippable_command_param.create_listed
            [%e flag_name]
            (Roundtrippable_arg_type.arg_type [%e roundtrippable_arg_type])
            ~doc:
              (Roundtrippable_arg_type.arg_placeholder [%e roundtrippable_arg_type]
               ^ " "
               ^ [%e doc]
               ^ " (can be passed multiple times)")
            ~to_string:
              (Roundtrippable_arg_type.to_string [%e roundtrippable_arg_type]
               |> Staged.unstage)]
      | Comma_separated ->
        [%expr
          let roundtrippable_arg_type =
            Roundtrippable_arg_type.comma_separated [%e roundtrippable_arg_type]
          in
          Roundtrippable_command_param.create_optional_with_default_drop_default
            [%e flag_name]
            (Roundtrippable_arg_type.arg_type roundtrippable_arg_type)
            ~default:[]
            ~equal:[%equal: _ list]
            ~doc:
              (Roundtrippable_arg_type.arg_placeholder roundtrippable_arg_type
               ^ " "
               ^ [%e doc]
               ^ " (comma-separated)")
            ~to_string:
              (Roundtrippable_arg_type.to_string roundtrippable_arg_type |> Staged.unstage)]
    in
    wrap_with_open_runtime_lib ~open_runtime_lib ~loc param_expr
  ;;
end

module Default_param = struct
  type t =
    { roundtrippable_arg_type : expression
    ; flag_name : expression
    ; doc : expression
    ; default : expression
    ; open_runtime_lib : bool
    ; loc : Location.t
    }

  let to_rcp { roundtrippable_arg_type; flag_name; doc; default; open_runtime_lib; loc } =
    [%expr
      Or_default.create_optional_param_with_default_doc'
        [%e flag_name]
        (Roundtrippable_arg_type.arg_type [%e roundtrippable_arg_type])
        ~default:[%e default]
        ~doc:
          (Roundtrippable_arg_type.arg_placeholder [%e roundtrippable_arg_type]
           ^ " "
           ^ [%e doc])
        ~to_string:
          (Roundtrippable_arg_type.to_string [%e roundtrippable_arg_type]
           |> Staged.unstage)]
    |> wrap_with_open_runtime_lib ~open_runtime_lib ~loc
  ;;
end

module Existing_param = struct
  type t =
    | Default_rcp of Longident.t loc
    | Custom_rcp of expression

  let of_expr ~type_lid = function
    | Or_default.Default -> Default_rcp (Lazy.force type_lid)
    | Or_default.Custom expr -> Custom_rcp expr
  ;;

  let to_rcp = function
    | Default_rcp type_lid ->
      Ast_builder.Default.unapplied_type_constr_conv
        ~loc:type_lid.loc
        type_lid
        ~f:Naming.rcp_variable_name_of_type_name
    | Custom_rcp expr -> expr
  ;;
end

module Bool_no_arg_param = struct
  type t =
    { flag_name : expression
    ; doc : expression
    ; loc : Location.t
    }

  let to_rcp { flag_name; doc; loc } =
    [%expr Roundtrippable_command_param.create_no_arg [%e flag_name] ~doc:[%e doc]]
  ;;
end

type t =
  | Existing_param of Existing_param.t
  | Bool_no_arg_param of Bool_no_arg_param.t
  | Default_param of Default_param.t
  | Required_param of Required_param.t
  | Optional_param of Optional_param.t
  | List_param of List_param.t

let rcp_expression_of_t = function
  | Existing_param existing_param -> Existing_param.to_rcp existing_param
  | Bool_no_arg_param bool_no_arg_param -> Bool_no_arg_param.to_rcp bool_no_arg_param
  | Default_param default_param -> Default_param.to_rcp default_param
  | Required_param required_param -> Required_param.to_rcp required_param
  | Optional_param optional_param -> Optional_param.to_rcp optional_param
  | List_param list_param -> List_param.to_rcp list_param
;;
