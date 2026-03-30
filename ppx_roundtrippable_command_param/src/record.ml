open! Base
open! Ppxlib
open Ast_builder.Default
open Ppxlib_helpers

module With_generated_symbol = struct
  type 'a t =
    { generated_symbol : string loc
    ; inner : 'a
    }

  let mk a ~loc =
    { generated_symbol =
        gen_symbol ~prefix:"ppx_roundtrippable_command_param" () |> Located.mk ~loc
    ; inner = a
    }
  ;;
end

module Parsed_field = struct
  (** All attribute fields are lazy so that we only mark attributes as seen when they are
      actually used. This lets the compiler complain about unused attributes. Docstrings
      and dafault values are not lazy since they are not under our control. *)
  type t =
    { loc : Location.t
    ; full_loc : Location.t
    ; field_name : label loc
    ; core_type : core_type
    ; parsed_type : Parsed_type.t Lazy.t
    (** We keep the [parsed_type] lazy since parsing types can raise errors. There might
        be a case where people have complicated types and want it to work with custom rcp.
        Making parsed_type non-lazy will eliminate this ability. *)
    ; custom_roundtrippable_arg_type : expression Or_default.t Lazy.t option
    ; docstring : label loc option
    ; custom_rcp : expression Or_default.t Lazy.t option
    ; default : expression option
    ; with_defaults : bool
    ; bool_no_arg : unit Lazy.t option
    ; list_method : Specification.List_param.List_method.t Lazy.t option
    ; custom_flag_name : expression option
    }

  (** [@roundtrippable_arg_type] means to use the default roundtrippable_arg_type
      [@roundtrippable_arg_type my_roundtrippable_arg_type] means to use the
      [my_roundtrippable_arg_type] *)
  let roundtrippable_arg_type_attribute =
    Attribute.declare
      "roundtrippable_command_param.roundtrippable_arg_type"
      Attribute.Context.label_declaration
      Ast_pattern.(alt_option (single_expr_payload __) (pstr nil))
      Or_default.of_option
  ;;

  (** [@rcp.custom] means to use the default roundtrippable_command_param
      [@rcp.custom my_roundtrippable_command_param] means to use the
      [my_roundtrippable_arg_type] *)
  let custom_roundtrippable_command_param_attribute =
    Attribute.declare
      "roundtrippable_command_param.roundtrippable_command_param"
      Attribute.Context.label_declaration
      Ast_pattern.(alt_option (single_expr_payload __) (pstr nil))
      Or_default.of_option
  ;;

  (** [@rcp.bool_no_arg] means to return true if the flag is present, false otherwise. *)
  let bool_no_arg_attribute =
    Attribute.declare_flag
      "roundtrippable_command_param.bool_no_arg"
      Attribute.Context.label_declaration
  ;;

  (** [@rcp.list_method] dictates how to parse the list argument (is the flag repeated or
      does it contain comma-separated values?) *)
  let list_method_attribute =
    Attribute.declare
      "roundtrippable_command_param.list_method"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr (pstr_eval __' nil ^:: nil))
      (fun { txt; loc } : Specification.List_param.List_method.t ->
        match txt.pexp_desc with
        | Pexp_ident { txt = Longident.Lident "listed"; _ } -> Listed
        | Pexp_ident { txt = Longident.Lident "comma_separated"; _ } -> Comma_separated
        | _ ->
          Location.raise_errorf
            ~loc
            "[@list_method] value must be [listed] or [comma_separated]")
  ;;

  (** [@name "foo"] overrides the flag name for the field. *)
  let name_attribute =
    Attribute.declare
      "roundtrippable_command_param.name"
      Attribute.Context.label_declaration
      Ast_pattern.(single_expr_payload __)
      Fn.id
  ;;

  (** We have to write this since we cannot use [Attribute.get] as we did on custom
      attributes we define ourselves for docstrings and default values. *)
  let extract_attribute attributes ~pattern ~continuation ~names =
    let match_results =
      List.filter attributes ~f:(fun { attr_name; _ } ->
        List.mem names attr_name.txt ~equal:String.equal)
      |> List.filter_map ~f:(fun { attr_payload; attr_loc; _ } ->
        Ast_pattern.parse
          pattern
          attr_loc
          attr_payload
          ~on_error:(fun () -> None)
          (continuation ~attr_loc))
    in
    match match_results with
    | [] -> None
    | [ result ] -> Some result.txt
    | _ :: snd :: _ -> Location.raise_errorf ~loc:snd.loc "Duplicated attribute"
  ;;

  let extract_docstring =
    extract_attribute
      ~pattern:Ast_pattern.(single_expr_payload (estring __'))
      ~names:[ "ocaml.doc"; "doc" ]
      ~continuation:(fun ~attr_loc x -> Some (Located.mk ~loc:attr_loc x))
  ;;

  (** Currently this piece of code can totally misinterpret attributes of other modules
      also named default such as sexp as the default value. *)
  let extract_default =
    extract_attribute
      ~pattern:Ast_pattern.(single_expr_payload __)
      ~names:[ "or_default.default"; "default" ]
      ~continuation:(fun ~attr_loc x -> Some (Located.mk ~loc:attr_loc x))
  ;;

  let extract_with_defaults =
    extract_attribute
      ~pattern:Ast_pattern.(ptyp drop ||| pstr nil)
      ~names:[ "or_default.with_defaults"; "with_defaults" ]
      ~continuation:(fun ~attr_loc -> Some (Located.mk ~loc:attr_loc ()))
  ;;

  let extract_default_sig =
    extract_attribute
      ~pattern:Ast_pattern.(pstr nil)
      ~names:
        [ "or_default.default"; "default"; "or_default.with_defaults"; "with_defaults" ]
      ~continuation:(fun ~attr_loc -> Some (Located.mk ~loc:attr_loc ()))
  ;;

  let of_label_declaration ~with_default label_declaration =
    let get_attribute_lazy attribute =
      (* gets the attribute, but as a ['a Lazy.t option] that marks the attribute as seen
         only when the [Lazy.t] is forced. *)
      Attribute.get ~mark_as_seen:false attribute label_declaration
      |> Option.map ~f:(fun value ->
        lazy
          (ignore
             (Attribute.get ~mark_as_seen:true attribute label_declaration : _ option);
           value))
    in
    { full_loc = label_declaration.pld_loc
    ; loc = label_declaration.pld_type.ptyp_loc
    ; field_name = label_declaration.pld_name
    ; core_type = label_declaration.pld_type
    ; parsed_type = lazy (Parsed_type.of_core_type label_declaration.pld_type)
    ; custom_roundtrippable_arg_type =
        get_attribute_lazy roundtrippable_arg_type_attribute
    ; docstring = extract_docstring label_declaration.pld_attributes
    ; custom_rcp = get_attribute_lazy custom_roundtrippable_command_param_attribute
    ; default =
        (if with_default then extract_default label_declaration.pld_attributes else None)
    ; with_defaults =
        with_default
        && extract_with_defaults label_declaration.pld_attributes |> Option.is_some
    ; bool_no_arg = get_attribute_lazy bool_no_arg_attribute
    ; list_method = get_attribute_lazy list_method_attribute
    ; custom_flag_name = Attribute.get name_attribute label_declaration
    }
  ;;

  module Which_roundtrippable_arg_type = struct
    type t =
      | Custom_roundtrippable_arg_type of expression
      | Default_roundtrippable_arg_type of Longident.t loc

    let of_optional_attribute ~type_lid = function
      | None -> Default_roundtrippable_arg_type (Lazy.force type_lid)
      | Some expr ->
        (match Lazy.force expr with
         | Or_default.Default -> Default_roundtrippable_arg_type (Lazy.force type_lid)
         | Or_default.Custom expr -> Custom_roundtrippable_arg_type expr)
    ;;

    let to_roundtrippable_arg_type_expr = function
      | Custom_roundtrippable_arg_type expr -> expr
      | Default_roundtrippable_arg_type type_lid ->
        unapplied_type_constr_conv
          ~loc:type_lid.loc
          type_lid
          ~f:Naming.roundtrippable_arg_type_name_of_type_name
    ;;

    let open_runtime_lib = function
      | Custom_roundtrippable_arg_type _ -> false
      | Default_roundtrippable_arg_type _ -> true
    ;;
  end

  (** Here whitespaces are ignored to get a better doc string format, which may wipe out
      intended whitespaces. Additionally, since doc strings are treated as literals,
      unescape is used to generate the escape sequences. *)
  let doc_of_docstring ~loc docstring =
    let docstring = Option.value docstring ~default:(Located.mk "" ~loc) in
    let tackle_whitespaces s =
      let lines = String.split_lines s in
      (* Group lines by paragraphs (separated by empty lines) *)
      let rec group_paragraphs acc current_para = function
        | [] ->
          if List.is_empty current_para
          then List.rev acc
          else List.rev (List.rev current_para :: acc)
        | line :: rest ->
          let stripped = String.strip line in
          if String.is_empty stripped
          then
            (* Empty line marks paragraph break *)
            if List.is_empty current_para
            then group_paragraphs acc [] rest
            else group_paragraphs (List.rev current_para :: acc) [] rest
          else
            (* Add non-empty line to current paragraph *)
            group_paragraphs acc (stripped :: current_para) rest
      in
      let paragraphs = group_paragraphs [] [] lines in
      (* Join each paragraph's lines with spaces, then join paragraphs with double spaces *)
      paragraphs
      |> List.map ~f:(fun para -> String.concat ~sep:" " para)
      |> String.concat ~sep:"\n\n"
    in
    map_located docstring ~f:tackle_whitespaces |> map_with_loc ~f:estring
  ;;

  let flag_name_of_field_name (field_name : label loc) =
    map_located field_name ~f:(String.tr ~target:'_' ~replacement:'-')
    |> map_with_loc ~f:estring
  ;;

  let to_specification
    { custom_roundtrippable_arg_type
    ; parsed_type
    ; docstring
    ; loc
    ; full_loc
    ; field_name
    ; custom_rcp
    ; default
    ; with_defaults
    ; bool_no_arg
    ; list_method
    ; custom_flag_name
    ; _
    }
    =
    let lazy_arg_type_info =
      lazy
        (let roundtrippable_arg_type =
           Which_roundtrippable_arg_type.of_optional_attribute
             ~type_lid:(Lazy.map ~f:Parsed_type.type_lid parsed_type)
             custom_roundtrippable_arg_type
         in
         let open_runtime_lib =
           Which_roundtrippable_arg_type.open_runtime_lib roundtrippable_arg_type
         in
         let roundtrippable_arg_type =
           Which_roundtrippable_arg_type.to_roundtrippable_arg_type_expr
             roundtrippable_arg_type
         in
         roundtrippable_arg_type, open_runtime_lib)
    in
    let doc = doc_of_docstring ~loc:full_loc docstring in
    let flag_name =
      match custom_flag_name with
      | Some flag_name -> flag_name
      | None -> flag_name_of_field_name field_name
    in
    (* The parsing follows a waterfall pattern, starting with attributes that require
       minimal context and gradually processing more complex ones through lazy evaluation.
       This approach provides two key benefits:

       1. Only parses what's needed, avoiding unnecessary error conditions
       2. Leaves invalid attribute combinations to be caught by the compiler's unused
          attribute errors

       The only sad part is that [default] attribute is not under our control. Unused
       atttribute errors won't be triggered in this case, but we claim that it is actually
       fine since it simply won't type check if wrong combinations are used. *)
    match custom_rcp, with_defaults, bool_no_arg, default with
    | Some rcp, _, _, _ ->
      let rcp = Lazy.force rcp in
      Specification.Existing_param.of_expr
        ~type_lid:(Lazy.map ~f:Parsed_type.type_lid parsed_type)
        rcp
      |> Specification.Existing_param
    | _, true, _, _ ->
      Specification.Existing_param.of_expr
        ~type_lid:(Lazy.map ~f:Parsed_type.type_lid parsed_type)
        Default
      |> Specification.Existing_param
    | _, _, Some bool_no_arg, _ ->
      ignore (Lazy.force bool_no_arg : unit);
      Specification.Bool_no_arg_param { doc; flag_name; loc }
    | _, _, _, Some default ->
      let roundtrippable_arg_type, open_runtime_lib = Lazy.force lazy_arg_type_info in
      Specification.Default_param
        { doc; flag_name; default; roundtrippable_arg_type; open_runtime_lib; loc }
    | None, false, None, None ->
      let roundtrippable_arg_type, open_runtime_lib = Lazy.force lazy_arg_type_info in
      (match Lazy.force parsed_type with
       | Basic_type _ ->
         Specification.Required_param
           { doc; flag_name; roundtrippable_arg_type; open_runtime_lib; loc }
       | Option_type _ ->
         Specification.Optional_param
           { doc; flag_name; roundtrippable_arg_type; open_runtime_lib; loc }
       | List_type _ ->
         let list_method =
           Option.map ~f:Lazy.force list_method |> Option.value ~default:Listed
         in
         Specification.List_param
           { doc; flag_name; roundtrippable_arg_type; list_method; open_runtime_lib; loc })
  ;;
end

let with_defaults_module_name declared_type_name : longident =
  match declared_type_name with
  | "t" -> Longident.parse "With_defaults"
  | name -> Longident.parse [%string "%{String.capitalize name}_with_defaults"]
;;

(** We generate a [(t, 'b) Roundtrippable_command_param.T2.t] when
    [@@deriving roundtrippable_command_param] on [t].

    [to_input_type] gives us ['b]. Normally, ['b] is [t], but if [t] is a record with a
    [@default] field, we use [With_defaults.t]. (Similarly if [t] is actually named
    something like [my_type].) *)
let to_input_type ~with_default declared_type_name : longident loc =
  map_located declared_type_name ~f:(fun declared_type_name ->
    match with_default with
    | false -> Longident.parse declared_type_name
    | true -> Ldot (with_defaults_module_name declared_type_name, declared_type_name))
;;

let fields_make_creator_function input_type =
  let module_expr =
    unapplied_type_constr_conv ~loc:input_type.loc input_type ~f:(function
      | "t" -> "Fields"
      | name -> [%string "Fields_of_%{name}"])
  in
  match module_expr.pexp_desc with
  | Pexp_ident { txt = module_path; _ } ->
    let full_path = Longident.Ldot (module_path, "make_creator") in
    evar ~loc:input_type.loc (Longident.name full_path)
  | _ -> failwith "Expected an expression representing a module"
;;

let rcp_definition_for_record ~loc declared_type_name fields =
  let with_default =
    List.exists fields ~f:(fun field ->
      Parsed_field.extract_default field.pld_attributes |> Option.is_some
      || Parsed_field.extract_with_defaults field.pld_attributes |> Option.is_some)
  in
  let parsed_fields_with_generated_symbols =
    List.map fields ~f:(fun field ->
      Parsed_field.of_label_declaration ~with_default field
      |> With_generated_symbol.mk ~loc:field.pld_loc)
  in
  let call_to_record_builder =
    let call_to_fields_make_creator =
      let fields_make_creator_function =
        fields_make_creator_function (Located.map_lident declared_type_name)
      in
      let args_to_fields_make_creator =
        (* [~foo:(field command__008_] *)
        List.map
          parsed_fields_with_generated_symbols
          ~f:(fun { inner = { loc; field_name; _ }; generated_symbol } ->
            let generated_symbol_evar = map_with_loc generated_symbol ~f:evar in
            let field =
              match with_default with
              | false ->
                [%expr
                  Roundtrippable_command_param.Record_builder.field
                    [%e generated_symbol_evar]]
              | true ->
                let with_defaults_getter =
                  Located.map
                    (fun name -> Ldot (with_defaults_module_name name, field_name.txt))
                    declared_type_name
                  |> pexp_ident ~loc
                in
                [%expr
                  Roundtrippable_command_param.Record_builder.Bare.field
                    (Roundtrippable_command_param.contra_map
                       [%e generated_symbol_evar]
                       ~f:[%e with_defaults_getter])]
            in
            Labelled field_name.txt, field)
      in
      pexp_apply ~loc fields_make_creator_function args_to_fields_make_creator
    in
    [%expr
      Roundtrippable_command_param.Record_builder.Bare.build_for_record
        [%e call_to_fields_make_creator]]
  in
  let per_field_param_bindings =
    (* This generates the definition for the parameter to use for each field in the
       record:

       {[
         __[generated_symbol]_ = Roundtrippable_command_param.create_required ...
       ]} *)
    List.map
      parsed_fields_with_generated_symbols
      ~f:(fun { inner = parsed_field; generated_symbol } ->
        let rhs =
          Parsed_field.to_specification parsed_field |> Specification.rcp_expression_of_t
        in
        value_binding ~loc ~pat:(ppat_var ~loc generated_symbol) ~expr:rhs)
  in
  (* This pulls everything together to generate the final roundtrippable command param:

     {[
       let roundtrippable_command_param =
         let __[generated_symbol]_ = Roundtrippable_command_param.create_required ...
         and ...
         in Roundtrippable_command_param.Record_builder.build_for_record ...
     ]} *)
  [%stri
    let [%p
          Located.map Naming.rcp_variable_name_of_type_name declared_type_name
          |> ppat_var ~loc:declared_type_name.loc]
      =
      [%e pexp_let ~loc Nonrecursive per_field_param_bindings call_to_record_builder]
    ;;]
;;

let rcp_declaration_for_record ~loc declared_type_name t fields =
  let rcp_name = Located.map Naming.rcp_variable_name_of_type_name declared_type_name in
  let with_default =
    List.exists fields ~f:(fun field ->
      Parsed_field.extract_default_sig field.pld_attributes |> Option.is_some)
  in
  let output_type =
    ptyp_constr
      ~loc:declared_type_name.loc
      (to_input_type ~with_default declared_type_name)
      []
  in
  psig_value
    ~loc
    (value_description
       ~loc
       ~name:rcp_name
       ~type_:[%type: ([%t t], [%t output_type]) Roundtrippable_command_param.T2.t]
       ~prim:[])
;;
