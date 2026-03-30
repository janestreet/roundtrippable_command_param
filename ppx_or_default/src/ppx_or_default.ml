open Base
open Ppxlib

module Maybe_in_module = struct
  type 'a t =
    | Top_level of 'a
    | In_module of longident * 'a
  [@@deriving variants]

  let pattern inner_pattern =
    let open Ast_pattern in
    ptyp_constr
      (lident inner_pattern
       |> map1 ~f:top_level
       ||| (ldot __ inner_pattern |> map2 ~f:in_module))
      nil
  ;;

  let map t ~f =
    match t with
    | Top_level inner -> Top_level (f inner)
    | In_module (module_, inner) -> In_module (module_, f inner)
  ;;

  let inner t =
    match t with
    | Top_level inner -> inner
    | In_module ((_ : longident), inner) -> inner
  ;;

  let to_longident t =
    match t with
    | Top_level inner -> Lident inner
    | In_module (module_, inner) -> Ldot (module_, inner)
  ;;

  let add_inner_to_module t = In_module (to_longident t, ())
end

module Attrs = struct
  let defaultable_label_declaration_attribute ~name ~payload =
    Attribute.declare_with_attr_loc
      (* Attribute names are path-like and may be referenced by their leaf names. *)
      [%string "or_default.%{name}"]
      Attribute.Context.label_declaration
      Ast_pattern.(alt_option payload (pstr nil))
      (fun ~attr_loc expr -> expr, attr_loc)
  ;;

  let default =
    defaultable_label_declaration_attribute
    (* This name was chosen for consistency with [sexp.default]. *)
      ~name:"default"
      ~payload:Ast_pattern.(single_expr_payload __)
  ;;

  let with_defaults =
    defaultable_label_declaration_attribute
      ~name:"with_defaults"
      ~payload:(Maybe_in_module.pattern Ast_pattern.(__) |> Ast_pattern.ptyp)
  ;;

  let get_default_for_str_exn ld =
    match Attribute.get default ld with
    | None -> None
    | Some ((Some _ as default), _attr_loc) -> default
    | Some (None, attr_loc) ->
      Location.raise_errorf
        ~loc:attr_loc
        "Unsupported use: [@or_error.default] requires a default value in structures."
  ;;

  let get_default_for_sig_exn ld =
    match Attribute.get default ld with
    | None -> None
    | Some (None, _attr_loc) -> Some ()
    | Some (Some _, attr_loc) ->
      Location.raise_errorf
        ~loc:attr_loc
        "Unsupported use: [@or_error.default] does not allow a default value in \
         signatures."
  ;;
end

module Field_annotation = struct
  type 'default t =
    | No_annotations
    | Default of 'default
    | With_defaults of string Maybe_in_module.t option

  let get_exn ld ~get_default_exn =
    let default = get_default_exn ld in
    let with_defaults = Attribute.get Attrs.with_defaults ld in
    match default, with_defaults with
    | None, None -> No_annotations
    | Some default, None -> Default default
    | None, Some (with_defaults, _attr_loc) -> With_defaults with_defaults
    | Some _, Some (_, attr_loc) ->
      Location.raise_errorf
        ~loc:attr_loc
        "Unsupported use: [@or_error.default] and [@or_error.with_defaults] cannot be \
         used on the same field."
  ;;

  let get_for_str_exn = get_exn ~get_default_exn:Attrs.get_default_for_str_exn
  let get_for_sig_exn = get_exn ~get_default_exn:Attrs.get_default_for_sig_exn
end

let type_params_to_core_types type_params = List.map type_params ~f:fst
let derived_on_type_name = "derived_on"

(* The expression [type derived_on = t]. *)
let derived_on_alias ~loc ~type_name ~type_params pstr_type_or_psig_type =
  let open Ast_builder.Default in
  let derived_on_t =
    ptyp_constr
      ~loc
      { txt = Lident type_name; loc }
      (type_params_to_core_types type_params)
  in
  let type_decl : Ppxlib_jane.Shim.Type_declaration.t =
    { ptype_name = { txt = derived_on_type_name; loc }
    ; ptype_kind = Ptype_abstract
    ; ptype_attributes = []
    ; ptype_loc = loc
    ; ptype_params = type_params
    ; ptype_cstrs = []
    ; ptype_private = Public
    ; ptype_manifest = Some derived_on_t
    ; ptype_jkind_annotation = None
    }
  in
  pstr_type_or_psig_type
    ~loc
    Nonrecursive
    [ Ppxlib_jane.Shim.Type_declaration.to_parsetree type_decl ]
;;

let derived_on_alias_declaration = derived_on_alias Ast_builder.Default.psig_type
let derived_on_alias_definition = derived_on_alias Ast_builder.Default.pstr_type

let with_defaults_module_name_of type_name =
  match type_name with
  | "t" -> "With_defaults"
  | _ -> [%string "%{String.capitalize type_name}_with_defaults"]
;;

let resolve_function_name_of type_name =
  match type_name with
  | "t" -> "resolve"
  | _ -> [%string "resolve_%{type_name}"]
;;

let with_defaults_type_name_exn ~loc ~with_defaults ~default_type =
  let with_defaults_type_name_maybe_in_module =
    match with_defaults with
    | Some with_defaults -> with_defaults
    | None ->
      let type_name_maybe_in_module =
        Ast_pattern.parse
          (Maybe_in_module.pattern Ast_pattern.(__))
          loc
          default_type
          Fn.id
      in
      let type_name = Maybe_in_module.inner type_name_maybe_in_module in
      Maybe_in_module.map type_name_maybe_in_module ~f:with_defaults_module_name_of
      |> Maybe_in_module.add_inner_to_module
      |> Maybe_in_module.map ~f:(fun () -> type_name)
  in
  Maybe_in_module.to_longident with_defaults_type_name_maybe_in_module
  |> Ast_builder.Default.Located.mk ~loc
;;

(** A function to add to [With_defaults] which returns the original type which has no
    default values. i.e. [val resolve : t -> derived_on] where [derived_on] is the type
    originally annotated with [@@deriving or_default] *)
let resolve_function_definition ~loc labels ~type_name ~type_params =
  let open (val Ast_builder.make loc) in
  (* We just need the [core_type] which is the first element in the tuple *)
  let type_params_types = type_params_to_core_types type_params in
  let pattern =
    ppat_record
      (List.map labels ~f:(fun label ->
         let name = label.pld_name.txt in
         { txt = Lident name; loc }, pvar name))
      Closed
  in
  let body_expr =
    pexp_record
      (List.map labels ~f:(fun label ->
         let name = label.pld_name.txt in
         let type_ = label.pld_type in
         let field = evar name in
         let expr =
           match Field_annotation.get_for_str_exn label with
           | No_annotations -> field
           | Default default ->
             [%expr Or_default.resolve [%e field] ~default:[%e default]]
           | With_defaults with_defaults ->
             let resolve =
               with_defaults_type_name_exn ~loc ~with_defaults ~default_type:type_
               |> unapplied_type_constr_conv ~f:resolve_function_name_of
             in
             [%expr [%e resolve] [%e field]]
         in
         { txt = Lident name; loc }, expr))
      None
  in
  let resolve_expr = pexp_fun Nolabel None pattern body_expr in
  let resolve_pat = resolve_function_name_of type_name |> pvar in
  [%stri
    let [%p resolve_pat] =
      ([%e resolve_expr]
       : [%t ptyp_constr { txt = Lident type_name; loc } type_params_types]
         -> [%t ptyp_constr { txt = Lident derived_on_type_name; loc } type_params_types])
    ;;]
;;

let resolve_function_declaration ~loc ~type_name ~type_params =
  let open (val Ast_builder.make loc) in
  (* We just need the [core_type] which is the first element in the tuple *)
  let type_params_types = type_params_to_core_types type_params in
  psig_value
    (value_description
       ~name:{ txt = resolve_function_name_of type_name; loc }
       ~type_:
         (ptyp_arrow
            Nolabel
            (ptyp_constr { txt = Lident type_name; loc } type_params_types)
            (ptyp_constr { txt = Lident derived_on_type_name; loc } type_params_types))
       ~prim:[])
;;

(* The type t expression in the With_defaults module *)
let type_t_in_with_defaults
  ~loc
  ~type_kind
  ~type_params
  ~rec_flag
  ~type_name
  ~attributes
  (pstr_type_or_psig_type : loc:location -> 'a -> type_declaration list -> 'b)
  =
  let type_decl : Ppxlib_jane.Shim.Type_declaration.t =
    { ptype_name = { txt = type_name; loc }
    ; ptype_kind = type_kind
    ; ptype_attributes = attributes
    ; ptype_loc = loc
    ; ptype_params = type_params
    ; ptype_cstrs = []
    ; ptype_private = Public
    ; ptype_manifest = None
    ; ptype_jkind_annotation = None
    }
  in
  pstr_type_or_psig_type
    ~loc
    rec_flag
    [ Ppxlib_jane.Shim.Type_declaration.to_parsetree type_decl ]
;;

let type_t_in_with_defaults_declaration =
  type_t_in_with_defaults Ast_builder.Default.psig_type
;;

let type_t_in_with_defaults_definition =
  type_t_in_with_defaults Ast_builder.Default.pstr_type
;;

let labels_with_defaults
  labels
  ~(get_field_annotation_exn : label_declaration -> _ Field_annotation.t)
  ~loc
  ~stable
  =
  (* [labels] contains all record fields, while [labels_with_added_or_default] contains
     the same fields except for those annotated with [@default <value>] which are wrapped
     in [Or_default.t], so [int] -> [int Or_default.t]. *)
  List.map labels ~f:(fun label ->
    let pld_type = { label.pld_type with ptyp_attributes = [] } in
    let pld_type =
      match get_field_annotation_exn label, stable with
      | No_annotations, _ -> pld_type
      | Default _, true -> [%type: [%t pld_type] Or_default.Stable.V1.t]
      | Default _, false -> [%type: [%t pld_type] Or_default.t]
      | With_defaults with_defaults, _ ->
        let with_defaults_type_name =
          with_defaults_type_name_exn ~loc ~with_defaults ~default_type:pld_type
        in
        Ast_builder.Default.ptyp_constr ~loc with_defaults_type_name []
    in
    { label with pld_type; pld_attributes = [] })
;;

let raise_unsupported_use_error ~loc =
  Location.raise_errorf
    ~loc
    "Unsupported use: you can only use [@@deriving or_default] on records"
;;

let raise_mult_rec_type_definitions_unsupported_error ~loc =
  Location.raise_errorf
    ~loc
    "Unsupported use: you cannot use [@@deriving or_default] on multiple recursive type \
     definitions (type a = ... and b = ... )"
;;

(** Removes the [or_default] from the [@@deriving] clause for the generated type. *)
let remove_self_from_deriving_attributes attributes =
  let attributes =
    Deriving_parser.remove_from_deriving_attributes attributes ~exclude:"or_default"
  in
  let make_ghost_loc =
    object
      inherit Ast_traverse.map as super
      method! location loc = { (super#location loc) with loc_ghost = true }
    end
  in
  make_ghost_loc#attributes attributes
;;

let or_default =
  let str_type_decl =
    Deriving.Generator.make
      Deriving.Args.(empty +> flag "stable")
      (fun ~loc ~path:_ (rec_flag, type_declarations) stable ->
        match type_declarations with
        | [ { ptype_name = { txt = type_name; _ }
            ; ptype_params = type_params
            ; ptype_kind = Ptype_record labels
            ; ptype_attributes = attributes
            ; _
            }
          ] ->
          let derived_on_alias =
            derived_on_alias_definition ~loc ~type_name ~type_params
          in
          let labels_with_added_or_default =
            labels_with_defaults
              labels
              ~get_field_annotation_exn:Field_annotation.get_for_str_exn
              ~loc
              ~stable
          in
          let resolve_function_definition = resolve_function_definition ~loc labels in
          let attributes = remove_self_from_deriving_attributes attributes in
          let loc = { loc with loc_ghost = true } in
          let open (val Ast_builder.make loc) in
          let with_defaults_module =
            pmod_structure
              [ derived_on_alias
                (* First make the outer [type t] available for later usage. *)
              ; type_t_in_with_defaults_definition
                  ~loc
                  ~type_kind:(Ptype_record labels_with_added_or_default)
                  ~rec_flag
                  ~type_name
                  ~type_params
                  ~attributes
              ; resolve_function_definition ~type_name ~type_params
              ]
          in
          let with_defaults_structure =
            pstr_module
              (module_binding
                 ~name:{ txt = Some (with_defaults_module_name_of type_name); loc }
                 ~expr:with_defaults_module)
          in
          [ with_defaults_structure ]
        | _ :: _ :: _ -> raise_mult_rec_type_definitions_unsupported_error ~loc
        | _ -> raise_unsupported_use_error ~loc)
  in
  let sig_type_decl =
    Deriving.Generator.make_noarg (fun ~loc ~path:_ (rec_flag, type_declarations) ->
      let loc = { loc with loc_ghost = true } in
      let open (val Ast_builder.make loc) in
      match type_declarations with
      | [ ({ ptype_name = { txt = type_name; _ }
           ; ptype_params = type_params
           ; ptype_kind = type_kind
           ; ptype_attributes = attributes
           ; _
           } as _type_declaration)
        ] ->
        let derived_on_alias =
          derived_on_alias_declaration ~loc ~type_name ~type_params
        in
        let type_declaration =
          let attributes = remove_self_from_deriving_attributes attributes in
          let type_t_in_with_defaults_declaration =
            type_t_in_with_defaults_declaration
              ~loc
              ~rec_flag
              ~type_name
              ~type_params
              ~attributes
          in
          match type_kind with
          | Ptype_record labels ->
            let labels_with_added_or_default =
              labels_with_defaults
                labels
                ~get_field_annotation_exn:Field_annotation.get_for_sig_exn
                ~loc
                ~stable:false
            in
            type_t_in_with_defaults_declaration
              ~type_kind:(Ptype_record labels_with_added_or_default)
          | Ptype_abstract -> type_t_in_with_defaults_declaration ~type_kind
          | _ -> raise_unsupported_use_error ~loc
        in
        let module_type =
          pmty_signature
            [ derived_on_alias
            ; type_declaration
            ; resolve_function_declaration ~loc ~type_name ~type_params
            ]
        in
        let with_defaults_signature =
          psig_module
            (module_declaration
               ~name:{ txt = Some (with_defaults_module_name_of type_name); loc }
               ~type_:module_type)
        in
        [ with_defaults_signature ]
      | _ :: _ :: _ -> raise_mult_rec_type_definitions_unsupported_error ~loc
      | _ -> raise_unsupported_use_error ~loc)
  in
  Deriving.add "or_default" ~str_type_decl ~sig_type_decl
;;
