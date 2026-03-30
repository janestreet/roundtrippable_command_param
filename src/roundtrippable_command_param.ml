open! Base
open! Import

module T1 = struct
  type ('a, 'b) t =
    { param : 'a Command.Param.t
    ; command_args : 'b -> string list
    }
  [@@deriving fields ~getters ~iterators:create]

  let create = Fields.create
  let map t ~f = { t with param = Command.Param.map t.param ~f }
  let contra_map t ~f = { t with command_args = (fun b -> t.command_args (f b)) }

  let both t1 t2 =
    { command_args = (fun b -> t1.command_args b @ t2.command_args b)
    ; param = Command.Param.both t1.param t2.param
    }
  ;;
end

module T2 = struct
  include T1

  include Applicative.Make2_using_map2 (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let return a = { param = Command.Param.return a; command_args = (fun _ -> []) }
      let map2 t1 t2 ~f = map (both t1 t2) ~f:(fun (a, b) -> f a b)
      let map = `Custom map
    end)
end

module Record_builder = Profunctor.Record_builder (T2)
include T2

type 'a t = ('a, 'a) T2.t

include
  Applicative.Make_let_syntax2
    (T2)
    (struct
      module type S = module type of T2
    end)
    (T2)

let dash = "-"
let name_with_dash name = if String.is_prefix ~prefix:dash name then name else "-" ^ name

let create ?aliases ?full_flag_required name flag ~doc ~command_args =
  { param = Command.Param.flag ?aliases ?full_flag_required name flag ~doc
  ; command_args = (fun x -> command_args ~name_with_dash:(name_with_dash name) x)
  }
;;

let create_optional ?aliases ?full_flag_required name arg_type ~doc ~to_string =
  create
    ?aliases
    ?full_flag_required
    name
    (Command.Param.optional arg_type)
    ~doc
    ~command_args:(fun ~name_with_dash x ->
      match x with
      | None -> []
      | Some x -> [ name_with_dash; to_string x ])
;;

let optional_enum_command_args enum name x =
  match x with
  | None -> []
  | Some x -> [ name_with_dash name; Enum.to_string_hum enum x ]
;;

let create_optional_from_enum
  ?case_sensitive
  ?represent_choice_with
  ?list_values_in_help
  ?aliases
  ?key
  name
  enum
  ~doc
  =
  { param =
      Enum.make_param
        ?case_sensitive
        ?represent_choice_with
        ?list_values_in_help
        ?aliases
        ?key
        name
        enum
        ~f:Command.Param.optional
        ~doc
  ; command_args = optional_enum_command_args enum name
  }
;;

let create_optional_with_default
  ?aliases
  ?default_value_doc_string
  name
  arg_type
  ~default
  ~to_string
  ~doc
  =
  let to_string_as_sexp a =
    let str =
      match default_value_doc_string with
      | Some str -> str
      | None -> to_string a
    in
    [%sexp_of: string] str
  in
  { param =
      Command.Param.flag_optional_with_default_doc_sexp
        ?aliases
        name
        arg_type
        to_string_as_sexp
        ~default
        ~doc
  ; command_args =
      (function
        | None -> []
        | Some x -> [ name_with_dash name; to_string x ])
  }
;;

let create_optional_with_default_drop_default
  ?aliases
  ?default_value_doc_string
  name
  arg_type
  ~default
  ~equal
  ~to_string
  ~doc
  =
  create_optional_with_default
    ?aliases
    ?default_value_doc_string
    name
    arg_type
    ~default
    ~to_string
    ~doc
  |> contra_map ~f:(fun x -> if equal x default then None else Some x)
;;

let create_optional_with_default_from_enum
  ?case_sensitive
  ?represent_choice_with
  ?list_values_in_help
  ?aliases
  ?key
  name
  enum
  ~default
  ~doc
  =
  { param =
      Enum.make_param_optional_with_default_doc_sexp
        ?case_sensitive
        ?represent_choice_with
        ?list_values_in_help
        ?aliases
        ?key
        name
        enum
        ~doc
        ~default
  ; command_args = optional_enum_command_args enum name
  }
;;

let create_required ?aliases name arg_type ~to_string ~doc =
  create
    ?aliases
    name
    (Command.Param.required arg_type)
    ~command_args:(fun ~name_with_dash x -> [ name_with_dash; to_string x ])
    ~doc
;;

let create_required_from_enum
  ?case_sensitive
  ?represent_choice_with
  ?list_values_in_help
  ?aliases
  ?key
  name
  enum
  ~doc
  =
  { param =
      Enum.make_param
        ?case_sensitive
        ?represent_choice_with
        ?list_values_in_help
        ?aliases
        ?key
        name
        enum
        ~f:Command.Param.required
        ~doc
  ; command_args = (fun x -> [ name_with_dash name; Enum.to_string_hum enum x ])
  }
;;

let create_flags_from_enum ?aliases enum ~doc =
  { param = Enum.make_param_one_of_flags ?aliases enum ~doc
  ; command_args = (fun x -> [ name_with_dash (Enum.to_string_hum enum x) ])
  }
;;

let create_listed ?aliases name arg_type ~doc ~to_string =
  create
    ?aliases
    name
    (Command.Param.listed arg_type)
    ~doc
    ~command_args:(fun ~name_with_dash ->
      List.concat_map ~f:(fun x -> [ name_with_dash; to_string x ]))
;;

let create_one_or_more_as_pair ?aliases name arg_type ~doc ~to_string =
  create
    ?aliases
    name
    (Command.Param.one_or_more_as_pair arg_type)
    ~doc
    ~command_args:(fun ~name_with_dash (x, xs) ->
      List.concat_map (x :: xs) ~f:(fun x -> [ name_with_dash; to_string x ]))
;;

let create_one_or_more_as_list ?aliases name arg_type ~doc ~to_string =
  create
    ?aliases
    name
    (Command.Param.one_or_more_as_list arg_type)
    ~doc
    ~command_args:(fun ~name_with_dash xs ->
      List.concat_map xs ~f:(fun x -> [ name_with_dash; to_string x ]))
;;

let create_no_arg ?aliases name ~doc =
  create
    ?aliases
    ~command_args:(fun ~name_with_dash p -> if p then [ name_with_dash ] else [])
    name
    Command.Param.no_arg
    ~doc
;;

let create_no_arg_some ?aliases name value ~doc =
  create
    ?aliases
    ~command_args:(fun ~name_with_dash p ->
      if Option.is_some p then [ name_with_dash ] else [])
    name
    (Command.Param.no_arg_some value)
    ~doc
;;

let create_no_arg_required ?aliases name ~doc =
  create
    ?aliases
    ~command_args:(fun ~name_with_dash () -> [ name_with_dash ])
    name
    (Command.Param.no_arg_required ())
    ~doc
;;

let pair t1 t2 =
  { param = Command.Param.both t1.param t2.param
  ; command_args = (fun (a1, a2) -> t1.command_args a1 @ t2.command_args a2)
  }
;;

module Anons = struct
  module T2 = struct
    type ('a, 'b) t =
      { anon : 'a Command.Anons.t
      ; to_command_args : 'b -> string list
      }

    let map t ~f = { t with anon = Command.Param.map_anons t.anon ~f }
    let contra_map t ~f = { t with to_command_args = (fun b -> t.to_command_args (f b)) }
  end

  include T2

  type 'a t = ('a, 'a) T2.t

  let one name arg_type ~to_string =
    { anon = Command.Param.(name %: arg_type)
    ; to_command_args = (fun x -> [ to_string x ])
    }
  ;;

  let sequence { anon; to_command_args } =
    { anon = Command.Param.sequence anon
    ; to_command_args = (fun x -> List.map x ~f:to_command_args |> List.concat)
    }
  ;;

  let non_empty_sequence_as_pair { anon; to_command_args } =
    { anon = Command.Param.non_empty_sequence_as_pair anon
    ; to_command_args =
        (fun (x, l) -> to_command_args x :: List.map l ~f:to_command_args |> List.concat)
    }
  ;;

  let non_empty_sequence_as_list { anon; to_command_args } =
    { anon = Command.Param.non_empty_sequence_as_list anon
    ; to_command_args =
        (function
          | [] -> raise_s [%sexp "Serialization error: List must not be empty"]
          | l -> List.map l ~f:to_command_args |> List.concat)
    }
  ;;

  let maybe { anon; to_command_args } =
    { anon = Command.Param.maybe anon
    ; to_command_args =
        (function
          | Some x -> to_command_args x
          | None -> [])
    }
  ;;

  let maybe_with_default default { anon; to_command_args } =
    { anon = Command.Param.maybe_with_default default anon
    ; to_command_args =
        (function
          | Some x -> to_command_args x
          | None -> [])
    }
  ;;

  let t2 t1 t2 =
    { anon = Command.Param.t2 t1.anon t2.anon
    ; to_command_args =
        (fun (a1, a2) -> List.concat [ t1.to_command_args a1; t2.to_command_args a2 ])
    }
  ;;

  let t3 t1 t2 t3 =
    { anon = Command.Param.t3 t1.anon t2.anon t3.anon
    ; to_command_args =
        (fun (a1, a2, a3) ->
          List.concat
            [ t1.to_command_args a1; t2.to_command_args a2; t3.to_command_args a3 ])
    }
  ;;

  let t4 t1 t2 t3 t4 =
    { anon = Command.Param.t4 t1.anon t2.anon t3.anon t4.anon
    ; to_command_args =
        (fun (a1, a2, a3, a4) ->
          List.concat
            [ t1.to_command_args a1
            ; t2.to_command_args a2
            ; t3.to_command_args a3
            ; t4.to_command_args a4
            ])
    }
  ;;
end

let anon Anons.T2.{ anon; to_command_args = command_args } =
  { param = Command.Param.anon anon; command_args }
;;

module Variant_builder = struct
  type 'a t = 'a option Command.Param.t list

  module Match_result = struct
    type t = string list
  end

  module Case = struct
    type nonrec ('constructor, 'variant, 'matcher) t =
      'constructor Variant.t -> 'variant t -> 'matcher * 'variant t
  end

  module If_nothing_chosen = struct
    type (_, _, _) t =
      | Default_to : 'a -> ('a, 'a, 'a option) t
      | Raise : ('a, 'a, 'a) t
      | Return_none : ('a, 'a option, 'a option) t

    let to_command_if_nothing_chosen (type a b c) (t : (a, b, c) t)
      : (a, b) Command.Param.If_nothing_chosen.t
      =
      match t with
      | Default_to default -> Default_to default
      | Raise -> Raise
      | Return_none -> Return_none
    ;;
  end

  let build_variant
    (type variant param_type command_args_type)
    (make_matcher_and_fold_over_constructors :
      variant t -> (variant -> Match_result.t) * variant t)
    ~(if_nothing_chosen : (variant, param_type, command_args_type) If_nothing_chosen.t)
    : (param_type, command_args_type) T2.t
    =
    let command_args, params = make_matcher_and_fold_over_constructors [] in
    let param =
      Command.Param.choose_one
        params
        ~if_nothing_chosen:
          (If_nothing_chosen.to_command_if_nothing_chosen if_nothing_chosen)
    in
    let command_args : command_args_type -> string list =
      let command_args_for_option = function
        | None -> []
        | Some a -> command_args a
      in
      match if_nothing_chosen with
      | Default_to (_ : variant) -> command_args_for_option
      | Return_none -> command_args_for_option
      | Raise -> command_args
    in
    T2.create ~param ~command_args
  ;;

  let build_set
    (type variant cmp)
    (make_matcher_and_fold_over_constructors :
      variant t -> (variant -> Match_result.t) * variant t)
    ~comparable:
      (module Variant : Comparable.S
        with type t = variant
         and type comparator_witness = cmp)
    : ((variant, cmp) Set.t, (variant, cmp) Set.t) T2.t
    =
    let command_args, params = make_matcher_and_fold_over_constructors [] in
    let param =
      Command.Param.all params
      |> Command.Param.map ~f:(fun params ->
        params |> List.filter_map ~f:Fn.id |> Set.of_list (module Variant))
    in
    let command_args variants =
      variants |> Set.to_list |> List.concat_map ~f:command_args
    in
    T2.create ~param ~command_args
  ;;

  let variant roundtrippable_param ~to_variant ~to_matcher (v : _ Variant.t) params =
    (match command_args roundtrippable_param None with
     | [] -> ()
     | _ :: _ as args_for_none ->
       raise_s
         [%message
           "[Variant_builder] doesn't support passing arguments to represent [None]"
             (args_for_none : string list)]);
    let param =
      Command.Param.map
        (param roundtrippable_param)
        ~f:(Option.map ~f:(to_variant v.constructor))
    in
    let params = param :: params in
    let command_args =
      to_matcher (fun argument -> command_args roundtrippable_param (Some argument))
    in
    command_args, params
  ;;
end

module All_or_nothing_param = struct
  type _ t =
    | [] : unit t
    | ( :: ) : ('a option, 'a option) T2.t * 'b t -> ('a * 'b) t

  let rec to_command_args : type a. a t -> a option -> string list =
    fun t option ->
    match t with
    | [] -> []
    | roundtrippable :: t ->
      let v, vs =
        match option with
        | None -> None, None
        | Some (v, vs) -> Some v, Some vs
      in
      command_args roundtrippable v @ to_command_args t vs
  ;;

  let to_param t =
    let open struct
      type 'a all_or_nothing =
        | All of
            { arg_names : string list list
            ; value : 'a
            }
        | Nothing of { arg_names : string list list }
        | Something_in_between of
            { absent : string list list
            ; present : string list list
            }

      let rec to_param : type a b. (a * b) t -> (a * b) all_or_nothing Command.Param.t
        = function
        | [ roundtrippable ] ->
          let%map_open.Command a, arg_names = and_arg_names (param roundtrippable) in
          let arg_names = List.return arg_names in
          (match a with
           | Some value -> All { arg_names; value = value, () }
           | None -> Nothing { arg_names })
        | roundtrippable :: (_ :: _ as t) ->
          let%map_open.Command a, arg_names = and_arg_names (param roundtrippable)
          and all_or_nothing = to_param t in
          (match a with
           | Some a ->
             (match all_or_nothing with
              | All { arg_names = present; value } ->
                All { arg_names = arg_names :: present; value = a, value }
              | Nothing { arg_names = absent } ->
                Something_in_between { absent; present = [ arg_names ] }
              | Something_in_between { absent; present } ->
                Something_in_between { absent; present = arg_names :: present })
           | None ->
             (match all_or_nothing with
              | All { arg_names = present; value = _ } ->
                Something_in_between { absent = [ arg_names ]; present }
              | Nothing { arg_names = absent } ->
                Nothing { arg_names = arg_names :: absent }
              | Something_in_between { absent; present } ->
                Something_in_between { absent = arg_names :: absent; present }))
      ;;
    end in
    match%map.Command to_param t with
    | All { arg_names = _; value } -> Some value
    | Nothing { arg_names = _ } -> None
    | Something_in_between { absent; present } ->
      let s = "Must pass all or none of these arguments, but got a mix." in
      raise_s [%message s (absent : string list list) (present : string list list)]
  ;;

  let to_variant_builder t ~curry ~uncurry =
    T2.create ~param:(to_param t) ~command_args:(to_command_args t)
    |> Variant_builder.variant ~to_variant:curry ~to_matcher:uncurry
  ;;
end

let build_variant = Variant_builder.build_variant
let build_set = Variant_builder.build_set
let variant = Variant_builder.variant

let variant1 a =
  All_or_nothing_param.to_variant_builder
    [ a ]
    ~curry:(fun f (a, ()) -> f a)
    ~uncurry:(fun f a -> f (a, ()))
;;

let variant0 (type a) a (v : a Variant.t) params =
  let f, params =
    variant1
      (map a ~f:(fun bool -> Option.some_if bool v.constructor)
       |> contra_map ~f:Option.is_some)
      { v with constructor = (fun (_ : a) -> v.constructor) }
      params
  in
  (fun () -> f v.constructor), params
;;

let variant2 a b =
  All_or_nothing_param.to_variant_builder
    [ a; b ]
    ~curry:(fun f (a, (b, ())) -> f a b)
    ~uncurry:(fun f a b -> f (a, (b, ())))
;;

let variant3 a b c =
  All_or_nothing_param.to_variant_builder
    [ a; b; c ]
    ~curry:(fun f (a, (b, (c, ()))) -> f a b c)
    ~uncurry:(fun f a b c -> f (a, (b, (c, ()))))
;;

let int_option name ~doc =
  create_optional name Command.Param.int ~doc ~to_string:Int.to_string
;;

let string_option name ~doc =
  create_optional name Command.Param.string ~doc ~to_string:String.to_string
;;

let optional_params params =
  T2.create
    ~param:
      (Command.Param.choose_one_non_optional
         [ param params ]
         ~if_nothing_chosen:Return_none)
    ~command_args:(function
      | Some value -> command_args params value
      | None -> [])
;;

let alias_params { param; command_args } alternatives ~if_nothing_chosen =
  T2.create
    ~param:(Command.Param.choose_one (param :: alternatives) ~if_nothing_chosen)
    ~command_args
;;

module Variant_builder_non_optional = struct
  type 'a t = 'a Command.Param.t list

  module Match_result = struct
    type t = string list
  end

  module Case = struct
    type nonrec ('constructor, 'variant, 'matcher) t =
      'constructor Variant.t -> 'variant t -> 'matcher * 'variant t
  end

  let build_variant
    (type variant param_type command_args_type)
    (make_matcher_and_fold_over_constructors :
      variant t -> (variant -> Match_result.t) * variant t)
    ~(if_nothing_chosen :
        (variant, param_type, command_args_type) Variant_builder.If_nothing_chosen.t)
    : (param_type, command_args_type) T2.t
    =
    let command_args, params = make_matcher_and_fold_over_constructors [] in
    let param =
      Command.Param.choose_one_non_optional
        params
        ~if_nothing_chosen:
          (Variant_builder.If_nothing_chosen.to_command_if_nothing_chosen
             if_nothing_chosen)
    in
    let command_args : command_args_type -> string list =
      let command_args_for_option = function
        | None -> []
        | Some a -> command_args a
      in
      match if_nothing_chosen with
      | Default_to (_ : variant) -> command_args_for_option
      | Return_none -> command_args_for_option
      | Raise -> command_args
    in
    T2.create ~param ~command_args
  ;;

  let variant roundtrippable_param ~to_variant ~to_matcher (v : _ Variant.t) params =
    let param =
      Command.Param.map (param roundtrippable_param) ~f:(to_variant v.constructor)
    in
    let params = param :: params in
    let command_args =
      to_matcher (fun argument -> command_args roundtrippable_param argument)
    in
    command_args, params
  ;;

  let variant0 roundtrippable_param =
    variant roundtrippable_param ~to_variant:(fun f () -> f) ~to_matcher:Fn.id
  ;;

  let variant1 roundtrippable_param =
    variant roundtrippable_param ~to_variant:Fn.id ~to_matcher:Fn.id
  ;;

  let variant2 roundtrippable_param1 roundtrippable_param2 =
    variant
      (pair roundtrippable_param1 roundtrippable_param2)
      ~to_variant:(fun f (a, b) -> f a b)
      ~to_matcher:(fun f a b -> f (a, b))
  ;;

  let variant3 roundtrippable_param1 roundtrippable_param2 roundtrippable_param3 =
    variant
      (pair (pair roundtrippable_param1 roundtrippable_param2) roundtrippable_param3)
      ~to_variant:(fun f ((a, b), c) -> f a b c)
      ~to_matcher:(fun f a b c -> f ((a, b), c))
  ;;

  let alias_params { param; command_args } alternatives ~if_nothing_chosen =
    T2.create
      ~param:
        (Command.Param.choose_one_non_optional (param :: alternatives) ~if_nothing_chosen)
      ~command_args
  ;;
end
