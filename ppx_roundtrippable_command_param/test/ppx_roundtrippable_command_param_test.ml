open! Core
open! Async
open! Expect_test_helpers_core
open! Ppxlib

module type With_equal_and_rcp = sig
  type t [@@deriving equal, sexp_of, roundtrippable_command_param]
end

module Test_case = struct
  type ('a, 'b) t =
    { input : 'a
    ; output : 'b
    }
end

(** Input represents the ocaml code we want to feed into the roundtrippable command param
    to get args, and output represents the ocaml code get from args fed into the
    roundtrippable command param. *)
let generate_args_and_test_roundtrip''
  (type a b)
  ?separate_rows
  ~(to_sexp_input : a -> Sexp.t)
  ~(to_sexp_output : b -> Sexp.t)
  ~(equal : b -> b -> bool)
  ~rcp
  ~(test_cases : (a, b) Test_case.t list)
  ()
  =
  let parse args =
    Command.Param.parse (Roundtrippable_command_param.param rcp) args |> Or_error.ok_exn
  in
  let table_row_for_value_and_test_roundtrip { Test_case.input; output } =
    let args = Roundtrippable_command_param.command_args rcp input in
    require_equal
      (module struct
        type t = b

        let equal = equal
        let sexp_of_t = to_sexp_output
      end)
      (parse args)
      output;
    String.concat ~sep:" " args
  in
  Expectable.print_cases
    ?separate_rows
    ~f:table_row_for_value_and_test_roundtrip
    ~sexp_of_input:(fun { Test_case.input; output = _ } -> to_sexp_input input)
    ~sexp_of_output:(function
      | "" -> [%sexp { args : string = "EMPTY" }]
      | args -> [%sexp { args : string }])
    test_cases
;;

let generate_args_and_test_roundtrip'
  (type a)
  ?separate_rows
  ~to_sexp
  ~(equal : a -> a -> bool)
  ~rcp
  ~(test_cases : a list)
  ()
  =
  let test_cases =
    List.map test_cases ~f:(fun x -> { Test_case.input = x; output = x })
  in
  generate_args_and_test_roundtrip''
    ?separate_rows
    ~to_sexp_input:to_sexp
    ~to_sexp_output:to_sexp
    ~equal
    ~rcp
    ~test_cases
    ()
;;

let generate_args_and_test_roundtrip
  (type a)
  (module M : With_equal_and_rcp with type t = a)
  ?separate_rows
  test_cases
  =
  generate_args_and_test_roundtrip'
    ?separate_rows
    ~to_sexp:M.sexp_of_t
    ~equal:M.equal
    ~rcp:M.roundtrippable_command_param
    ~test_cases
    ()
;;

let print_patdiff str1 str2 =
  let anonymize_all_generated_symbols str =
    let regexp = Re.Str.regexp "__[0-9][0-9][0-9]_" in
    Re.Str.global_replace regexp "__###_" str
  in
  let format_str_pretty str =
    let contents = Driver.map_structure str |> Pprintast.string_of_structure in
    Apply_style_client.format ~file:(File_path.of_string "sample.ml") ~contents ()
    >>| ok_exn
    >>| anonymize_all_generated_symbols
  in
  let%bind left = format_str_pretty str1
  and right = format_str_pretty str2 in
  Expect_test_patdiff.print_patdiff ~location_style:None ~context:999999 left right;
  return ()
;;

(****** Test for Abstract type ******)

module Record : sig
  type t =
    { foo : bool
    ; bar : float
    }
  [@@deriving sexp_of, equal, fields ~iterators:make_creator]
  [@@deriving_inline roundtrippable_command_param]

  include sig
    [@@@ocaml.warning "-32"]

    val roundtrippable_command_param : (t, t) Roundtrippable_command_param.T2.t
    val param : t Command.Param.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t =
    { foo : bool
    ; bar : float
    }
  [@@deriving
    sexp_of, equal, fields ~iterators:make_creator, roundtrippable_command_param]
end

let%expect_test "generate command args Record and test roundtrip" =
  generate_args_and_test_roundtrip
    (module Record)
    [ { foo = true; bar = 1.0 }; { foo = false; bar = -0.07 } ];
  [%expect
    {|
    ┌───────┬───────┬───────────────────────┐
    │ foo   │ bar   │ args                  │
    ├───────┼───────┼───────────────────────┤
    │ true  │  1    │ -foo true -bar 1.     │
    │ false │ -0.07 │ -foo false -bar -0.07 │
    └───────┴───────┴───────────────────────┘
    |}];
  return ()
;;

let%expect_test "show generated code of Record" =
  let loc = Location.none in
  let record_no_ppx =
    [%str
      type t =
        { foo : bool
        ; bar : float
        }]
  in
  let record_with_ppx =
    [%str
      type t =
        { foo : bool
        ; bar : float
        }
      [@@deriving roundtrippable_command_param]]
  in
  let%bind () = print_patdiff record_no_ppx record_with_ppx in
  [%expect
    {|
      type t =
        { foo : bool
        ; bar : float
        }
    +|[@@deriving roundtrippable_command_param]
    +|
    +|include struct
    +|  let _ = fun (_ : t) -> ()
    +|
    +|  let roundtrippable_command_param =
    +|    let ppx_roundtrippable_command_param__###_ =
    +|      let open! Ppx_roundtrippable_command_param_runtime in
    +|      Roundtrippable_command_param.create_required
    +|        "foo"
    +|        (Roundtrippable_arg_type.arg_type roundtrippable_arg_type_bool)
    +|        ~doc:
    +|          (Roundtrippable_arg_type.arg_placeholder roundtrippable_arg_type_bool ^ " " ^ "")
    +|        ~to_string:
    +|          (Staged.unstage
    +|             (Roundtrippable_arg_type.to_string roundtrippable_arg_type_bool))
    +|    and ppx_roundtrippable_command_param__###_ =
    +|      let open! Ppx_roundtrippable_command_param_runtime in
    +|      Roundtrippable_command_param.create_required
    +|        "bar"
    +|        (Roundtrippable_arg_type.arg_type roundtrippable_arg_type_float)
    +|        ~doc:
    +|          (Roundtrippable_arg_type.arg_placeholder roundtrippable_arg_type_float
    +|           ^ " "
    +|           ^ "")
    +|        ~to_string:
    +|          (Staged.unstage
    +|             (Roundtrippable_arg_type.to_string roundtrippable_arg_type_float))
    +|    in
    +|    Roundtrippable_command_param.Record_builder.Bare.build_for_record
    +|      (Fields.make_creator
    +|         ~foo:
    +|           (Roundtrippable_command_param.Record_builder.field
    +|              ppx_roundtrippable_command_param__###_)
    +|         ~bar:
    +|           (Roundtrippable_command_param.Record_builder.field
    +|              ppx_roundtrippable_command_param__###_))
    +|  ;;
    +|
    +|  let _     = roundtrippable_command_param
    +|  let param = Roundtrippable_command_param.param roundtrippable_command_param
    +|  let _     = param
    +|end [@@ocaml.doc "@inline"] [@@merlin.hide]
    |}];
  return ()
;;

module Record_with_custom_type_name : sig
  type custom_type =
    { baz : bool
    ; qux : float
    }
  [@@deriving sexp_of, equal, roundtrippable_command_param]
end = struct
  type custom_type =
    { baz : bool
    ; qux : float
    }
  [@@deriving
    sexp_of, equal, fields ~iterators:make_creator, roundtrippable_command_param]
end

let%expect_test "generate command args Record_with_custom_type_name and test roundtrip" =
  generate_args_and_test_roundtrip'
    ~to_sexp:Record_with_custom_type_name.sexp_of_custom_type
    ~equal:Record_with_custom_type_name.equal_custom_type
    ~rcp:Record_with_custom_type_name.roundtrippable_command_param_custom_type
    ~test_cases:[ { baz = true; qux = 1.0 }; { baz = false; qux = -0.07 } ]
    ();
  [%expect
    {|
    ┌───────┬───────┬───────────────────────┐
    │ baz   │ qux   │ args                  │
    ├───────┼───────┼───────────────────────┤
    │ true  │  1    │ -baz true -qux 1.     │
    │ false │ -0.07 │ -baz false -qux -0.07 │
    └───────┴───────┴───────────────────────┘
    |}];
  return ()
;;

let%expect_test "compare generated code of t and custom type name" =
  let loc = Location.none in
  let type_name_t =
    [%str
      type t =
        { foo : bool
        ; bar : float
        }
      [@@deriving roundtrippable_command_param]]
  in
  let type_name_custom =
    [%str
      type custom_name =
        { foo : bool
        ; bar : float
        }
      [@@deriving roundtrippable_command_param]]
  in
  let%bind () = print_patdiff type_name_t type_name_custom in
  [%expect
    {|
    -|type t =
    +|type custom_name =
        { foo : bool
        ; bar : float
        }
      [@@deriving roundtrippable_command_param]

      include struct
    -|  let _ = fun (_ : t) -> ()
    +|  let _ = fun (_ : custom_name) -> ()

    -|  let roundtrippable_command_param =
    +|  let roundtrippable_command_param_custom_name =
          let ppx_roundtrippable_command_param__###_ =
            let open! Ppx_roundtrippable_command_param_runtime in
            Roundtrippable_command_param.create_required
              "foo"
              (Roundtrippable_arg_type.arg_type roundtrippable_arg_type_bool)
              ~doc:
                (Roundtrippable_arg_type.arg_placeholder roundtrippable_arg_type_bool ^ " " ^ "")
              ~to_string:
                (Staged.unstage
                   (Roundtrippable_arg_type.to_string roundtrippable_arg_type_bool))
          and ppx_roundtrippable_command_param__###_ =
            let open! Ppx_roundtrippable_command_param_runtime in
            Roundtrippable_command_param.create_required
              "bar"
              (Roundtrippable_arg_type.arg_type roundtrippable_arg_type_float)
              ~doc:
                (Roundtrippable_arg_type.arg_placeholder roundtrippable_arg_type_float
                 ^ " "
                 ^ "")
              ~to_string:
                (Staged.unstage
                   (Roundtrippable_arg_type.to_string roundtrippable_arg_type_float))
          in
          Roundtrippable_command_param.Record_builder.Bare.build_for_record
    -|      (Fields.make_creator
    +|      (Fields_of_custom_name.make_creator
               ~foo:
                 (Roundtrippable_command_param.Record_builder.field
                    ppx_roundtrippable_command_param__###_)
               ~bar:
                 (Roundtrippable_command_param.Record_builder.field
                    ppx_roundtrippable_command_param__###_))
        ;;

    -|  let _     = roundtrippable_command_param
    -|  let param = Roundtrippable_command_param.param roundtrippable_command_param
    +|  let _ = roundtrippable_command_param_custom_name
    +|
    +|  let param_custom_name =
    +|    Roundtrippable_command_param.param roundtrippable_command_param_custom_name
    +|  ;;
    -|  let _     = param
    +|
    +|  let _ = param_custom_name
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
    |}];
  return ()
;;

(****** Test for Abstract type ******)

module Abstract_type = struct
  type t = Record.t [@@deriving sexp_of, equal, roundtrippable_command_param]
end

let%expect_test "generate command args Abstract_type and test roundtrip" =
  generate_args_and_test_roundtrip
    (module Abstract_type)
    [ { foo = true; bar = 1.0 }; { foo = false; bar = -0.07 } ];
  [%expect
    {|
    ┌───────┬───────┬───────────────────────┐
    │ foo   │ bar   │ args                  │
    ├───────┼───────┼───────────────────────┤
    │ true  │  1    │ -foo true -bar 1.     │
    │ false │ -0.07 │ -foo false -bar -0.07 │
    └───────┴───────┴───────────────────────┘
    |}];
  return ()
;;

let%expect_test "show generated code of Abstract_type" =
  let loc = Location.none in
  let abstract_type_no_ppx = [%str type t = Record.t] in
  let abstract_type_with_ppx =
    [%str type t = Record.t [@@deriving roundtrippable_command_param]]
  in
  let%bind () = print_patdiff abstract_type_no_ppx abstract_type_with_ppx in
  [%expect
    {|
    -|type t = Record.t
    +|type t = Record.t [@@deriving roundtrippable_command_param]
    +|
    +|include struct
    +|  let _ = fun (_ : t) -> ()
    +|  let roundtrippable_command_param = Record.roundtrippable_command_param
    +|  let _ = roundtrippable_command_param
    +|  let param = Roundtrippable_command_param.param roundtrippable_command_param
    +|  let _ = param
    +|end [@@ocaml.doc "@inline"] [@@merlin.hide]
    |}];
  return ()
;;

module Abstract_type_with_custom_type_name = struct
  type custom = Record_with_custom_type_name.custom_type
  [@@deriving sexp_of, equal, roundtrippable_command_param]
end

let%expect_test "generate command args Abstract_type_with_custom_type_name and test \
                 roundtrip"
  =
  generate_args_and_test_roundtrip'
    ~to_sexp:Abstract_type_with_custom_type_name.sexp_of_custom
    ~equal:Abstract_type_with_custom_type_name.equal_custom
    ~rcp:Abstract_type_with_custom_type_name.roundtrippable_command_param_custom
    ~test_cases:[ { baz = true; qux = 1.0 }; { baz = false; qux = -0.07 } ]
    ();
  [%expect
    {|
    ┌───────┬───────┬───────────────────────┐
    │ baz   │ qux   │ args                  │
    ├───────┼───────┼───────────────────────┤
    │ true  │  1    │ -baz true -qux 1.     │
    │ false │ -0.07 │ -baz false -qux -0.07 │
    └───────┴───────┴───────────────────────┘
    |}];
  return ()
;;

let%expect_test "compare generated code of t and custom type name" =
  let loc = Location.none in
  let type_name_t = [%str type t = Record.t [@@deriving roundtrippable_command_param]] in
  let type_name_custom =
    [%str
      type custom = Record_with_custom_type_name.custom_type
      [@@deriving roundtrippable_command_param]]
  in
  let%bind () = print_patdiff type_name_t type_name_custom in
  [%expect
    {|
    -|type t = Record.t [@@deriving roundtrippable_command_param]
    +|type custom = Record_with_custom_type_name.custom_type
    +|[@@deriving roundtrippable_command_param]

      include struct
    -|  let _ = fun (_ : t) -> ()
    +|  let _ = fun (_ : custom) -> ()
    -|  let roundtrippable_command_param = Record.roundtrippable_command_param
    +|
    +|  let roundtrippable_command_param_custom =
    +|    Record_with_custom_type_name.roundtrippable_command_param_custom_type
    +|  ;;
    -|  let _ = roundtrippable_command_param
    +|
    +|  let _ = roundtrippable_command_param_custom
    -|  let param = Roundtrippable_command_param.param roundtrippable_command_param
    +|
    +|  let param_custom =
    +|    Roundtrippable_command_param.param roundtrippable_command_param_custom
    +|  ;;
    -|  let _ = param
    +|
    +|  let _ = param_custom
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
    |}];
  return ()
;;

(****** Test for attribute [roundtrippable_command_param] ******)

module Record_rcp_nested = struct
  type t =
    { use_first : bool
    ; use_second : bool
         [@roundtrippable_command_param
           Roundtrippable_command_param.create_no_arg
             "use-second"
             ~doc:"Use second if provided"]
    ; first : Record.t [@roundtrippable_command_param]
    ; second : Record_with_custom_type_name.custom_type [@roundtrippable_command_param]
    }
  [@@deriving
    sexp_of, equal, fields ~iterators:make_creator, roundtrippable_command_param]
end

let%expect_test "generate command args Record_rcp_nested and test roundtrip" =
  generate_args_and_test_roundtrip
    (module Record_rcp_nested)
    [ { use_first = true
      ; use_second = false
      ; first = { foo = true; bar = 1.0 }
      ; second = { baz = false; qux = -0.07 }
      }
    ; { use_first = false
      ; use_second = true
      ; first = { foo = false; bar = 10.2 }
      ; second = { baz = true; qux = 0.0 }
      }
    ];
  [%expect
    {|
    ┌───────────┬────────────┬───────────┬───────────┬────────────┬────────────┬─────────────────────────────────────────────────────────────────────┐
    │ use_first │ use_second │ first.foo │ first.bar │ second.baz │ second.qux │ args                                                                │
    ├───────────┼────────────┼───────────┼───────────┼────────────┼────────────┼─────────────────────────────────────────────────────────────────────┤
    │ true      │ false      │ true      │  1        │ false      │ -0.07      │ -use-first true -foo true -bar 1. -baz false -qux -0.07             │
    │ false     │ true       │ false     │ 10.2      │ true       │  0         │ -use-first false -use-second -foo false -bar 10.2 -baz true -qux 0. │
    └───────────┴────────────┴───────────┴───────────┴────────────┴────────────┴─────────────────────────────────────────────────────────────────────┘
    |}];
  return ()
;;

(****** Test for [roundtrippable_arg_type] attribute ******)

module Record_rcp_with_custom_arg_type = struct
  type other_type =
    | Foo
    | Bar
  [@@deriving sexp_of, enumerate, equal]

  let arg_type =
    Command.Arg_type.create (function
      | "Alt_Foo" -> Foo
      | "Alt_Bar" -> Bar
      | _ -> raise_s [%message "Unknown arg type"])
  ;;

  let to_string = function
    | Foo -> "Alt_Foo"
    | Bar -> "Alt_Bar"
  ;;

  let custom_roundtrippable_arg_type =
    Roundtrippable_arg_type.create ~arg_type ~to_string ~arg_placeholder:"ABC"
  ;;

  type t =
    { baz : bool
    ; qux : other_type
         [@roundtrippable_command_param.roundtrippable_arg_type
           custom_roundtrippable_arg_type]
    }
  [@@deriving
    sexp_of, equal, fields ~iterators:make_creator, roundtrippable_command_param]
end

let%expect_test "generate command args Record_rcp_with_custom_arg_type and test roundtrip"
  =
  generate_args_and_test_roundtrip
    (module Record_rcp_with_custom_arg_type)
    [ { baz = false; qux = Foo }; { baz = true; qux = Bar } ];
  [%expect
    {|
    ┌───────┬─────┬─────────────────────────┐
    │ baz   │ qux │ args                    │
    ├───────┼─────┼─────────────────────────┤
    │ false │ Foo │ -baz false -qux Alt_Foo │
    │ true  │ Bar │ -baz true -qux Alt_Bar  │
    └───────┴─────┴─────────────────────────┘
    |}];
  return ()
;;

module Record_rcp_arg_type = struct
  module Other_type = struct
    type t =
      | Foo
      | Bar
    [@@deriving sexp_of, enumerate, equal]

    let arg_type =
      Command.Arg_type.enumerated_sexpable
        (module struct
          type nonrec t = t [@@deriving sexp_of, enumerate]
        end)
    ;;

    let to_string x = [%sexp_of: t] x |> Sexp.to_string

    let roundtrippable_arg_type =
      Roundtrippable_arg_type.create ~arg_type ~to_string ~arg_placeholder:"ABC"
    ;;

    type another_type = bool [@@deriving sexp_of, enumerate, equal]

    let arg_type_another_type =
      Command.Arg_type.enumerated_sexpable
        (module struct
          type nonrec t = another_type [@@deriving sexp_of, enumerate]
        end)
    ;;

    let another_type_to_string x = [%sexp_of: another_type] x |> Sexp.to_string

    let roundtrippable_arg_type_another_type =
      Roundtrippable_arg_type.create
        ~arg_type:arg_type_another_type
        ~to_string:another_type_to_string
        ~arg_placeholder:"ABC"
    ;;
  end

  type t =
    { baz : Other_type.another_type
         [@roundtrippable_command_param.roundtrippable_arg_type]
    ; qux : Other_type.t [@roundtrippable_command_param.roundtrippable_arg_type]
    }
  [@@deriving
    sexp_of, equal, fields ~iterators:make_creator, roundtrippable_command_param]
end

let%expect_test "generate command args Record_rcp_arg_type and test roundtrip" =
  generate_args_and_test_roundtrip
    (module Record_rcp_arg_type)
    [ { baz = false; qux = Foo }; { baz = true; qux = Bar } ];
  [%expect
    {|
    ┌───────┬─────┬─────────────────────┐
    │ baz   │ qux │ args                │
    ├───────┼─────┼─────────────────────┤
    │ false │ Foo │ -baz false -qux Foo │
    │ true  │ Bar │ -baz true -qux Bar  │
    └───────┴─────┴─────────────────────┘
    |}];
  return ()
;;

(****** Test for [option] flags ******)

module Record_option = struct
  type t =
    { foo : float option
    ; bar : Record_rcp_arg_type.Other_type.another_type option
         [@roundtrippable_command_param.roundtrippable_arg_type]
    ; baz : Record_rcp_with_custom_arg_type.other_type option
         [@roundtrippable_command_param.roundtrippable_arg_type
           Record_rcp_with_custom_arg_type.custom_roundtrippable_arg_type]
    }
  [@@deriving
    sexp_of, equal, fields ~iterators:make_creator, roundtrippable_command_param]
end

let%expect_test "generate command args Record_option and test roundtrip" =
  generate_args_and_test_roundtrip
    (module Record_option)
    [ { foo = Some 1.0; bar = None; baz = None }
    ; { foo = None; bar = Some false; baz = Some Foo }
    ];
  [%expect
    {|
    ┌─────┬───────┬─────┬─────────────────────────┐
    │ foo │ bar   │ baz │ args                    │
    ├─────┼───────┼─────┼─────────────────────────┤
    │ 1   │       │     │ -foo 1.                 │
    │     │ false │ Foo │ -bar false -baz Alt_Foo │
    └─────┴───────┴─────┴─────────────────────────┘
    |}];
  return ()
;;

let%expect_test "compare generated code of option and non-option fields" =
  let loc = Location.none in
  let str_record =
    [%str
      type t =
        { foo : bool
        ; bar : float
        }
      [@@deriving roundtrippable_command_param]]
  in
  let str_record_with_optional_fields =
    [%str
      type t =
        { foo : bool
        ; bar : float option
        }
      [@@deriving roundtrippable_command_param]]
  in
  let%bind () = print_patdiff str_record str_record_with_optional_fields in
  [%expect
    {|
      type t =
        { foo : bool
    -|  ; bar : float
    +|  ; bar : float option
        }
      [@@deriving roundtrippable_command_param]

      include struct
        let _ = fun (_ : t) -> ()

        let roundtrippable_command_param =
          let ppx_roundtrippable_command_param__###_ =
            let open! Ppx_roundtrippable_command_param_runtime in
            Roundtrippable_command_param.create_required
              "foo"
              (Roundtrippable_arg_type.arg_type roundtrippable_arg_type_bool)
              ~doc:
                (Roundtrippable_arg_type.arg_placeholder roundtrippable_arg_type_bool ^ " " ^ "")
              ~to_string:
                (Staged.unstage
                   (Roundtrippable_arg_type.to_string roundtrippable_arg_type_bool))
          and ppx_roundtrippable_command_param__###_ =
            let open! Ppx_roundtrippable_command_param_runtime in
    -|      Roundtrippable_command_param.create_required
    +|      Roundtrippable_command_param.create_optional
              "bar"
              (Roundtrippable_arg_type.arg_type roundtrippable_arg_type_float)
              ~doc:
                (Roundtrippable_arg_type.arg_placeholder roundtrippable_arg_type_float
                 ^ " "
                 ^ "")
              ~to_string:
                (Staged.unstage
                   (Roundtrippable_arg_type.to_string roundtrippable_arg_type_float))
          in
          Roundtrippable_command_param.Record_builder.Bare.build_for_record
            (Fields.make_creator
               ~foo:
                 (Roundtrippable_command_param.Record_builder.field
                    ppx_roundtrippable_command_param__###_)
               ~bar:
                 (Roundtrippable_command_param.Record_builder.field
                    ppx_roundtrippable_command_param__###_))
        ;;

        let _     = roundtrippable_command_param
        let param = Roundtrippable_command_param.param roundtrippable_command_param
        let _     = param
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
    |}];
  return ()
;;

(****** Test for [list] flags ******)

module Record_list = struct
  type t =
    { foos : float list [@name "-foo-flag"]
    ; bars : int list [@roundtrippable_command_param.list_method listed] [@name "bar"]
    ; bazs : string list [@roundtrippable_command_param.list_method comma_separated]
    }
  [@@deriving
    sexp_of, equal, fields ~iterators:make_creator, roundtrippable_command_param]
end

let%expect_test "generate command args Record_list and test roundtrip" =
  generate_args_and_test_roundtrip
    (module Record_list)
    ~separate_rows:true
    [ { foos = []; bars = []; bazs = [] }
    ; { foos = [ 1.0 ]; bars = [ 1 ]; bazs = [ "1" ] }
    ; { foos = [ 1.0; 2.0 ]; bars = [ 1; 2 ]; bazs = [ "1"; "2" ] }
    ];
  [%expect
    {|
    ┌──────┬──────┬──────┬───────────────────────────────────────────────────┐
    │ foos │ bars │ bazs │ args                                              │
    ├──────┼──────┼──────┼───────────────────────────────────────────────────┤
    │      │      │      │ EMPTY                                             │
    ├──────┼──────┼──────┼───────────────────────────────────────────────────┤
    │ 1    │ 1    │ 1    │ -foo-flag 1. -bar 1 -bazs 1                       │
    ├──────┼──────┼──────┼───────────────────────────────────────────────────┤
    │ 1    │ 1    │ 1    │ -foo-flag 1. -foo-flag 2. -bar 1 -bar 2 -bazs 1,2 │
    │ 2    │ 2    │ 2    │                                                   │
    └──────┴──────┴──────┴───────────────────────────────────────────────────┘
    |}];
  return ()
;;

let%expect_test "compare generated code of list and non-list fields" =
  let loc = Location.none in
  let str_record =
    [%str
      type t =
        { foo : bool
        ; bar : float
        }
      [@@deriving roundtrippable_command_param]]
  in
  let str_record_with_list_fields =
    [%str
      type t =
        { foo : bool
        ; bar : float list
        }
      [@@deriving roundtrippable_command_param]]
  in
  let%bind () = print_patdiff str_record str_record_with_list_fields in
  [%expect
    {|
      type t =
        { foo : bool
    -|  ; bar : float
    +|  ; bar : float list
        }
      [@@deriving roundtrippable_command_param]

      include struct
        let _ = fun (_ : t) -> ()

        let roundtrippable_command_param =
          let ppx_roundtrippable_command_param__###_ =
            let open! Ppx_roundtrippable_command_param_runtime in
            Roundtrippable_command_param.create_required
              "foo"
              (Roundtrippable_arg_type.arg_type roundtrippable_arg_type_bool)
              ~doc:
                (Roundtrippable_arg_type.arg_placeholder roundtrippable_arg_type_bool ^ " " ^ "")
              ~to_string:
                (Staged.unstage
                   (Roundtrippable_arg_type.to_string roundtrippable_arg_type_bool))
          and ppx_roundtrippable_command_param__###_ =
            let open! Ppx_roundtrippable_command_param_runtime in
    -|      Roundtrippable_command_param.create_required
    +|      Roundtrippable_command_param.create_listed
              "bar"
              (Roundtrippable_arg_type.arg_type roundtrippable_arg_type_float)
              ~doc:
                (Roundtrippable_arg_type.arg_placeholder roundtrippable_arg_type_float
                 ^ " "
    -|           ^ "")
    +|           ^ ""
    +|           ^ " (can be passed multiple times)")
              ~to_string:
                (Staged.unstage
                   (Roundtrippable_arg_type.to_string roundtrippable_arg_type_float))
          in
          Roundtrippable_command_param.Record_builder.Bare.build_for_record
            (Fields.make_creator
               ~foo:
                 (Roundtrippable_command_param.Record_builder.field
                    ppx_roundtrippable_command_param__###_)
               ~bar:
                 (Roundtrippable_command_param.Record_builder.field
                    ppx_roundtrippable_command_param__###_))
        ;;

        let _     = roundtrippable_command_param
        let param = Roundtrippable_command_param.param roundtrippable_command_param
        let _     = param
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
    |}];
  return ()
;;

(****** Test for integration with [or_default] ******)

module Record_including_defaults : sig
  type t =
    { foo : bool [@default]
    ; bar : float (** docstring *)
    ; baz : string option
    }
  [@@deriving or_default, roundtrippable_command_param, sexp_of, equal]
end = struct
  type t =
    { foo : bool [@default false]
    ; bar : float (** docstring *)
    ; baz : string option
    }
  [@@deriving
    fields ~iterators:make_creator
    , or_default
    , roundtrippable_command_param
    , sexp_of
    , equal]
end

let%expect_test "generate command args Record_including_defaults and test roundtrip" =
  let test_cases : Record_including_defaults.With_defaults.t list =
    [ { foo = Default; bar = -1.7; baz = Some "foo" }
    ; { foo = Custom false; bar = 1.; baz = None }
    ; { foo = Custom true; bar = 1.; baz = None }
    ]
  in
  generate_args_and_test_roundtrip''
    ~to_sexp_input:[%sexp_of: Record_including_defaults.With_defaults.t]
    ~to_sexp_output:[%sexp_of: Record_including_defaults.t]
    ~equal:[%equal: Record_including_defaults.t]
    ~rcp:Record_including_defaults.roundtrippable_command_param
    ~test_cases:
      (List.map test_cases ~f:(fun x ->
         { Test_case.input = x
         ; output = Record_including_defaults.With_defaults.resolve x
         }))
    ();
  [%expect
    {|
    ┌────────────────┬──────┬─────┬────────────────────┐
    │ foo            │ bar  │ baz │ args               │
    ├────────────────┼──────┼─────┼────────────────────┤
    │ Default        │ -1.7 │ foo │ -bar -1.7 -baz foo │
    │ (Custom false) │  1   │     │ -foo false -bar 1. │
    │ (Custom true)  │  1   │     │ -foo true -bar 1.  │
    └────────────────┴──────┴─────┴────────────────────┘
    |}];
  return ()
;;

let%expect_test "compare generated code of field with and without default" =
  let loc = Location.none in
  let str_record =
    [%str
      type t =
        { foo : bool
        ; bar : float
        }
      [@@deriving roundtrippable_command_param]]
  in
  let str_record_with_defaults =
    [%str
      type t =
        { foo : bool [@default false]
        ; bar : float
        }
      [@@deriving roundtrippable_command_param]]
  in
  let%bind () = print_patdiff str_record str_record_with_defaults in
  [%expect
    {|
    +|[%%ocaml.error "Attribute `default' was not used"]
    +|
      type t =
    -|  { foo : bool
    +|  { foo : bool [@default false]
        ; bar : float
        }
      [@@deriving roundtrippable_command_param]

      include struct
        let _ = fun (_ : t) -> ()

        let roundtrippable_command_param =
          let ppx_roundtrippable_command_param__###_ =
            let open! Ppx_roundtrippable_command_param_runtime in
    -|      Roundtrippable_command_param.create_required
    +|      Or_default.create_optional_param_with_default_doc'
              "foo"
              (Roundtrippable_arg_type.arg_type roundtrippable_arg_type_bool)
    +|        ~default:false
              ~doc:
                (Roundtrippable_arg_type.arg_placeholder roundtrippable_arg_type_bool ^ " " ^ "")
              ~to_string:
                (Staged.unstage
                   (Roundtrippable_arg_type.to_string roundtrippable_arg_type_bool))
          and ppx_roundtrippable_command_param__###_ =
            let open! Ppx_roundtrippable_command_param_runtime in
            Roundtrippable_command_param.create_required
              "bar"
              (Roundtrippable_arg_type.arg_type roundtrippable_arg_type_float)
              ~doc:
                (Roundtrippable_arg_type.arg_placeholder roundtrippable_arg_type_float
                 ^ " "
                 ^ "")
              ~to_string:
                (Staged.unstage
                   (Roundtrippable_arg_type.to_string roundtrippable_arg_type_float))
          in
          Roundtrippable_command_param.Record_builder.Bare.build_for_record
            (Fields.make_creator
               ~foo:
    -|           (Roundtrippable_command_param.Record_builder.field
    +|           (Roundtrippable_command_param.Record_builder.Bare.field
    +|              (Roundtrippable_command_param.contra_map
    -|              ppx_roundtrippable_command_param__###_)
    +|                 ppx_roundtrippable_command_param__###_
    +|                 ~f:With_defaults.foo))
    -|         ~bar:
    -|           (Roundtrippable_command_param.Record_builder.field
    +|         ~bar:
    +|           (Roundtrippable_command_param.Record_builder.Bare.field
    +|              (Roundtrippable_command_param.contra_map
    -|              ppx_roundtrippable_command_param__###_))
    +|                 ppx_roundtrippable_command_param__###_
    +|                 ~f:With_defaults.bar)))
        ;;

        let _     = roundtrippable_command_param
        let param = Roundtrippable_command_param.param roundtrippable_command_param
        let _     = param
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
    |}];
  return ()
;;

module Record_with_defaults_and_attributes = struct
  module Baz = struct
    type baz = { baz : bool [@default false] }
    [@@deriving
      fields ~iterators:make_creator
      , or_default
      , sexp_of
      , equal
      , roundtrippable_command_param]
  end

  type t =
    { foo : Record_rcp_arg_type.Other_type.another_type
         [@roundtrippable_arg_type] [@default false]
    ; bar : Record_rcp_with_custom_arg_type.other_type
         [@roundtrippable_command_param.roundtrippable_arg_type
           Record_rcp_with_custom_arg_type.custom_roundtrippable_arg_type]
    ; baz : Baz.baz [@with_defaults]
    }
  [@@deriving
    fields ~iterators:make_creator
    , or_default
    , sexp_of
    , equal
    , roundtrippable_command_param]
end

let%expect_test "generate command args Record_with_defaults_and_attributes and test \
                 roundtrip"
  =
  let test_cases : Record_with_defaults_and_attributes.With_defaults.t list =
    [ { foo = Default; bar = Foo; baz = { baz = Default } }
    ; { foo = Custom true; bar = Bar; baz = { baz = Custom true } }
    ]
  in
  generate_args_and_test_roundtrip''
    ~to_sexp_input:Record_with_defaults_and_attributes.With_defaults.sexp_of_t
    ~to_sexp_output:Record_with_defaults_and_attributes.sexp_of_t
    ~equal:Record_with_defaults_and_attributes.equal
    ~rcp:Record_with_defaults_and_attributes.roundtrippable_command_param
    ~test_cases:
      (List.map test_cases ~f:(fun x ->
         { Test_case.input = x
         ; output = Record_with_defaults_and_attributes.With_defaults.resolve x
         }))
    ();
  [%expect
    {|
    ┌───────────────┬─────┬───────────────┬──────────────────────────────────┐
    │ foo           │ bar │ baz.baz       │ args                             │
    ├───────────────┼─────┼───────────────┼──────────────────────────────────┤
    │ Default       │ Foo │ Default       │ -bar Alt_Foo                     │
    │ (Custom true) │ Bar │ (Custom true) │ -foo true -bar Alt_Bar -baz true │
    └───────────────┴─────┴───────────────┴──────────────────────────────────┘
    |}];
  return ()
;;

module Record_with_defaults_custom_type_name : sig
  type custom_type =
    { foo : bool [@default]
    ; bar : float
    ; baz : string option
    }
  [@@deriving or_default, roundtrippable_command_param, sexp_of, equal]
end = struct
  type custom_type =
    { foo : bool [@default false]
    ; bar : float (** docstring *)
    ; baz : string option
    }
  [@@deriving
    fields ~iterators:make_creator
    , or_default
    , roundtrippable_command_param
    , sexp_of
    , equal]
end

let%expect_test "generate command args Record_with_defaults_custom_type_name and test \
                 roundtrip"
  =
  let test_cases
    : Record_with_defaults_custom_type_name.Custom_type_with_defaults.custom_type list
    =
    [ { foo = Default; bar = -1.7; baz = Some "foo" }
    ; { foo = Custom false; bar = 1.; baz = None }
    ]
  in
  generate_args_and_test_roundtrip''
    ~to_sexp_input:
      [%sexp_of:
        Record_with_defaults_custom_type_name.Custom_type_with_defaults.custom_type]
    ~to_sexp_output:Record_with_defaults_custom_type_name.sexp_of_custom_type
    ~equal:Record_with_defaults_custom_type_name.equal_custom_type
    ~rcp:Record_with_defaults_custom_type_name.roundtrippable_command_param_custom_type
    ~test_cases:
      (List.map test_cases ~f:(fun x ->
         { Test_case.input = x
         ; output =
             Record_with_defaults_custom_type_name.Custom_type_with_defaults
             .resolve_custom_type
               x
         }))
    ();
  [%expect
    {|
    ┌────────────────┬──────┬─────┬────────────────────┐
    │ foo            │ bar  │ baz │ args               │
    ├────────────────┼──────┼─────┼────────────────────┤
    │ Default        │ -1.7 │ foo │ -bar -1.7 -baz foo │
    │ (Custom false) │  1   │     │ -foo false -bar 1. │
    └────────────────┴──────┴─────┴────────────────────┘
    |}];
  return ()
;;

let%expect_test "compare generated code of t and custom type name with defaults" =
  let loc = Location.none in
  let str_record =
    [%str
      type t =
        { foo : bool [@default false]
        ; bar : float
        }
      [@@deriving roundtrippable_command_param]]
  in
  let str_record_custom_type_name =
    [%str
      type custom_name =
        { foo : bool [@default false]
        ; bar : float
        }
      [@@deriving roundtrippable_command_param]]
  in
  let%bind () = print_patdiff str_record str_record_custom_type_name in
  [%expect
    {|
      [%%ocaml.error "Attribute `default' was not used"]

    -|type t =
    +|type custom_name =
        { foo : bool [@default false]
        ; bar : float
        }
      [@@deriving roundtrippable_command_param]

      include struct
    -|  let _ = fun (_ : t) -> ()
    +|  let _ = fun (_ : custom_name) -> ()

    -|  let roundtrippable_command_param =
    +|  let roundtrippable_command_param_custom_name =
          let ppx_roundtrippable_command_param__###_ =
            let open! Ppx_roundtrippable_command_param_runtime in
            Or_default.create_optional_param_with_default_doc'
              "foo"
              (Roundtrippable_arg_type.arg_type roundtrippable_arg_type_bool)
              ~default:false
              ~doc:
                (Roundtrippable_arg_type.arg_placeholder roundtrippable_arg_type_bool ^ " " ^ "")
              ~to_string:
                (Staged.unstage
                   (Roundtrippable_arg_type.to_string roundtrippable_arg_type_bool))
          and ppx_roundtrippable_command_param__###_ =
            let open! Ppx_roundtrippable_command_param_runtime in
            Roundtrippable_command_param.create_required
              "bar"
              (Roundtrippable_arg_type.arg_type roundtrippable_arg_type_float)
              ~doc:
                (Roundtrippable_arg_type.arg_placeholder roundtrippable_arg_type_float
                 ^ " "
                 ^ "")
              ~to_string:
                (Staged.unstage
                   (Roundtrippable_arg_type.to_string roundtrippable_arg_type_float))
          in
          Roundtrippable_command_param.Record_builder.Bare.build_for_record
    -|      (Fields.make_creator
    +|      (Fields_of_custom_name.make_creator
               ~foo:
                 (Roundtrippable_command_param.Record_builder.Bare.field
                    (Roundtrippable_command_param.contra_map
                       ppx_roundtrippable_command_param__###_
    -|                 ~f:With_defaults.foo))
    +|                 ~f:Custom_name_with_defaults.foo))
               ~bar:
                 (Roundtrippable_command_param.Record_builder.Bare.field
                    (Roundtrippable_command_param.contra_map
                       ppx_roundtrippable_command_param__###_
    -|                 ~f:With_defaults.bar)))
    +|                 ~f:Custom_name_with_defaults.bar)))
        ;;

    -|  let _     = roundtrippable_command_param
    -|  let param = Roundtrippable_command_param.param roundtrippable_command_param
    +|  let _ = roundtrippable_command_param_custom_name
    +|
    +|  let param_custom_name =
    +|    Roundtrippable_command_param.param roundtrippable_command_param_custom_name
    +|  ;;
    -|  let _     = param
    +|
    +|  let _ = param_custom_name
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
    |}];
  return ()
;;

(* test signature generation matches implementation *)
module _ : sig
  module Foo : sig
    type t = { foo : int [@default] }
    [@@deriving or_default, roundtrippable_command_param]
  end

  type t = { foo : Foo.t [@with_defaults] }
  [@@deriving or_default, roundtrippable_command_param]
end = struct
  module Foo = struct
    type t = { foo : int [@default 1] }
    [@@deriving fields ~iterators:make_creator, or_default, roundtrippable_command_param]
  end

  type t = { foo : Foo.t [@with_defaults] }
  [@@deriving fields ~iterators:make_creator, or_default, roundtrippable_command_param]
end

(****** Test for attribute [bool_no_arg] ******)

module Record_bool_no_arg = struct
  type t =
    { foo : bool
    ; bar : bool [@bool_no_arg]
    }
  [@@deriving
    sexp_of, equal, fields ~iterators:make_creator, roundtrippable_command_param]
end

let%expect_test "generate command args Record_bool_no_arg and test roundtrip" =
  generate_args_and_test_roundtrip
    (module Record_bool_no_arg)
    [ { foo = true; bar = true }; { foo = false; bar = false } ];
  [%expect
    {|
    ┌───────┬───────┬────────────────┐
    │ foo   │ bar   │ args           │
    ├───────┼───────┼────────────────┤
    │ true  │ true  │ -foo true -bar │
    │ false │ false │ -foo false     │
    └───────┴───────┴────────────────┘
    |}];
  return ()
;;

let%expect_test "show generated code of using bool_no_arg attribute" =
  let loc = Location.none in
  let str_record =
    [%str
      type t =
        { foo : bool
        ; bar : float
        }
      [@@deriving roundtrippable_command_param]]
  in
  let str_record_bool_no_arg =
    [%str
      type t =
        { foo : bool [@bool_no_arg]
        ; bar : float
        }
      [@@deriving roundtrippable_command_param]]
  in
  let%bind () = print_patdiff str_record str_record_bool_no_arg in
  [%expect
    {|
      type t =
    -|  { foo : bool
    +|  { foo : bool [@bool_no_arg]
        ; bar : float
        }
      [@@deriving roundtrippable_command_param]

      include struct
        let _ = fun (_ : t) -> ()

        let roundtrippable_command_param =
          let ppx_roundtrippable_command_param__###_ =
    -|      let open! Ppx_roundtrippable_command_param_runtime in
    -|      Roundtrippable_command_param.create_required
    -|        "foo"
    -|        (Roundtrippable_arg_type.arg_type roundtrippable_arg_type_bool)
    -|        ~doc:
    -|          (Roundtrippable_arg_type.arg_placeholder roundtrippable_arg_type_bool ^ " " ^ "")
    -|        ~to_string:
    -|          (Staged.unstage
    -|             (Roundtrippable_arg_type.to_string roundtrippable_arg_type_bool))
    +|      Roundtrippable_command_param.create_no_arg "foo" ~doc:""
          and ppx_roundtrippable_command_param__###_ =
            let open! Ppx_roundtrippable_command_param_runtime in
            Roundtrippable_command_param.create_required
              "bar"
              (Roundtrippable_arg_type.arg_type roundtrippable_arg_type_float)
              ~doc:
                (Roundtrippable_arg_type.arg_placeholder roundtrippable_arg_type_float
                 ^ " "
                 ^ "")
              ~to_string:
                (Staged.unstage
                   (Roundtrippable_arg_type.to_string roundtrippable_arg_type_float))
          in
          Roundtrippable_command_param.Record_builder.Bare.build_for_record
            (Fields.make_creator
               ~foo:
                 (Roundtrippable_command_param.Record_builder.field
                    ppx_roundtrippable_command_param__###_)
               ~bar:
                 (Roundtrippable_command_param.Record_builder.field
                    ppx_roundtrippable_command_param__###_))
        ;;

        let _     = roundtrippable_command_param
        let param = Roundtrippable_command_param.param roundtrippable_command_param
        let _     = param
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
    |}];
  return ()
;;
