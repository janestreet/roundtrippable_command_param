open! Core
open! Test_helpers

module _ : module type of Roundtrippable_command_param = struct
  (* helpers *)

  (* types *)

  module T2 = struct
    type ('a, 'b) t = ('a, 'b) Roundtrippable_command_param.T2.t

    (* low-level, not tested directly *)
    let create = Roundtrippable_command_param.T2.create
  end

  type 'a t = 'a Roundtrippable_command_param.t

  (* conversions, used in test helpers and in many tests below *)

  let command_args = Roundtrippable_command_param.command_args
  let param = Roundtrippable_command_param.param
  let name_with_dash = Roundtrippable_command_param.name_with_dash

  (* untested *)

  let contra_map = Roundtrippable_command_param.contra_map
  let create_no_arg = Roundtrippable_command_param.create_no_arg
  let create_optional_from_enum = Roundtrippable_command_param.create_optional_from_enum
  let create_required_from_enum = Roundtrippable_command_param.create_required_from_enum
  let int_option = Roundtrippable_command_param.int_option
  let string_option = Roundtrippable_command_param.string_option

  (* tested in [test_record_builder.ml] *)

  module Record_builder = Roundtrippable_command_param.Record_builder

  let optional_params = Roundtrippable_command_param.optional_params

  module Let_syntax = Roundtrippable_command_param.Let_syntax

  include (
    Roundtrippable_command_param : Applicative.S2 with type ('a, 'b) t := ('a, 'b) T2.t)

  (* individual tests follow *)

  let create_required = Roundtrippable_command_param.create_required

  let%expect_test "Roundtrip flag" =
    let example1 =
      Roundtrippable_command_param.create_required
        "ex1"
        Command.Param.int
        ~to_string:Int.to_string
        ~doc:"doc"
    in
    print_param_flags example1;
    [%expect
      {|
      (((name "-ex1 _") (doc doc) (aliases ()))
       ((name [-help]) (doc "print this help text and exit") (aliases (-?))))
      |}];
    print_s
      [%sexp (Roundtrippable_command_param.command_args example1 1005 : string list)];
    [%expect {| (-ex1 1005) |}]
  ;;

  let create_optional = Roundtrippable_command_param.create_optional

  let%expect_test "Roundtrip flag" =
    let ex2 =
      Roundtrippable_command_param.create_optional
        "ex2"
        ~doc:"doc"
        Command.Param.string
        ~to_string:Fn.id
    in
    print_param_flags ex2;
    [%expect
      {|
      (((name "[-ex2 _]") (doc doc) (aliases ()))
       ((name [-help]) (doc "print this help text and exit") (aliases (-?))))
      |}];
    print_s
      [%sexp
        (Roundtrippable_command_param.command_args ex2 (Some "test-value") : string list)];
    [%expect {| (-ex2 test-value) |}];
    print_s [%sexp (Roundtrippable_command_param.command_args ex2 None : string list)];
    [%expect {| () |}]
  ;;

  let create_listed = Roundtrippable_command_param.create_listed

  let%expect_test "round trip param from [create_listed]" =
    let ex3 =
      Roundtrippable_command_param.create_listed
        "ex3"
        ~doc:"doc"
        Command.Param.string
        ~to_string:Fn.id
    in
    print_param_flags ex3;
    [%expect
      {|
      (((name "[-ex3 _] ...") (doc doc) (aliases ()))
       ((name [-help]) (doc "print this help text and exit") (aliases (-?))))
      |}];
    print_s [%sexp (Roundtrippable_command_param.command_args ex3 [] : string list)];
    [%expect {| () |}];
    print_s
      [%sexp (Roundtrippable_command_param.command_args ex3 [ "foo" ] : string list)];
    [%expect {| (-ex3 foo) |}];
    print_s
      [%sexp
        (Roundtrippable_command_param.command_args ex3 [ "foo"; "bar" ] : string list)];
    [%expect {| (-ex3 foo -ex3 bar) |}]
  ;;

  let create_one_or_more_as_pair = Roundtrippable_command_param.create_one_or_more_as_pair

  let%expect_test "round trip param from [create_one_or_more_as_pair]" =
    let ex3 =
      Roundtrippable_command_param.create_one_or_more_as_pair
        "one-or-more"
        ~doc:"doc"
        Command.Param.string
        ~to_string:Fn.id
    in
    print_param_flags ex3;
    [%expect
      {|
      (((name "-one-or-more _ ...") (doc doc) (aliases ()))
       ((name [-help]) (doc "print this help text and exit") (aliases (-?))))
      |}];
    print_s
      [%sexp (Roundtrippable_command_param.command_args ex3 ("foo", []) : string list)];
    [%expect {| (-one-or-more foo) |}];
    print_s
      [%sexp
        (Roundtrippable_command_param.command_args ex3 ("foo", [ "bar" ]) : string list)];
    [%expect {| (-one-or-more foo -one-or-more bar) |}];
    print_s
      [%sexp
        (Roundtrippable_command_param.command_args ex3 ("foo", [ "bar"; "baz" ])
         : string list)];
    [%expect {| (-one-or-more foo -one-or-more bar -one-or-more baz) |}]
  ;;

  let create_one_or_more_as_list = Roundtrippable_command_param.create_one_or_more_as_list

  let%expect_test "round trip param from [create_one_or_more_as_list]" =
    let ex3 =
      Roundtrippable_command_param.create_one_or_more_as_list
        "one-or-more"
        ~doc:"doc"
        Command.Param.string
        ~to_string:Fn.id
    in
    print_param_flags ex3;
    [%expect
      {|
      (((name "-one-or-more _ ...") (doc doc) (aliases ()))
       ((name [-help]) (doc "print this help text and exit") (aliases (-?))))
      |}];
    print_s
      [%sexp (Roundtrippable_command_param.command_args ex3 [ "foo" ] : string list)];
    [%expect {| (-one-or-more foo) |}];
    print_s
      [%sexp
        (Roundtrippable_command_param.command_args ex3 [ "foo"; "bar" ] : string list)];
    [%expect {| (-one-or-more foo -one-or-more bar) |}];
    print_s
      [%sexp
        (Roundtrippable_command_param.command_args ex3 [ "foo"; "bar"; "baz" ]
         : string list)];
    [%expect {| (-one-or-more foo -one-or-more bar -one-or-more baz) |}]
  ;;

  let create_optional_with_default =
    Roundtrippable_command_param.create_optional_with_default
  ;;

  let%expect_test "round trip param from [create_optional_with_default]" =
    let owd =
      Roundtrippable_command_param.create_optional_with_default
        "owd"
        ~doc:"owd doc"
        Command.Param.int
        ~to_string:Int.to_string
        ~default:55
    in
    print_param_flags owd;
    [%expect
      {|
      (((name "[-owd owd]") (doc "doc (default: 55)") (aliases ()))
       ((name [-help]) (doc "print this help text and exit") (aliases (-?))))
      |}];
    print_s [%sexp (Roundtrippable_command_param.command_args owd None : string list)];
    [%expect {| () |}];
    print_s
      [%sexp (Roundtrippable_command_param.command_args owd (Some 42) : string list)];
    [%expect {| (-owd 42) |}];
    let owd =
      Roundtrippable_command_param.create_optional_with_default
        "owd"
        ~doc:"owd doc"
        Command.Param.int
        ~default_value_doc_string:"default_value_doc_string"
        ~to_string:Int.to_string
        ~default:55
    in
    print_param_flags owd;
    [%expect
      {|
      (((name "[-owd owd]") (doc "doc (default: default_value_doc_string)")
        (aliases ()))
       ((name [-help]) (doc "print this help text and exit") (aliases (-?))))
      |}]
  ;;

  let create_optional_with_default_drop_default =
    Roundtrippable_command_param.create_optional_with_default_drop_default
  ;;

  let%expect_test "round trip param from [create_optional_with_default_drop_default]" =
    let default = 55 in
    let not_the_default = 42 in
    let owd_dd =
      create_optional_with_default_drop_default
        "owd-dd"
        Command.Param.int
        ~default
        ~equal:Int.equal
        ~to_string:Int.to_string
        ~doc:"owd_dd doc"
    in
    print_param_flags owd_dd;
    [%expect
      {|
      (((name "[-owd-dd owd_dd]") (doc "doc (default: 55)") (aliases ()))
       ((name [-help]) (doc "print this help text and exit") (aliases (-?))))
      |}];
    print_s
      [%sexp (Roundtrippable_command_param.command_args owd_dd default : string list)];
    [%expect {| () |}];
    print_s
      [%sexp
        (Roundtrippable_command_param.command_args owd_dd not_the_default : string list)];
    [%expect {| (-owd-dd 42) |}]
  ;;

  let alias_params = Roundtrippable_command_param.alias_params

  let%expect_test "round trip param from [alias_params] default value" =
    let open Command.Param in
    let open Roundtrippable_command_param in
    let alternative = flag "alternative" (optional int) ~doc:"int param alternative" in
    let param =
      create_optional "param" float ~doc:"float param" ~to_string:Float.to_string
      |> map ~f:(Option.map ~f:Int.of_float)
      |> contra_map ~f:(fun x -> Some (Int.to_float x))
    in
    let param = alias_params param [ alternative ] ~if_nothing_chosen:(Default_to 5) in
    print_param_flags param;
    [%expect
      {|
      (((name "[-alternative int]") (doc "param alternative") (aliases ()))
       ((name "[-param float]") (doc param) (aliases ()))
       ((name [-help]) (doc "print this help text and exit") (aliases (-?))))
      |}];
    print_s [%sexp (Roundtrippable_command_param.command_args param 42 : string list)];
    [%expect {| (-param 42.) |}]
  ;;

  let%expect_test "round trip param from [alias_params] return none" =
    let open Command.Param in
    let open Roundtrippable_command_param in
    let alternative = flag "alternative" (optional int) ~doc:"int param alternative" in
    let param =
      create_optional "param" float ~doc:"float param" ~to_string:Float.to_string
      |> map ~f:(Option.map ~f:Int.of_float)
      |> contra_map ~f:(Option.map ~f:Int.to_float)
    in
    let param = alias_params param [ alternative ] ~if_nothing_chosen:Return_none in
    print_param_flags param;
    [%expect
      {|
      (((name "[-alternative int]") (doc "param alternative") (aliases ()))
       ((name "[-param float]") (doc param) (aliases ()))
       ((name [-help]) (doc "print this help text and exit") (aliases (-?))))
      |}];
    print_s [%sexp (Roundtrippable_command_param.command_args param None : string list)];
    [%expect {| () |}]
  ;;

  let create_flags_from_enum = Roundtrippable_command_param.create_flags_from_enum

  open struct
    module Enumeration = struct
      type t =
        | Everything
        | Just_one
        | Nothing
      [@@deriving enumerate, sexp]
    end
  end

  let%expect_test "round trip param from [create_flags_from_enum]" =
    let open Roundtrippable_command_param in
    let open Enumeration in
    let param =
      create_flags_from_enum
        (module Enumeration)
        ~doc:(function
          | Everything -> "run against everything"
          | Just_one -> "run against just one"
          | Nothing -> "don't actually run")
        ~aliases:(function
          | Nothing -> [ "do-not-run"; "-never-run" ]
          | Just_one | Everything -> [])
    in
    print_param_flags param;
    [%expect
      {|
      (((name [-everything]) (doc "run against everything") (aliases ()))
       ((name [-just-one]) (doc "run against just one") (aliases ()))
       ((name [-nothing]) (doc "don't actually run")
        (aliases (-do-not-run -never-run)))
       ((name [-help]) (doc "print this help text and exit") (aliases (-?))))
      |}];
    Enumeration.all
    |> List.map ~f:(fun t -> t, command_args param t)
    |> [%sexp_of: (t * string list) list]
    |> print_s;
    [%expect
      {| ((Everything (-everything)) (Just_one (-just-one)) (Nothing (-nothing))) |}]
  ;;

  let create_optional_with_default_from_enum =
    Roundtrippable_command_param.create_optional_with_default_from_enum
  ;;

  let%expect_test "param from [create_optional_with_default_from_enum] looks reasonable" =
    let param =
      Roundtrippable_command_param.create_optional_with_default_from_enum
        "flag"
        (module Enumeration)
        ~doc:"doc"
        ~default:Everything
    in
    print_param_flags param;
    [%expect
      {|
      (((name "[-flag _]")
        (doc "doc (default: everything) (can be: everything, just-one, nothing)")
        (aliases ()))
       ((name [-help]) (doc "print this help text and exit") (aliases (-?))))
      |}];
    List.iter (None :: List.map Enumeration.all ~f:Option.return) ~f:(fun option ->
      print_args_and_parsed param ~t:option ~sexp_of_t:[%sexp_of: Enumeration.t];
      print_endline "");
    [%expect
      {|
      ()
      Everything

      (-flag everything)
      Everything

      (-flag just-one)
      Just_one

      (-flag nothing)
      Nothing
      |}]
  ;;

  let%expect_test "param from [create_optional_with_default_from_enum] with extra options"
    =
    let param =
      Roundtrippable_command_param.create_optional_with_default_from_enum
        ~case_sensitive:false
        ~represent_choice_with:"FLAG"
        ~list_values_in_help:false
        "flag"
        (module Enumeration)
        ~doc:"doc"
        ~default:Everything
    in
    print_param_flags param;
    [%expect
      {|
      (((name "[-flag FLAG]") (doc "doc (default: everything)") (aliases ()))
       ((name [-help]) (doc "print this help text and exit") (aliases (-?))))
      |}];
    List.iter (None :: List.map Enumeration.all ~f:Option.return) ~f:(fun option ->
      print_args_and_parsed param ~t:option ~sexp_of_t:[%sexp_of: Enumeration.t];
      print_endline "");
    [%expect
      {|
      ()
      Everything

      (-flag everything)
      Everything

      (-flag just-one)
      Just_one

      (-flag nothing)
      Nothing
      |}];
    (* Test case insensitivity *)
    List.iter Enumeration.all ~f:(fun t ->
      let args =
        Roundtrippable_command_param.command_args param (Some t)
        |> List.map ~f:String.capitalize
      in
      print_s [%message "" ~_:(args : string list)];
      print_parsed_args param ~args ~sexp_of_t:[%sexp_of: Enumeration.t];
      print_endline "");
    [%expect
      {|
      (-flag Everything)
      Everything

      (-flag Just-one)
      Just_one

      (-flag Nothing)
      Nothing
      |}]
  ;;

  let create_no_arg_some = Roundtrippable_command_param.create_no_arg_some

  let%expect_test "Roundtrip flag" =
    let example = create_no_arg_some "no-arg" () ~doc:"doc" in
    print_param_flags example;
    [%expect
      {|
      (((name [-no-arg]) (doc doc) (aliases ()))
       ((name [-help]) (doc "print this help text and exit") (aliases (-?))))
      |}];
    print_s [%sexp (command_args example (Some ()) : string list)];
    [%expect {| (-no-arg) |}];
    print_s [%sexp (command_args example None : string list)];
    [%expect {| () |}]
  ;;

  let create_no_arg_required = Roundtrippable_command_param.create_no_arg_required

  let%expect_test "Roundtrip flag" =
    let example = create_no_arg_required "no-arg-req" ~doc:"doc" in
    print_param_flags example;
    [%expect
      {|
      (((name -no-arg-req) (doc doc) (aliases ()))
       ((name [-help]) (doc "print this help text and exit") (aliases (-?))))
      |}];
    print_s [%sexp (command_args example () : string list)];
    [%expect {| (-no-arg-req) |}]
  ;;

  (* Tested with [Anons] module, below. *)

  let anon = Roundtrippable_command_param.anon

  (* Tested in [test_anons.ml] *)

  module Anons = Roundtrippable_command_param.Anons

  (* Tested in [test_variant_builder.ml] *)

  module Variant_builder = Roundtrippable_command_param.Variant_builder

  let build_variant = Roundtrippable_command_param.build_variant
  let build_set = Roundtrippable_command_param.build_set
  let variant = Roundtrippable_command_param.variant
  let variant0 = Roundtrippable_command_param.variant0
  let variant1 = Roundtrippable_command_param.variant1
  let variant2 = Roundtrippable_command_param.variant2
  let variant3 = Roundtrippable_command_param.variant3

  module Variant_builder_non_optional =
    Roundtrippable_command_param.Variant_builder_non_optional
end
