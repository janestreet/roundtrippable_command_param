open! Core
open! Test_helpers
open Roundtrippable_command_param

module Example = struct
  type t =
    | No_arg
    | Basic_arg of string
    | Two_arg of string * int
    | Inline_record of { foo : int }
  [@@deriving sexp_of, variants]

  let no_arg_param = create_no_arg "no-arg" ~doc:"no arg"
  let basic_arg_param = string_option "basic-arg" ~doc:"basic arg"
  let two_arg_param_a = string_option "two-arg-a" ~doc:"two arg string"
  let two_arg_param_b = int_option "two-arg-b" ~doc:"two arg int"
  let inline_record_param = int_option "inline-record" ~doc:"inline record"

  let roundtrippable_param ~if_nothing_chosen =
    Variants.make_matcher
      ~no_arg:(variant0 no_arg_param)
      ~basic_arg:(variant1 basic_arg_param)
      ~two_arg:(variant2 two_arg_param_a two_arg_param_b)
      ~inline_record:
        (variant
           inline_record_param
           ~to_variant:(fun f foo -> f ~foo)
           ~to_matcher:(fun f ~foo -> f foo))
    |> build_variant ~if_nothing_chosen
  ;;
end

let%expect_test "show help" =
  print_param (Example.roundtrippable_param ~if_nothing_chosen:Return_none);
  [%expect
    {|
    (Basic
     ((summary "Example command") (anons (Grammar Zero))
      (flags
       (((name "[-basic-arg basic]") (doc arg) (aliases ()))
        ((name "[-inline-record inline]") (doc record) (aliases ()))
        ((name [-no-arg]) (doc "no arg") (aliases ()))
        ((name "[-two-arg-a two]") (doc "arg string") (aliases ()))
        ((name "[-two-arg-b two]") (doc "arg int") (aliases ()))
        ((name [-help]) (doc "print this help text and exit") (aliases (-?)))))))
    |}]
;;

let%expect_test "test different args" =
  let param = Example.roundtrippable_param ~if_nothing_chosen:Return_none in
  let test t = print_args_and_parsed param ~t ~sexp_of_t:[%sexp_of: Example.t option] in
  test None;
  [%expect
    {|
    ()
    ()
    |}];
  test (Some No_arg);
  [%expect
    {|
    (-no-arg)
    (No_arg)
    |}];
  test (Some (Basic_arg "arg"));
  [%expect
    {|
    (-basic-arg arg)
    ((Basic_arg arg))
    |}];
  test (Some (Two_arg ("one", 1)));
  [%expect
    {|
    (-two-arg-a one -two-arg-b 1)
    ((Two_arg one 1))
    |}];
  test (Some (Inline_record { foo = 5 }));
  [%expect
    {|
    (-inline-record 5)
    ((Inline_record (foo 5)))
    |}]
;;

let%expect_test "test if_nothing_chosen" =
  let param = Example.roundtrippable_param ~if_nothing_chosen:Return_none in
  print_args_and_parsed param ~t:(Some No_arg) ~sexp_of_t:[%sexp_of: Example.t option];
  [%expect
    {|
    (-no-arg)
    (No_arg)
    |}];
  print_args_and_parsed param ~t:None ~sexp_of_t:[%sexp_of: Example.t option];
  [%expect
    {|
    ()
    ()
    |}];
  let param =
    Example.roundtrippable_param ~if_nothing_chosen:(Default_to (Basic_arg "arg"))
  in
  print_args_and_parsed param ~t:(Some No_arg) ~sexp_of_t:[%sexp_of: Example.t];
  [%expect
    {|
    (-no-arg)
    No_arg
    |}];
  print_args_and_parsed param ~t:None ~sexp_of_t:[%sexp_of: Example.t];
  [%expect
    {|
    ()
    (Basic_arg arg)
    |}];
  let param = Example.roundtrippable_param ~if_nothing_chosen:Raise in
  print_args_and_parsed param ~t:No_arg ~sexp_of_t:[%sexp_of: Example.t];
  [%expect
    {|
    (-no-arg)
    No_arg
    |}];
  Expect_test_helpers_base.show_raise (fun () ->
    print_parsed_args param ~args:[] ~sexp_of_t:[%sexp_of: Example.t]);
  [%expect
    {|
    Error parsing command line:

      Must pass one of these:
        -basic-arg
        -inline-record
        -no-arg
        -two-arg-a,-two-arg-b

    For usage information, run

      exe_name_arg -help

    (raised (command.ml.Exit_called (status 1)))
    |}]
;;

module Two_arg_test = struct
  type t = Two_arg of string * int [@@deriving sexp_of, variants]

  let two_arg_param_a = string_option "two-arg-a" ~doc:"two arg string"

  let two_arg_param_b =
    let primary = int_option "two-arg-b" ~doc:"two arg int" in
    let alias = param (int_option "two-arg-b-alternate" ~doc:"two arg int alternate") in
    alias_params primary [ alias ] ~if_nothing_chosen:Return_none
  ;;

  let roundtrippable_param =
    Variants.make_matcher ~two_arg:(variant2 two_arg_param_a two_arg_param_b)
    |> build_variant ~if_nothing_chosen:Return_none
  ;;
end

let%expect_test "test tuple parsing" =
  let param = Two_arg_test.roundtrippable_param in
  let run_test args =
    print_parsed_args param ~args ~sexp_of_t:[%sexp_of: Two_arg_test.t option]
  in
  run_test [ "-two-arg-a"; "foo"; "-two-arg-b"; "3" ];
  [%expect {| ((Two_arg foo 3)) |}];
  run_test [ "-two-arg-a"; "foo"; "-two-arg-b-alternate"; "3" ];
  [%expect {| ((Two_arg foo 3)) |}];
  run_test [];
  [%expect {| () |}];
  Expect_test_helpers_base.require_does_raise (fun () ->
    run_test [ "-two-arg-b"; "3"; "-two-arg-b-alternate"; "3" ]);
  [%expect
    {|
    Error parsing command line:

      Cannot pass more than one of these:
        -two-arg-b-alternate
        -two-arg-b

    For usage information, run

      exe_name_arg -help

    (command.ml.Exit_called (status 1))
    |}];
  Expect_test_helpers_base.require_does_raise (fun () -> run_test [ "-two-arg-b"; "3" ]);
  (* This is a confusing error message. It looks like we're saying the user passed both
     [-two-arg-b] and [-two-arg-b-alternate] when we mean the user passed one of them. In
     general, we lose the structure of the nested params. But I don't see any way to do
     better. *)
  [%expect
    {|
    Error parsing command line:

      ("Must pass all or none of these arguments, but got a mix."
       (absent ((-two-arg-a))) (present ((-two-arg-b -two-arg-b-alternate))))

    For usage information, run

      exe_name_arg -help

    (command.ml.Exit_called (status 1))
    |}]
;;

module None_requires_arg = struct
  module Option = struct
    type 'a t = 'a option =
      | None
      | Some of 'a
    [@@deriving variants]
  end

  type t =
    | A of int
    | B
  [@@deriving variants, sexp]

  let a_param =
    let open Roundtrippable_command_param in
    Option.Variants.make_matcher
      ~none:(variant0 (create_no_arg "none" ~doc:"none"))
      ~some:(variant1 (int_option "some-int" ~doc:"some int"))
    |> build_variant ~if_nothing_chosen:Raise
  ;;

  let roundtrippable_param () =
    let open Roundtrippable_command_param in
    Variants.make_matcher ~a:(variant1 a_param) ~b:(variant0 (create_no_arg "b" ~doc:"b"))
    |> build_variant ~if_nothing_chosen:Raise
  ;;
end

let%expect_test "test none requiring argument raises" =
  Expect_test_helpers_base.show_raise (fun () ->
    let (_ : None_requires_arg.t Roundtrippable_command_param.t) =
      None_requires_arg.roundtrippable_param ()
    in
    ());
  [%expect
    {|
    (raised (
      "[Variant_builder] doesn't support passing arguments to represent [None]"
      (args_for_none (-none))))
    |}]
;;

module Multiple_arg_test = struct
  module T = struct
    type t =
      | Foo
      | Bar
      | Baz
    [@@deriving compare, variants, sexp_of]

    include Comparable.Make_plain (struct
        type nonrec t = t [@@deriving sexp_of, compare]
      end)
  end

  include T

  let matcher () =
    let open Roundtrippable_command_param in
    let no_arg (variant : _ Variant.t) =
      let name = variant.name |> String.lowercase in
      variant0 (create_no_arg name ~doc:name) variant
    in
    Variants.make_matcher ~foo:no_arg ~bar:no_arg ~baz:no_arg
  ;;

  let single_roundtrippable_param =
    let open Roundtrippable_command_param in
    matcher () |> build_variant ~if_nothing_chosen:Raise
  ;;

  let set_roundtrippable_param =
    let open Roundtrippable_command_param in
    matcher () |> build_set ~comparable:(module T)
  ;;
end

let%expect_test "test multiple args" =
  let param = Multiple_arg_test.single_roundtrippable_param in
  let run_test args =
    print_parsed_args param ~args ~sexp_of_t:[%sexp_of: Multiple_arg_test.t]
  in
  run_test [ "-bar" ];
  [%expect {| Bar |}];
  Expect_test_helpers_base.show_raise (fun () -> run_test [ "-bar"; "-baz" ]);
  [%expect
    {|
    Error parsing command line:

      Cannot pass more than one of these:
        -baz
        -bar

    For usage information, run

      exe_name_arg -help

    (raised (command.ml.Exit_called (status 1)))
    |}]
;;

let%expect_test "test set args" =
  let param = Multiple_arg_test.set_roundtrippable_param in
  let run_test args =
    print_parsed_args param ~args ~sexp_of_t:[%sexp_of: Multiple_arg_test.Set.t]
  in
  run_test [];
  [%expect {| () |}];
  run_test [ "-foo" ];
  [%expect {| (Foo) |}];
  run_test [ "-bar"; "-foo" ];
  [%expect {| (Foo Bar) |}]
;;

module%test [@name "non_optional"] _ = struct
  open Variant_builder_non_optional

  let string_required name ~doc =
    create_required name Command.Param.string ~doc ~to_string:String.to_string
  ;;

  let int_required name ~doc =
    create_required name Command.Param.int ~doc ~to_string:Int.to_string
  ;;

  module Example = struct
    type t =
      | No_arg
      | Basic_arg of string
      | Two_arg of string * int
      | Inline_record of { foo : int }
    [@@deriving sexp_of, variants]

    let no_arg_param = create_no_arg_required "no-arg" ~doc:"no arg"
    let basic_arg_param = string_required "basic-arg" ~doc:"basic arg"
    let two_arg_param_a = string_required "two-arg-a" ~doc:"two arg string"
    let two_arg_param_b = int_required "two-arg-b" ~doc:"two arg int"
    let inline_record_param = int_required "inline-record" ~doc:"inline record"

    let roundtrippable_param ~if_nothing_chosen =
      Variants.make_matcher
        ~no_arg:(variant0 no_arg_param)
        ~basic_arg:(variant1 basic_arg_param)
        ~two_arg:(variant2 two_arg_param_a two_arg_param_b)
        ~inline_record:
          (variant
             inline_record_param
             ~to_variant:(fun f foo -> f ~foo)
             ~to_matcher:(fun f ~foo -> f foo))
      |> build_variant ~if_nothing_chosen
    ;;
  end

  let%expect_test "show help" =
    print_param (Example.roundtrippable_param ~if_nothing_chosen:Return_none);
    [%expect
      {|
      (Basic
       ((summary "Example command") (anons (Grammar Zero))
        (flags
         (((name "[-basic-arg basic]") (doc arg) (aliases ()))
          ((name "[-inline-record inline]") (doc record) (aliases ()))
          ((name [-no-arg]) (doc "no arg") (aliases ()))
          ((name "[-two-arg-a two]") (doc "arg string [requires: \"-two-arg-b\"]")
           (aliases ()))
          ((name "[-two-arg-b two]") (doc "arg int [requires: \"-two-arg-a\"]")
           (aliases ()))
          ((name [-help]) (doc "print this help text and exit") (aliases (-?)))))))
      |}]
  ;;

  let%expect_test "test different args" =
    let param = Example.roundtrippable_param ~if_nothing_chosen:Return_none in
    let test t = print_args_and_parsed param ~t ~sexp_of_t:[%sexp_of: Example.t option] in
    test None;
    [%expect
      {|
      ()
      ()
      |}];
    test (Some No_arg);
    [%expect
      {|
      (-no-arg)
      (No_arg)
      |}];
    test (Some (Basic_arg "arg"));
    [%expect
      {|
      (-basic-arg arg)
      ((Basic_arg arg))
      |}];
    test (Some (Two_arg ("one", 1)));
    [%expect
      {|
      (-two-arg-a one -two-arg-b 1)
      ((Two_arg one 1))
      |}];
    test (Some (Inline_record { foo = 5 }));
    [%expect
      {|
      (-inline-record 5)
      ((Inline_record (foo 5)))
      |}]
  ;;

  let%expect_test "test if_nothing_chosen" =
    let param = Example.roundtrippable_param ~if_nothing_chosen:Return_none in
    print_args_and_parsed param ~t:(Some No_arg) ~sexp_of_t:[%sexp_of: Example.t option];
    [%expect
      {|
      (-no-arg)
      (No_arg)
      |}];
    print_args_and_parsed param ~t:None ~sexp_of_t:[%sexp_of: Example.t option];
    [%expect
      {|
      ()
      ()
      |}];
    let param =
      Example.roundtrippable_param ~if_nothing_chosen:(Default_to (Basic_arg "arg"))
    in
    print_args_and_parsed param ~t:(Some No_arg) ~sexp_of_t:[%sexp_of: Example.t];
    [%expect
      {|
      (-no-arg)
      No_arg
      |}];
    print_args_and_parsed param ~t:None ~sexp_of_t:[%sexp_of: Example.t];
    [%expect
      {|
      ()
      (Basic_arg arg)
      |}];
    let param = Example.roundtrippable_param ~if_nothing_chosen:Raise in
    print_args_and_parsed param ~t:No_arg ~sexp_of_t:[%sexp_of: Example.t];
    [%expect
      {|
      (-no-arg)
      No_arg
      |}];
    Expect_test_helpers_base.show_raise (fun () ->
      print_parsed_args param ~args:[] ~sexp_of_t:[%sexp_of: Example.t]);
    [%expect
      {|
      Error parsing command line:

        Must pass one of these:
          -basic-arg
          -inline-record
          -no-arg
          -two-arg-a,-two-arg-b

      For usage information, run

        exe_name_arg -help

      (raised (command.ml.Exit_called (status 1)))
      |}]
  ;;

  module Two_arg_test = struct
    type t = Two_arg of string * int [@@deriving sexp_of, variants]

    let two_arg_param_a = string_required "two-arg-a" ~doc:"two arg string"

    let two_arg_param_b =
      let primary = int_required "two-arg-b" ~doc:"two arg int" in
      let alias =
        param (int_required "two-arg-b-alternate" ~doc:"two arg int alternate")
      in
      alias_params primary [ alias ] ~if_nothing_chosen:Raise
    ;;

    let roundtrippable_param =
      Variants.make_matcher ~two_arg:(variant2 two_arg_param_a two_arg_param_b)
      |> build_variant ~if_nothing_chosen:Return_none
    ;;
  end

  let%expect_test "test tuple parsing" =
    let param = Two_arg_test.roundtrippable_param in
    let run_test args =
      print_parsed_args param ~args ~sexp_of_t:[%sexp_of: Two_arg_test.t option]
    in
    run_test [ "-two-arg-a"; "foo"; "-two-arg-b"; "3" ];
    [%expect {| ((Two_arg foo 3)) |}];
    run_test [ "-two-arg-a"; "foo"; "-two-arg-b-alternate"; "3" ];
    [%expect {| ((Two_arg foo 3)) |}];
    run_test [];
    [%expect {| () |}];
    Expect_test_helpers_base.require_does_raise (fun () ->
      run_test [ "-two-arg-a"; "foo" ]);
    [%expect
      {|
      Error parsing command line:

        Not all flags in group "-two-arg-a" are given: Must pass one of these:
          -two-arg-b
          -two-arg-b-alternate

      For usage information, run

        exe_name_arg -help

      (command.ml.Exit_called (status 1))
      |}];
    Expect_test_helpers_base.require_does_raise (fun () ->
      run_test [ "-two-arg-b"; "3"; "-two-arg-b-alternate"; "3" ]);
    [%expect
      {|
      Error parsing command line:

        Cannot pass more than one of these:
          -two-arg-b-alternate
          -two-arg-b

      For usage information, run

        exe_name_arg -help

      (command.ml.Exit_called (status 1))
      |}];
    Expect_test_helpers_base.require_does_raise (fun () -> run_test [ "-two-arg-b"; "3" ]);
    [%expect
      {|
      Error parsing command line:

        Not all flags in group "-two-arg-a" are given: missing required flag: -two-arg-a

      For usage information, run

        exe_name_arg -help

      (command.ml.Exit_called (status 1))
      |}]
  ;;

  module Multiple_arg_test = struct
    module T = struct
      type t =
        | Foo
        | Bar
        | Baz
      [@@deriving compare, variants, sexp_of]

      include Comparable.Make_plain (struct
          type nonrec t = t [@@deriving sexp_of, compare]
        end)
    end

    include T

    let matcher () =
      let no_arg (variant : _ Variant.t) =
        let name = variant.name |> String.lowercase in
        variant0 (create_no_arg_required name ~doc:name) variant
      in
      Variants.make_matcher ~foo:no_arg ~bar:no_arg ~baz:no_arg
    ;;

    let single_roundtrippable_param = matcher () |> build_variant ~if_nothing_chosen:Raise
  end

  let%expect_test "test multiple args" =
    let param = Multiple_arg_test.single_roundtrippable_param in
    let run_test args =
      print_parsed_args param ~args ~sexp_of_t:[%sexp_of: Multiple_arg_test.t]
    in
    run_test [ "-bar" ];
    [%expect {| Bar |}];
    Expect_test_helpers_base.show_raise (fun () -> run_test [ "-bar"; "-baz" ]);
    [%expect
      {|
      Error parsing command line:

        Cannot pass more than one of these:
          -baz
          -bar

      For usage information, run

        exe_name_arg -help

      (raised (command.ml.Exit_called (status 1)))
      |}]
  ;;
end
