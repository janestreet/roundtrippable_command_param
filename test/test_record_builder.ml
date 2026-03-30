open! Core
open! Test_helpers
open Roundtrippable_command_param

module Example = struct
  type t =
    { a : int
    ; b : string
    }
  [@@deriving fields ~iterators:make_creator, sexp]

  let params =
    let a_param =
      create_required "a" Command.Param.int ~doc:"A int" ~to_string:Int.to_string
    in
    let b_param =
      create_required "b" Command.Param.string ~doc:"B string" ~to_string:Fn.id
    in
    Record_builder.(
      Fields.make_creator ~a:(field a_param) ~b:(field b_param) |> build_for_record)
  ;;
end

let%expect_test "basic record" =
  print_param_flags Example.params;
  [%expect
    {|
    (((name "-a A") (doc int) (aliases ()))
     ((name "-b B") (doc string) (aliases ()))
     ((name [-help]) (doc "print this help text and exit") (aliases (-?))))
    |}];
  print_args_and_parsed
    Example.params
    ~t:{ a = 1; b = "foo" }
    ~sexp_of_t:[%sexp_of: Example.t];
  [%expect
    {|
    (-a 1 -b foo)
    ((a 1) (b foo))
    |}]
;;

let%expect_test "optional record" =
  let params : Example.t option Roundtrippable_command_param.t =
    optional_params Example.params
  in
  print_param_flags params;
  [%expect
    {|
    (((name "[-a A]") (doc "int [requires: \"-b\"]") (aliases ()))
     ((name "[-b B]") (doc "string [requires: \"-a\"]") (aliases ()))
     ((name [-help]) (doc "print this help text and exit") (aliases (-?))))
    |}];
  print_args_and_parsed
    params
    ~t:(Some { a = 1; b = "foo" })
    ~sexp_of_t:[%sexp_of: Example.t option];
  [%expect
    {|
    (-a 1 -b foo)
    (((a 1) (b foo)))
    |}];
  print_args_and_parsed params ~t:None ~sexp_of_t:[%sexp_of: Example.t option];
  [%expect
    {|
    ()
    ()
    |}];
  Expect_test_helpers_base.require_does_raise (fun () ->
    print_parsed_args params ~args:[ "-a"; "1" ] ~sexp_of_t:[%sexp_of: Example.t option]);
  [%expect
    {|
    Error parsing command line:

      Not all flags in group "-a,-b" are given: missing required flag: -b

    For usage information, run

      exe_name_arg -help

    (command.ml.Exit_called (status 1))
    |}]
;;
