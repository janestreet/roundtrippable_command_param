open! Core
open! Test_helpers

module _ : module type of Roundtrippable_command_param.Anons = struct
  (* helpers *)

  open struct
    module Anons = Roundtrippable_command_param.Anons
  end

  (* types *)

  module T2 = struct
    type ('a, 'b) t = ('a, 'b) Anons.T2.t
  end

  type 'a t = 'a Anons.t

  (* individual tests follow *)

  let one = Anons.one

  let%expect_test "simple one" =
    let param =
      Anons.one "FOO" Command.Param.int ~to_string:Int.to_string
      |> Roundtrippable_command_param.anon
    in
    print_param param;
    [%expect
      {|
      (Basic
       ((summary "Example command") (anons (Grammar (One FOO)))
        (flags
         (((name [-help]) (doc "print this help text and exit") (aliases (-?)))))))
      |}];
    print_args_and_parsed param ~t:42 ~sexp_of_t:[%sexp_of: int];
    [%expect
      {|
      (42)
      42
      |}]
  ;;

  let maybe = Anons.maybe

  let%expect_test "simple maybe" =
    let param =
      Anons.maybe (Anons.one "FOO" Command.Param.int ~to_string:Int.to_string)
      |> Roundtrippable_command_param.anon
    in
    print_param param;
    [%expect
      {|
      (Basic
       ((summary "Example command") (anons (Grammar (Maybe (One FOO))))
        (flags
         (((name [-help]) (doc "print this help text and exit") (aliases (-?)))))))
      |}];
    let sexp_of_t = [%sexp_of: int option] in
    print_args_and_parsed param ~t:(Some 42) ~sexp_of_t;
    [%expect
      {|
      (42)
      (42)
      |}];
    print_args_and_parsed param ~t:None ~sexp_of_t;
    [%expect
      {|
      ()
      ()
      |}]
  ;;

  let maybe_with_default = Anons.maybe_with_default

  let%expect_test "simple maybe with default" =
    let param =
      Anons.maybe_with_default
        10
        (Anons.one "FOO" Command.Param.int ~to_string:Int.to_string)
      |> Roundtrippable_command_param.anon
    in
    print_param param;
    [%expect
      {|
      (Basic
       ((summary "Example command") (anons (Grammar (Maybe (One FOO))))
        (flags
         (((name [-help]) (doc "print this help text and exit") (aliases (-?)))))))
      |}];
    let sexp_of_t = [%sexp_of: int] in
    print_args_and_parsed param ~t:(Some 42) ~sexp_of_t;
    [%expect
      {|
      (42)
      42
      |}];
    print_args_and_parsed param ~t:None ~sexp_of_t;
    [%expect
      {|
      ()
      10
      |}]
  ;;

  let t2 = Anons.t2

  let%expect_test "simple t2" =
    let param =
      Anons.t2
        (Anons.one "FOO" Command.Param.string ~to_string:Fn.id)
        (Anons.one "BAR" Command.Param.int ~to_string:Int.to_string)
      |> Roundtrippable_command_param.anon
    in
    print_param param;
    [%expect
      {|
      (Basic
       ((summary "Example command")
        (anons (Grammar (Concat ((One FOO) (One BAR)))))
        (flags
         (((name [-help]) (doc "print this help text and exit") (aliases (-?)))))))
      |}];
    let sexp_of_t = [%sexp_of: string * int] in
    print_args_and_parsed param ~t:("hi", 42) ~sexp_of_t;
    [%expect
      {|
      (hi 42)
      (hi 42)
      |}]
  ;;

  let t3 = Anons.t3

  let%expect_test "simple t3" =
    let param =
      Anons.t3
        (Anons.one "FOO" Command.Param.string ~to_string:Fn.id)
        (Anons.one "BAR" Command.Param.int ~to_string:Int.to_string)
        (Anons.one "BAZ" Command.Param.bool ~to_string:Bool.to_string)
      |> Roundtrippable_command_param.anon
    in
    print_param param;
    [%expect
      {|
      (Basic
       ((summary "Example command")
        (anons (Grammar (Concat ((One FOO) (One BAR) (One BAZ)))))
        (flags
         (((name [-help]) (doc "print this help text and exit") (aliases (-?)))))))
      |}];
    let sexp_of_t = [%sexp_of: string * int * bool] in
    print_args_and_parsed param ~t:("hi", 42, true) ~sexp_of_t;
    [%expect
      {|
      (hi 42 true)
      (hi 42 true)
      |}]
  ;;

  let t4 = Anons.t4

  let%expect_test "simple t4" =
    let param =
      Anons.t4
        (Anons.one "FOO" Command.Param.string ~to_string:Fn.id)
        (Anons.one "BAR" Command.Param.int ~to_string:Int.to_string)
        (Anons.one "BAZ" Command.Param.bool ~to_string:Bool.to_string)
        (Anons.one "FIZ" Command.Param.date ~to_string:Date.to_string)
      |> Roundtrippable_command_param.anon
    in
    print_param param;
    [%expect
      {|
      (Basic
       ((summary "Example command")
        (anons (Grammar (Concat ((One FOO) (One BAR) (One BAZ) (One FIZ)))))
        (flags
         (((name [-help]) (doc "print this help text and exit") (aliases (-?)))))))
      |}];
    let sexp_of_t = [%sexp_of: string * int * bool * Date.t] in
    print_args_and_parsed
      param
      ~t:("hi", 42, true, Date.of_time Time_float.epoch ~zone:Time_float.Zone.utc)
      ~sexp_of_t;
    [%expect
      {|
      (hi 42 true 1970-01-01)
      (hi 42 true 1970-01-01)
      |}]
  ;;

  let%expect_test "test maybe t2" =
    let param =
      Anons.maybe
        (Anons.t2
           (Anons.one "FOO" Command.Param.string ~to_string:Fn.id)
           (Anons.one "BAR" Command.Param.int ~to_string:Int.to_string))
      |> Roundtrippable_command_param.anon
    in
    print_param param;
    [%expect
      {|
      (Basic
       ((summary "Example command")
        (anons (Grammar (Maybe (Concat ((One FOO) (One BAR))))))
        (flags
         (((name [-help]) (doc "print this help text and exit") (aliases (-?)))))))
      |}];
    let sexp_of_t = [%sexp_of: (string * int) option] in
    print_args_and_parsed param ~t:(Some ("hi", 42)) ~sexp_of_t;
    [%expect
      {|
      (hi 42)
      ((hi 42))
      |}];
    print_args_and_parsed param ~t:None ~sexp_of_t;
    [%expect
      {|
      ()
      ()
      |}]
  ;;

  let sequence = Anons.sequence

  let%expect_test "test t2 with sequence" =
    let param =
      Anons.t2
        (Anons.one "FOO" Command.Param.string ~to_string:Fn.id)
        (Anons.sequence (Anons.one "BAR" Command.Param.int ~to_string:Int.to_string))
      |> Roundtrippable_command_param.anon
    in
    print_param param;
    [%expect
      {|
      (Basic
       ((summary "Example command")
        (anons (Grammar (Concat ((One FOO) (Many (One BAR))))))
        (flags
         (((name [-help]) (doc "print this help text and exit") (aliases (-?)))))))
      |}];
    let sexp_of_t = [%sexp_of: string * int list] in
    print_args_and_parsed param ~t:("hi there", [ 42; 56 ]) ~sexp_of_t;
    [%expect
      {|
      ("hi there" 42 56)
      ("hi there" (42 56))
      |}];
    print_args_and_parsed param ~t:("bye", []) ~sexp_of_t;
    [%expect
      {|
      (bye)
      (bye ())
      |}]
  ;;

  let%expect_test "simple anon sequence" =
    let param =
      Anons.sequence (Anons.one "FOO" Command.Param.int ~to_string:Int.to_string)
      |> Roundtrippable_command_param.anon
    in
    print_param param;
    [%expect
      {|
      (Basic
       ((summary "Example command") (anons (Grammar (Many (One FOO))))
        (flags
         (((name [-help]) (doc "print this help text and exit") (aliases (-?)))))))
      |}];
    let sexp_of_t = [%sexp_of: int list] in
    print_args_and_parsed param ~t:[ 42; 23; 37 ] ~sexp_of_t;
    [%expect
      {|
      (42 23 37)
      (42 23 37)
      |}]
  ;;

  let non_empty_sequence_as_list = Anons.non_empty_sequence_as_list

  let%expect_test "simple non-empty list sequence" =
    let param =
      Anons.non_empty_sequence_as_list
        (Anons.one "FOO" Command.Param.int ~to_string:Int.to_string)
      |> Roundtrippable_command_param.anon
    in
    print_param param;
    [%expect
      {|
      (Basic
       ((summary "Example command")
        (anons (Grammar (Concat ((One FOO) (Many (One FOO))))))
        (flags
         (((name [-help]) (doc "print this help text and exit") (aliases (-?)))))))
      |}];
    let sexp_of_t = [%sexp_of: int list] in
    print_args_and_parsed param ~t:[ 42; 23; 37 ] ~sexp_of_t;
    [%expect
      {|
      (42 23 37)
      (42 23 37)
      |}];
    Or_error.try_with (fun () -> print_args_and_parsed param ~t:[] ~sexp_of_t)
    |> printf !"%{sexp: unit Or_error.t}";
    [%expect {| (Error "Serialization error: List must not be empty") |}]
  ;;

  let non_empty_sequence_as_pair = Anons.non_empty_sequence_as_pair

  let%expect_test "simple non-empty pair sequence" =
    let param =
      Anons.non_empty_sequence_as_pair
        (Anons.one "FOO" Command.Param.int ~to_string:Int.to_string)
      |> Roundtrippable_command_param.anon
    in
    print_param param;
    [%expect
      {|
      (Basic
       ((summary "Example command")
        (anons (Grammar (Concat ((One FOO) (Many (One FOO))))))
        (flags
         (((name [-help]) (doc "print this help text and exit") (aliases (-?)))))))
      |}];
    let sexp_of_t = [%sexp_of: int * int list] in
    print_args_and_parsed param ~t:(41, [ 42; 23; 37 ]) ~sexp_of_t;
    [%expect
      {|
      (41 42 23 37)
      (41 (42 23 37))
      |}];
    print_args_and_parsed param ~t:(42, []) ~sexp_of_t;
    [%expect
      {|
      (42)
      (42 ())
      |}]
  ;;

  let%expect_test "t2 sequence test" =
    let param =
      Anons.sequence
        (Anons.t2
           (Anons.one "FOO" Command.Param.int ~to_string:Int.to_string)
           (Anons.one "BAR" Command.Param.int ~to_string:Int.to_string))
      |> Roundtrippable_command_param.anon
    in
    print_param param;
    [%expect
      {|
      (Basic
       ((summary "Example command")
        (anons (Grammar (Many (Concat ((One FOO) (One BAR))))))
        (flags
         (((name [-help]) (doc "print this help text and exit") (aliases (-?)))))))
      |}];
    let sexp_of_t = [%sexp_of: (int * int) list] in
    print_args_and_parsed param ~t:[ 42, 31; 23, 37 ] ~sexp_of_t;
    [%expect
      {|
      (42 31 23 37)
      ((42 31) (23 37))
      |}];
    print_args_and_parsed param ~t:[] ~sexp_of_t;
    [%expect
      {|
      ()
      ()
      |}]
  ;;

  let map = Anons.map

  let%expect_test "map test" =
    let param =
      Anons.one "FOO" Command.Param.int ~to_string:Int.to_string
      |> Anons.map ~f:(fun i -> Int.to_string i ^ "num")
      |> Roundtrippable_command_param.anon
    in
    print_param param;
    [%expect
      {|
      (Basic
       ((summary "Example command") (anons (Grammar (One FOO)))
        (flags
         (((name [-help]) (doc "print this help text and exit") (aliases (-?)))))))
      |}];
    let sexp_of_t = [%sexp_of: string] in
    print_args_and_parsed param ~t:4 ~sexp_of_t;
    [%expect
      {|
      (4)
      4num
      |}]
  ;;

  let contra_map = Anons.contra_map

  let%expect_test "contra_map test" =
    let param =
      Anons.one "FOO" Command.Param.int ~to_string:Int.to_string
      |> Anons.contra_map ~f:(fun i -> i + 10)
      |> Roundtrippable_command_param.anon
    in
    print_param param;
    [%expect
      {|
      (Basic
       ((summary "Example command") (anons (Grammar (One FOO)))
        (flags
         (((name [-help]) (doc "print this help text and exit") (aliases (-?)))))))
      |}];
    let sexp_of_t = [%sexp_of: int] in
    print_args_and_parsed param ~t:4 ~sexp_of_t;
    [%expect
      {|
      (14)
      14
      |}]
  ;;
end
