open! Core

let print_parsed_args param ~args ~sexp_of_t =
  let command =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"Example command"
      (let%map_open t = Roundtrippable_command_param.param param in
       fun () -> print_s (sexp_of_t t))
  in
  Command_unix.run ~argv:("exe_name_arg" :: args) command
;;

let print_args_and_parsed param ~t ~sexp_of_t =
  let args = Roundtrippable_command_param.command_args param t in
  print_s [%sexp (args : string list)];
  print_parsed_args param ~args ~sexp_of_t
;;

let print_param param =
  let command =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"Example command"
      (let%map_open _ = Roundtrippable_command_param.param param in
       fun () -> ())
  in
  print_s
    [%sexp
      (Command.(Shape.fully_forced (Command_unix.shape command))
       : Command.Shape.Fully_forced.t)]
;;

let print_param_flags t =
  print_param t;
  let output = Expect_test_helpers_base.expect_test_output () in
  Sexp_app.Utils.get_one_field (Sexp.of_string output) "flags" |> ok_exn |> print_s
;;
