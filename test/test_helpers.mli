open! Core

val print_parsed_args
  :  ('a, 'b) Roundtrippable_command_param.T2.t
  -> args:string list
  -> sexp_of_t:('a -> Ppx_sexp_conv_lib.Sexp.t)
  -> unit

val print_args_and_parsed
  :  ('a, 'b) Roundtrippable_command_param.T2.t
  -> t:'b
  -> sexp_of_t:('a -> Ppx_sexp_conv_lib.Sexp.t)
  -> unit

val print_param : ('a, 'b) Roundtrippable_command_param.T2.t -> unit
val print_param_flags : ('a, 'b) Roundtrippable_command_param.T2.t -> unit
