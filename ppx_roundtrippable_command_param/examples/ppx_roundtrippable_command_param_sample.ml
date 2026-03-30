open! Core

module Sample_module = struct
  module T = struct
    type t =
      | Foo
      | Bar
    [@@deriving sexp_of, enumerate]
  end

  include T

  let roundtrippable_arg_type =
    Roundtrippable_arg_type.create
      ~arg_type:(Command.Arg_type.enumerated_sexpable (module T))
      ~to_string:(fun x -> [%sexp_of: t] x |> Sexp.to_string)
      ~arg_placeholder:"FOO_BAR"
  ;;
end

module Record_with_custom_doc = struct
  type t =
    { foo : bool [@bool_no_arg] (** Just want to make sure it compiles *)
    ; bar : float
    (** This is a very very very long doc string that explains "a lot of stuff", including
        why here should be a float and what do we want it for blah blah blah blah.

        There should be a newline before this paragraph. *)
    ; bars : float list (** This one is repeated! *)
    ; baz : Sample_module.t
    ; bazes : Sample_module.t list (** List plus custom arg type *)
    ; qux : string option (** This is optional *)
    ; quxes : string list [@list_method comma_separated] (** This is a list *)
    }
  [@@deriving sexp_of, fields ~iterators:make_creator, roundtrippable_command_param]
end

let command =
  Command.basic
    ~summary:"test"
    (let%map_open.Command value = Record_with_custom_doc.param in
     fun () -> print_s [%message "Parsed value" (value : Record_with_custom_doc.t)])
;;

module Record_with_default_with_custom_doc = struct
  type t =
    { foo : bool [@default false]
    ; bar : float
    ; baz : Sample_module.t [@default Sample_module.Foo]
    ; qux : string option
         [@roundtrippable_command_param
           Roundtrippable_command_param.Let_syntax.return None]
    }
  [@@deriving
    sexp_of, fields ~iterators:make_creator, or_default, roundtrippable_command_param]
end

let command_with_default =
  Command.basic
    ~summary:"with_default"
    (let%map_open.Command value = Record_with_default_with_custom_doc.param in
     fun () ->
       print_s [%message "Parsed value" (value : Record_with_default_with_custom_doc.t)])
;;

let main_command =
  Command.group
    ~summary:"Main command that groups test and with_default commands"
    [ "without-default", command; "with-default", command_with_default ]
;;

let () = Command_unix.run main_command
