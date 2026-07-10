# `ppx_roundtrippable_command_param`

`ppx_roundtrippable_command_param` is a PPX rewriter that generates command-line parameters mainly for record
types. For a record of type `t`, it creates a
[`val roundtrippable_command_param: t Roundtrippable_command_param.t`](%{root}/lib/roundtrippable_command_param/doc/readme.mlt)
that combines the parameters of all fields in that record.

It relies on the
`Roundtrippable_arg_type` library,
which provides enough information to automatically create `Roundtrippable_command_params`s
for each field:

1. How to parse the type from command-line arguments
2. How to serialize the type to arguments
3. What abbreviation to use in help text

# Overview

Borrowing the example from `Roundtrippable_command_param`, consider this record type:

```ocaml
type t =
  { fs_poll_interval : Time_ns.Span.t (** poll interval *)
  ; file_to_watch : Filename.t        (** file to watch *)
  ; port : int                        (** port to serve on *)
  }
[@@deriving fields ~iterators:make_creator, roundtrippable_command_param]
```

Using the field name, type, and docstring, this creates a
`t Roundtrippable_command_param.t` (and corresponding `t Command.Param.t`):

```ocaml skip
val roundtrippable_command_param : t Roundtrippable_command_param.t
val param : t Command.Param.t
```

<details>
<summary>Contrast with the boilerplate of the manual implementation.</summary>

```ocaml
# let fs_poll_interval_param =
    Roundtrippable_command_param.create_required
      "poll-interval"
      Time_ns.Span.arg_type
      ~doc:"SPAN poll interval"
      ~to_string:Time_ns.Span.to_string
  and file_to_watch_param =
    Roundtrippable_command_param.create_required
      "file-to-watch"
      Filename_unix.arg_type
      ~doc:"FILE file to watch"
      ~to_string:Fn.id
  and port_param =
    Roundtrippable_command_param.create_required
      "port"
      Command.Param.int
      ~doc:"INT port to serve on"
      ~to_string:Int.to_string
  in
  Roundtrippable_command_param.Record_builder.(
    Fields.make_creator
      ~fs_poll_interval:(field fs_poll_interval_param)
      ~file_to_watch:(field file_to_watch_param)
      ~port:(field port_param)
    |> build_for_record)
- : t Roundtrippable_command_param.Record_builder.profunctor_term = <abstr>
```

</details>

The generated roundtrippable command param can be used in two ways:

1. To convert a value into command-line arguments:

```ocaml
# let args =
    let t =
      { fs_poll_interval = Time_ns.Span.of_int_sec 1
      ; file_to_watch    = "important_data.txt"
      ; port             = 1337
      }
    in
    Roundtrippable_command_param.command_args roundtrippable_command_param t
val args : string list =
  ["-fs-poll-interval"; "1s"; "-file-to-watch"; "important_data.txt";
   "-port"; "1337"]
```

This provides type safety when invoking binaries (or specifying how to invoke binaries) from OCaml code (e.g., in Appd configs).

2. To parse command-line arguments back into a value:

```ocaml
# Command.Param.parse param args |> ok_exn
- : t =
{fs_poll_interval = 1s; file_to_watch = "important_data.txt"; port = 1337}
```

# Advanced features

Each field can be customized using the following ways:

```ocaml
(** Define a helper function to make examples a bit shorter *)
let parse_string_exn param args =
  let args =
    String.split ~on:' ' args |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  Command.Param.parse param args |> ok_exn
;;
```

## Optional flags

Fields with type `'a option` will be automatically detected and will create a optional
flag instead of a required flag:

```ocaml
type t =
  { just_int : int option (** Can work with any ['a option] *)
  ; interval : Time_ns.Span.t option (** where ['a] is not a parameterized type *)
  }
[@@deriving fields ~iterators:make_creator, roundtrippable_command_param]
```

```ocaml
# parse_string_exn param "-interval 1s"
- : t = {just_int = None; interval = Some 1s}
```

## List flags

Fields with type `'a list` will allow passing the flag repeatedly by default:

```ocaml
type t =
  { foos : int list [@list_method listed] (** [listed] is the default if not given *)
  ; intervals : Time_ns.Span.t list [@list_method comma_separated]
      (** make sure the arg_type doesn't have a comma in it! *)
  }
[@@deriving fields ~iterators:make_creator, roundtrippable_command_param]
```

```ocaml
# parse_string_exn param "-foos 1 -foos 2 -interval 1s,2s"
- : t = {foos = [1; 2]; intervals = [1s; 2s]}
```

## Custom flag names

By default, the flag name is derived from the record field name. This may be awkward for
fields of type `'a list`. Use `[@name "flag-name"]` to override this:

```ocaml
type t =
  { users : string list [@name "user"]
  }
[@@deriving fields ~iterators:make_creator, roundtrippable_command_param]
```

```ocaml
# parse_string_exn param "-user alice -user bob"
- : t = {users = ["alice"; "bob"]}
```

## No-arg bool flags

By default, flags for `bool` fields take an explicit `true` or `false` value; with
`[@bool_no_arg]`, simply providing the flag makes the argument true:

```ocaml
type t = { no_arg : bool [@bool_no_arg] (** Should take no argument *) }
[@@deriving fields ~iterators:make_creator, roundtrippable_command_param]
```

```ocaml
# parse_string_exn param ""
- : t = {no_arg = false}
# parse_string_exn param "-no-arg"
- : t = {no_arg = true}
```

## Flags with default values

The PPX integrates with `ppx_or_default` to handle default values (you must derive
`or_default` *first*, and make sure to add `ppx_or_default` to the `jbuild`). When
default fields are present, the PPX emits two bindings:

- `roundtrippable_command_param_with_defaults` (or
  `roundtrippable_command_param_<name>_with_defaults` for non-`t` types) of type
  `(t, With_defaults.t) Roundtrippable_command_param.T2.t`. Pass a `With_defaults.t`
  through `command_args` and unspecified fields will be omitted from the generated args.
- `roundtrippable_command_param` of type `t Roundtrippable_command_param.t`, obtained by
  [`contra_map`]ing the with-defaults binding through the `With_defaults.create` function
  emitted by `ppx_or_default`. Passing a fully-resolved `t` through `command_args`
  explicitly sets all switches in the resulting command-line unless those fields match the
  default *and* `@drop_default` is used.

```ocaml
type t =
  { num1 : int [@default 42] [@default.drop_default.equal] (** If num1 is not provided on the command-line, the value will default to 42. If [num1 = 42] when calling [command_args], then flags are not generated for this field. *)
  ; num2 : float option (** Default should not be used together with option *)
  ; use1 : bool [@default false] (** Provided flag will override default *)
  }
  [@@deriving fields ~iterators:make_creator, or_default, roundtrippable_command_param]
```

If you plan to expose the roundtrippable command param in your MLI, you will need to
derive `or_default` and also annotate fields with `[@default]` in the MLI as well.

```ocaml
# parse_string_exn param "-use1 true"
- : t = {num1 = 42; num2 = None; use1 = true}
# let with_defaults = { With_defaults.num1 = Custom 10; num2 = None; use1 = Default} in
  Roundtrippable_command_param.command_args
    roundtrippable_command_param_with_defaults
    with_defaults
- : string list = ["-num1"; "10"]
# Roundtrippable_command_param.command_args
    roundtrippable_command_param
    { num1 = 10; num2 = None; use1 = false }
- : string list = ["-num1"; "10"; "-use1"; "false"]
```


## Custom arg type

By default, the PPX will assume that a `Roundtrippable_arg_type.t` for a type `Foo.t` is
available at `Foo.roundtrippable_arg_type` (for `Foo.custom_type`, it looks at
`Foo.roundtrippable_arg_type_custom_type`). If that is not the case, or if you'd like to
specify a different arg type for a field, pass a `Roundtrippable_arg_type.t` via
`[@roundtrippable_arg_type <expression>]`.

```ocaml
module Foo = struct
  type t = string

  let roundtrippable_arg_type =
    Roundtrippable_arg_type.create
      ~arg_type:Command.Param.string
      ~to_string:Fn.id
      ~arg_placeholder:"FOO"

  let reversed_roundtrippable_arg_type =
    Roundtrippable_arg_type.create
      ~arg_type:(Command.Param.string |> Command.Arg_type.map ~f:String.rev)
      ~to_string:String.rev
      ~arg_placeholder:"OOF"
end

type t =
  { field1 : Foo.t (** defaults to [Foo.roundtrippable_arg_type] *)
  ; field2 : Foo.t [@roundtrippable_arg_type] (** also [Foo.roundtrippable_arg_type] *)
  ; field3 : Foo.t [@roundtrippable_arg_type Foo.reversed_roundtrippable_arg_type]
  }
[@@deriving fields ~iterators:make_creator, roundtrippable_command_param]
```

```ocaml
# let parse args = parse_string_exn param args in
  parse "-field1 foo -field2 foo -field3 sdrawkcab"
- : t = {field1 = "foo"; field2 = "foo"; field3 = "backwards"}
```

## Nested records

If your field can't be parsed via the default roundtrippable command param derived from
the arg type, you can use `[@roundtrippable_command_param]` to provide one.

If `ppx_or_default` isn't used, the PPX will assume that a
`t Roundtrippable_command_param.t` for a type `Foo.t` is available at
`Foo.roundtrippable_command_param` (for `Foo.custom_type`, it looks at
`Foo.roundtrippable_command_param_custom_type`). This makes nesting easy since we only
have to tag the field with `[@roundtrippable_command_param]` without arguments.

```ocaml
module Inner = struct
  type t = { foo : int (** This is the real doc for the help text *); bar : string }
  [@@deriving fields ~iterators:make_creator, roundtrippable_command_param]
end

type t = { inner : Inner.t [@roundtrippable_command_param] (** This doc is ignored *) }
[@@deriving fields ~iterators:make_creator, roundtrippable_command_param]
```

```ocaml
# parse_string_exn param "-foo 32 -bar baz"
- : t = {inner = {Inner.foo = 32; bar = "baz"}}
```

If you want `inner` to be optional, you must provide an `Inner.t option Roundtrippable_command_param.t`:

```ocaml
type t = { inner : Inner.t option [@roundtrippable_command_param Roundtrippable_command_param.optional_params Inner.roundtrippable_command_param] }
[@@deriving fields ~iterators:make_creator, roundtrippable_command_param]
```

```ocaml
# parse_string_exn param ""
- : t = {inner = None}
# parse_string_exn param "-foo 20 -bar baz"
- : t = {inner = Some {Inner.foo = 20; bar = "baz"}}
# parse_string_exn param "-foo 20" (* if you provide [-foo], you must provide [-bar]! *)
Exception:
"Not all flags in group \"-bar,-foo\" are given: missing required flag: -bar"
```

If `ppx_or_default` is in use, the PPX will look for a
`('a, 'b) Roundtrippable_command_param.T2.t` for a field which is of type `'a` in the
normal type and of type `'b` in the `With_defaults.t` type. Note that
`'a Roundtrippable_command_param.t` is type equal to
`('a, 'a) Roundtrippable_command_param.T2.t`, so non-defaulted fields work as normal.
Also, if the `[@with_defaults]` attribute is given, `[@roundtrippable_command_param]` is
automatically inferred.

```ocaml
module Inner = struct
  type t = { foo : int [@default 10]; bar : string  }
  [@@deriving fields ~iterators:make_creator, or_default, roundtrippable_command_param]
end

type t = { inner : Inner.t [@with_defaults]  }
[@@deriving fields ~iterators:make_creator, or_default, roundtrippable_command_param]
```

```ocaml
# parse_string_exn param "-bar baz"
- : t = {inner = {Inner.foo = 10; bar = "baz"}}
```

Currently, the PPX does not support optional nested `[@with_defaults]` records.

## Custom roundtrippable command param

You can also use `[@roundtrippable_command_param <expression>]` to provide a custom
parameter. This is a general-purpose escape hatch for a particular field.

```ocaml
let custom_roundtrippable_command_param =
    Roundtrippable_command_param.create_required
      "custom-flag"
      Command.Param.string
      ~doc:"STRING some string"
      ~to_string:Fn.id

type t =
  { value : string [@roundtrippable_command_param custom_roundtrippable_command_param] }
  [@@deriving fields ~iterators:make_creator, roundtrippable_command_param]
```

```ocaml
# parse_string_exn param "-custom-flag foo"
- : t = {value = "foo"}
```

# Note

1. Since ppx is deriving in the order they are written, it is important to derive `fields ~iterators:make_creator` and `or_default` (if used) before `roundtrippable_command_param`.
2. We provide a set of common Roundtrippable_arg_type.t implementations through our runtime library `Ppx_roundtrippable_command_param_runtime`. Note that this includes dependencies for all supported common types, regardless of which specific types you actually use.
