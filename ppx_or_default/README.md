# `ppx_or_default`

`ppx_or_default` is a ppx that copies the type definition of a record and converts each
field annotated with `[@default <expression>]` to have type `<type> Or_default.t` and each
field annotated with `[@with_defaults]` to have type `<module>.With_defaults.t`.

See the `Or_default` documentation for
more about the underlying library.

Overview
--------

`ppx_or_default` makes it easy to pass defaults through an OCaml API while allowing easy
serialization:
<!-- $MDX file=test/example_for_mdx_intf.ml,part=record-1 -->
```ocaml
  type t =
    { queue_length : int [@default 5]
    ; user_name : string
    }
```
generates a module with `Or_default.t` fields for each field which is `[@default]`ed.

The new module is generally called `With_defaults` and has the following
signature:

<!-- $MDX file=test/example_for_mdx_intf.ml,part=signature -->
```ocaml
  module With_defaults : sig
    type derived_on = t

    type t =
      { queue_length : int Or_default.t
      ; user_name : string
      }

    val create : derived_on -> t
    val resolve : t -> derived_on
  end
```

For the example above, the generated OCaml code is:
<!-- $MDX file=test/example_for_mdx_intf.ml,part=record-1-with-defaults -->
```ocaml
    module With_defaults = struct
      type nonrec derived_on = t

      type t =
        { queue_length : int Or_default.t
        ; user_name : string
        }

      let create =
        (fun { queue_length; user_name } ->
           { queue_length = Or_default.Custom queue_length; user_name }
         : derived_on -> t)
      ;;

      let _ = create

      let resolve =
        (fun { queue_length; user_name } ->
           { queue_length = Or_default.resolve queue_length ~default:5; user_name }
         : t -> derived_on)
      ;;
```
<!-- $MDX file=test/example_for_mdx_intf.ml,part=record-1-with-defaults-end -->
```ocaml
    end
```

Users can pass `Default` to fields that they don't wish to override, and your code can
use the `resolve` function to convert back to the original type `t` with all defaults
substituted.

The `create` function goes in the other direction: it takes a value of the original type
`t` and lifts it into `With_defaults.t` by wrapping each defaulted field as
`Or_default.Custom`. By default, `create` doesn't try to detect that a field already
holds the default value — for that, see `[@default.drop_default*]` below.

Attributes
----------

The `[@default <expression>]` attribute is used to specify a default value for that field.
For instance:

<!-- $MDX file=test/example_for_mdx_intf.ml,part=attribute-demo-type -->
```ocaml
  type t =
    { a : int [@default 10]
    ; b : string [@default "hi"]
    ; c : float [@default 12.5]
    ; d : bool [@default false]
    ; e : char [@default 'a'] (* etc *)
    ; no_default : int
    }
  [@@deriving_inline or_default]
```
generates the following module:
<!-- $MDX file=test/example_for_mdx_intf.ml,part=attribute-demo-type-with-defaults -->
```ocaml
    module With_defaults = struct
      type nonrec derived_on = t

      type t =
        { a : int Or_default.t
        ; b : string Or_default.t
        ; c : float Or_default.t
        ; d : bool Or_default.t
        ; e : char Or_default.t
        ; no_default : int
        }

      let create =
        (fun { a; b; c; d; e; no_default } ->
           { a = Or_default.Custom a
           ; b = Or_default.Custom b
           ; c = Or_default.Custom c
           ; d = Or_default.Custom d
           ; e = Or_default.Custom e
           ; no_default
           }
         : derived_on -> t)
      ;;

      let _ = create

      let resolve =
        (fun { a; b; c; d; e; no_default } ->
           { a = Or_default.resolve a ~default:10
           ; b = Or_default.resolve b ~default:"hi"
           ; c = Or_default.resolve c ~default:12.5
           ; d = Or_default.resolve d ~default:false
           ; e = Or_default.resolve e ~default:'a'
           ; no_default
           }
         : t -> derived_on)
      ;;
```
<!-- $MDX file=test/example_for_mdx_intf.ml,part=attribute-demo-type-with-defaults-end -->
```ocaml
    end
```

Note that this attribute is shared with the `sexp` ppx so you may already have these in
your types.

In signatures, the `[@default]` attribute cannot take a argument. Defaultable fields are
instead annotated with `[@default]`.

To put a record deriving `or_default` inside another record, you can use the
`[@with_defaults]` attribute instead of `[@default]`. For instance:

<!-- $MDX file=test/example_for_mdx_intf.ml,part=attribute-demo-nesting -->
```ocaml
  module Inner = struct
    type t =
      { a : int [@default 10]
      ; b : string [@default "hi"]
      ; no_default : int
      }
    [@@deriving or_default]
  end

  type t =
    { inner : Inner.t [@with_defaults]
    ; c : float [@default 12.5]
    ; d : bool [@default false]
    ; e : char [@default 'a'] (* etc *)
    }
  [@@deriving_inline or_default]
```
generates the following module:
<!-- $MDX file=test/example_for_mdx_intf.ml,part=attribute-demo-nesting-with-defaults -->
```ocaml
    module With_defaults = struct
      type nonrec derived_on = t

      type t =
        { inner : Inner.With_defaults.t
        ; c : float Or_default.t
        ; d : bool Or_default.t
        ; e : char Or_default.t
        }

      let create =
        (fun { inner; c; d; e } ->
           { inner = Inner.With_defaults.create inner
           ; c = Or_default.Custom c
           ; d = Or_default.Custom d
           ; e = Or_default.Custom e
           }
         : derived_on -> t)
      ;;

      let _ = create

      let resolve =
        (fun { inner; c; d; e } ->
           { inner = Inner.With_defaults.resolve inner
           ; c = Or_default.resolve c ~default:12.5
           ; d = Or_default.resolve d ~default:false
           ; e = Or_default.resolve e ~default:'a'
           }
         : t -> derived_on)
      ;;
```
<!-- $MDX file=test/example_for_mdx_intf.ml,part=attribute-demo-nesting-with-defaults-end -->
```ocaml
    end
```

Then, you can independently set defaults or custom values for the fields of the inner
record in the outer `With_defaults` type.

The inner type with defaults is inferred from the type of the field in the outer record.
If you want to use a custom type for the field in the outer record with defaults, you can
optionally give an argument to the `[@with_defaults]` attribute, like this:
`[@with_defaults: Custom_type.t]`. In this case, `Custom_type` needs a `resolve` function.

### Detecting default values in `create`

By default, `create` always emits `Or_default.Custom field` for `[@default]`-annotated
fields, even if the value happens to equal the default. You can opt into "drop default"
behavior — emitting `Or_default.Default` when the field equals the default — by adding
one of three attributes (modeled after the corresponding attributes in `ppx_sexp_conv`):

- `[@default.drop_default <equal_fn>]` — uses the user-supplied equality function.
- `[@default.drop_default.compare]` — uses `[%compare.equal: <field_type>]`.
- `[@default.drop_default.equal]` — uses `[%equal: <field_type>]`.

For example:

<!-- $MDX file=test/example_for_mdx_intf.ml,part=drop-default-demo -->
```ocaml
  type t =
    { num : int [@default 5] [@default.drop_default.compare]
    ; word : string [@default "hi"]
    }
  [@@deriving or_default]
```
With this annotation, `create { num = 5; word = "hi" }` produces
`{ num = Default; word = Custom "hi" }`: `num` is dropped because it equals its default,
but `word` has no `drop_default*` attribute and is always wrapped as `Custom`. The
attribute requires `[@default <value>]` on the same field, and at most one of the three
forms may be used per field.

Naming
------

The generated sub-module will be named `With_defaults` if the deriver is applied to a type
named `t`. Types with different name have different module names: capitalize the first
letter of the type and append `_with_defaults`. The `create` and `resolve` functions will
also have the type name appended.

For instance:

<!-- $MDX file=test/example_for_mdx_intf.ml,part=naming-type -->
```ocaml
  type s =
    { a : int
    ; b : string
    }
  [@@deriving_inline or_default]
```
<!-- $MDX file=test/example_for_mdx_intf.ml,part=naming-type-with-defaults -->
```ocaml
    module S_with_defaults = struct
      type nonrec derived_on = s

      type s =
        { a : int
        ; b : string
        }

      let create_s = (fun { a; b } -> { a; b } : derived_on -> s)
      let _ = create_s
      let resolve_s = (fun { a; b } -> { a; b } : s -> derived_on)
```
<!-- $MDX file=test/example_for_mdx_intf.ml,part=naming-type-with-defaults-end -->
```ocaml
    end
```

If the `[@with_defaults]` attribute is given a type argument that isn't named `t`, e.g.
`[@with_defaults: Custom.custom]`, the derived `create` and `resolve` functions will call
`Custom.create_custom` and `Custom.resolve_custom`.

Stable flag
-----------

By default, `ppx_or_default` uses `Or_default.t` for fields annotated with `[@default]`.
However, `Or_default.t` doesn't have `bin_io` support at the top level (only
`Or_default.Stable.V1.t` does). To enable `bin_io` derivation on types with defaults,
you can use the `~stable` flag:

<!-- $MDX file=test/example_for_mdx_intf.ml,part=stable-flag-type -->
```ocaml
  type t =
    { queue_length : int [@default 5]
    ; user_name : string
    }
  [@@deriving bin_io, or_default ~stable, sexp]
```

This will generate a `With_defaults` module that uses `Or_default.Stable.V1.t` instead
of `Or_default.t`, allowing `bin_io` to be derived on the `With_defaults.t` type:

<!-- $MDX file=test/example_for_mdx_intf.ml,part=stable-flag-with-defaults -->
```ocaml
    module With_defaults = struct
      type nonrec derived_on = t

      type t =
        { queue_length : int Or_default.Stable.V1.t
        ; user_name : string
        }
      [@@deriving bin_io, sexp]

      let create =
        (fun { queue_length; user_name } ->
           { queue_length = Or_default.Custom queue_length; user_name }
         : derived_on -> t)
      ;;

      let _ = create

      let resolve =
        (fun { queue_length; user_name } ->
           { queue_length = Or_default.resolve queue_length ~default:5; user_name }
         : t -> derived_on)
      ;;
```
