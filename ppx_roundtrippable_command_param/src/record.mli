open! Base
open! Ppxlib

(** For record types, we generate one or two [roundtrippable_command_param] bindings by
    combining the [roundtrippable_command_param] of all fields. Each field's
    [roundtrippable_command_param] is derived from its:
    - Field name
    - Type
    - Docstring
    - Potential custom attributes

    When no field has a default, a single [roundtrippable_command_param] binding is
    emitted.

    When some field has a default, two bindings are emitted:
    - [roundtrippable_command_param_with_defaults] (or
      [roundtrippable_command_param_<name>_with_defaults] for non-[t] types) with shape
      [(t, With_defaults.t) Roundtrippable_command_param.T2.t], and
    - [roundtrippable_command_param] with shape [t Roundtrippable_command_param.t],
      obtained by [contra_map]'ing the with-defaults binding through the
      [With_defaults.create] function emitted by [ppx_or_default]. *)
val rcp_definition_for_record
  :  loc:location
  -> label loc
  -> label_declaration list
  -> structure_item list

(** Looks at the fields to see if there is a default value.

    - If there is not, a single [roundtrippable_command_param] declaration is emitted with
      type [(t, t) Roundtrippable_command_param.T2.t].
    - If there is, two declarations are emitted: a
      [roundtrippable_command_param_with_defaults] of type
      [(t, With_defaults.t) Roundtrippable_command_param.T2.t] and a
      [roundtrippable_command_param] of type [t Roundtrippable_command_param.t]. *)
val rcp_declaration_for_record
  :  loc:location
  -> label loc
  -> core_type
  -> label_declaration list
  -> signature_item list
