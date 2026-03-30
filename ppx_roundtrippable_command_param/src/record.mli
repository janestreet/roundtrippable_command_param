open! Base
open! Ppxlib

(** For record types, we generate a [roundtrippable_command_param] by combining the
    [roundtrippable_command_param] of all fields. Each field's
    [roundtrippable_command_param] is derived from its:
    - Field name
    - Type
    - Docstring
    - Potential custom attributes *)
val rcp_definition_for_record
  :  loc:location
  -> label loc
  -> label_declaration list
  -> structure_item

(** Looks at the fields to see if there is a default value.

    - If there is not, the signature is [(t, t) Roundtrippable_command_param.T2.t]
    - If there is, the signature is
      [(t, With_defaults.t) Roundtrippable_command_param.T2.t] *)
val rcp_declaration_for_record
  :  loc:location
  -> label loc
  -> core_type
  -> label_declaration list
  -> signature_item
