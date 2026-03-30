open! Base
open! Ppxlib

(** For abstract types ([type t = Existing_module.t]), we assume the referenced module
    already has a [roundtrippable_command_param] definition. We directly use that module's
    param definition rather than generating a new one.

    For example, given [type t = Foo.t], we generate:
    [let roundtrippable_command_param = Foo.roundtrippable_command_param] *)
val rcp_definition_for_abstract : loc:location -> label loc -> core_type -> structure_item

(** [val roundtrippable_command_param = t Roundtrippable_command_param.t] *)
val rcp_declaration_for_abstract
  :  loc:location
  -> label loc
  -> core_type
  -> signature_item
