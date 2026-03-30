open! Base

let name_of_type_name ~prefix = function
  | "t" -> prefix
  | type_name -> prefix ^ "_" ^ type_name
;;

(** For a type [Foo.t], the convention is to put the generated roundtrippable command
    param value at [Foo.roundtrippable_arg_type]. For [Foo.custom_type], use
    [Foo.roundtrippable_arg_type_custom_type]. *)
let roundtrippable_arg_type_name_of_type_name =
  name_of_type_name ~prefix:"roundtrippable_arg_type"
;;

(** For a type [Foo.t], the convention is to put the generated roundtrippable command
    param value at [Foo.roundtrippable_command_param]. For [Foo.custom_type], use
    [Foo.roundtrippable_command_param_custom_type]. *)
let rcp_variable_name_of_type_name =
  name_of_type_name ~prefix:"roundtrippable_command_param"
;;

let param_variable_name_of_type_name = name_of_type_name ~prefix:"param"
