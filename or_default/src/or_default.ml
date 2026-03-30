open! Core

module Stable = struct
  module V1 = struct
    type 'a t =
      | Custom of 'a
      | Default
    [@@deriving bin_io, compare ~localize, equal ~localize, sexp, stable_witness]
  end
end

type 'a t = 'a Stable.V1.t =
  | Custom of 'a
  | Default
[@@deriving compare ~localize, equal ~localize, sexp, sexp_grammar, variants]

let return a = Custom a

let of_option = function
  | Some value -> Custom value
  | None -> Default
;;

let to_option = function
  | Custom custom_value -> Some custom_value
  | Default -> None
;;

let resolve t ~default =
  match t with
  | Default -> default
  | Custom a -> a
;;

let create_optional_param_with_default_doc ?aliases name arg_type ~default ~to_string ~doc
  =
  Roundtrippable_command_param.create_optional
    ?aliases
    name
    arg_type
    ~doc:[%string {|%{doc} (default: %{to_string default})|}]
    ~to_string
  |> Roundtrippable_command_param.map ~f:of_option
  |> Roundtrippable_command_param.contra_map ~f:to_option
;;

let create_optional_param_with_default_doc'
  ?aliases
  name
  arg_type
  ~default
  ~to_string
  ~doc
  =
  create_optional_param_with_default_doc ?aliases name arg_type ~default ~to_string ~doc
  |> Roundtrippable_command_param.map ~f:(resolve ~default)
;;

let create_optional_param_with_default_doc_from_enum
  ?represent_choice_with
  ?list_values_in_help
  ?aliases
  ?key
  name
  enum
  ~default
  ~doc
  =
  Roundtrippable_command_param.create_optional_from_enum
    ?represent_choice_with
    ?list_values_in_help
    ?aliases
    ?key
    name
    enum
    ~doc:[%string {|%{doc} (default: %{Enum.to_string_hum enum default})|}]
  |> Roundtrippable_command_param.map ~f:of_option
  |> Roundtrippable_command_param.contra_map ~f:to_option
;;

let create_optional_param_with_default_doc_from_enum'
  ?represent_choice_with
  ?list_values_in_help
  ?aliases
  ?key
  name
  enum
  ~default
  ~doc
  =
  create_optional_param_with_default_doc_from_enum
    ?represent_choice_with
    ?list_values_in_help
    ?aliases
    ?key
    name
    enum
    ~default
    ~doc
  |> Roundtrippable_command_param.map ~f:(resolve ~default)
;;
