open! Core
include Roundtrippable_arg_type_intf

type 'a t =
  { arg_type : 'a Command.Arg_type.t
  ; to_string : ('a -> string) Staged.t
  ; arg_placeholder : string
  }
[@@deriving fields ~getters]

let%template create ~arg_type ~to_string ~arg_placeholder =
  { arg_type; to_string = (Staged.stage [@mode p]) to_string; arg_placeholder }
[@@mode p = (nonportable, portable)]
;;

let of_arg_type_and_to_string
  (type a)
  (module M : Arg_type_and_to_string with type t = a)
  ~arg_placeholder
  =
  create ~arg_type:M.arg_type ~to_string:M.to_string ~arg_placeholder
;;

module Of_arg_type_and_to_string (M : Arg_for_include_functor) = struct
  let roundtrippable_arg_type =
    create ~arg_type:M.arg_type ~to_string:M.to_string ~arg_placeholder:M.arg_placeholder
  ;;
end

let comma_separated { arg_type; to_string; arg_placeholder } =
  let to_string value =
    String.concat ~sep:"," (List.map ~f:(Staged.unstage to_string) value)
  in
  { arg_type = Command.Arg_type.comma_separated arg_type
  ; to_string = Staged.stage to_string
  ; arg_placeholder = String.concat ~sep:"," [ arg_placeholder; "..." ]
  }
;;

let map t ~f_output ~f_input =
  { t with
    arg_type = Command.Arg_type.map t.arg_type ~f:f_output
  ; to_string = Staged.stage (fun value -> value |> f_input |> Staged.unstage t.to_string)
  }
;;
