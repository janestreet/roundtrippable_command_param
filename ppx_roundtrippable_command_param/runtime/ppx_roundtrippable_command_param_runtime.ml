open! Core

let create = Roundtrippable_arg_type.create

let roundtrippable_arg_type_bool =
  create ~arg_type:Command.Param.bool ~to_string:Bool.to_string ~arg_placeholder:"BOOL"
;;

let roundtrippable_arg_type_string =
  create ~arg_type:Command.Param.string ~to_string:Fn.id ~arg_placeholder:"STRING"
;;

let roundtrippable_arg_type_int =
  create ~arg_type:Command.Param.int ~to_string:Int.to_string ~arg_placeholder:"INT"
;;

let roundtrippable_arg_type_char =
  create ~arg_type:Command.Param.char ~to_string:Char.to_string ~arg_placeholder:"CHAR"
;;

let roundtrippable_arg_type_float =
  create ~arg_type:Command.Param.float ~to_string:Float.to_string ~arg_placeholder:"FLOAT"
;;

let roundtrippable_arg_type_sexp =
  create ~arg_type:Command.Param.sexp ~to_string:Sexp.to_string ~arg_placeholder:"SEXP"
;;

module Date = struct
  include Core.Date

  let roundtrippable_arg_type =
    Roundtrippable_arg_type.create
      ~arg_type:Command.Param.date
      ~to_string:Core.Date.to_string
      ~arg_placeholder:"DATE"
  ;;
end

module Percent = struct
  include Core.Percent

  let roundtrippable_arg_type =
    Roundtrippable_arg_type.create
      ~arg_type:Command.Param.percent
      ~to_string:Core.Percent.to_string
      ~arg_placeholder:"PERCENT"
  ;;
end

module Host_and_port = struct
  include Core.Host_and_port

  let roundtrippable_arg_type =
    Roundtrippable_arg_type.create
      ~arg_type:Command.Param.host_and_port
      ~to_string:Core.Host_and_port.to_string
      ~arg_placeholder:"HOST_AND_PORT"
  ;;
end

module Time_ns = struct
  include Core.Time_ns

  module Span = struct
    include Core.Time_ns.Span

    let roundtrippable_arg_type =
      Roundtrippable_arg_type.create
        ~arg_type:Core.Time_ns.Span.arg_type
        ~to_string:Core.Time_ns.Span.to_string
        ~arg_placeholder:"SPAN"
    ;;
  end
end

module Filename = struct
  include Core.Filename

  let roundtrippable_arg_type =
    Roundtrippable_arg_type.create
      ~arg_type:(Command.Arg_type.map File_path.arg_type ~f:File_path.to_string)
      ~to_string:Fn.id
      ~arg_placeholder:"FILE"
  ;;
end

module File_path = struct
  include File_path

  let roundtrippable_arg_type =
    Roundtrippable_arg_type.create
      ~arg_type:File_path.arg_type
      ~to_string:File_path.to_string
      ~arg_placeholder:"PATH"
  ;;
end
