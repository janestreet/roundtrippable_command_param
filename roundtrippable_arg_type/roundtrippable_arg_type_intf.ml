open! Core

module type Arg_type_and_to_string = sig
  type t

  val to_string : t -> string
  val arg_type : t Command.Arg_type.t
end

module type Arg_for_include_functor = sig
  include Arg_type_and_to_string

  val arg_placeholder : string
end

module type Roundtrippable_arg_type = sig
  module type Arg_type_and_to_string = Arg_type_and_to_string
  module type Arg_for_include_functor = Arg_for_include_functor

  (** We introduce the idea of [Roundtrippable_arg_type] to achieve better compatibility
      with [Roundtrippable_command_param]:

      - [arg_type] and [to_string] defines both how parsing and formatting works,
        respectively. Together they define the roundtrippable behavior of an arg_type.
        User is responsible that these two roundtrips while creating a
        [Roundtrippable_arg_type.t].
      - [arg_placeholder] is intended to be used when generating documentation for a
        param. For instance, the [INT] in [-flag INT . doc].

      If you are using [ppx_roundtrippable_command_param], the [roundtrippable_arg_type]
      is present in the runtime library {!module:Ppx_roundtrippable_command_param_runtime}
      and does not need to be specified for primitive types and some common types.

      A customized example for enumerated_sexpable type:

      {[
        module Example = struct
          type t =
            | Foo
            | Bar
          [@@deriving enumerate, sexp_of]
        end

        let roundtrippable_arg_type =
          Roundtrippable_arg_type.create
            ~arg_type:(Command.Arg_type.enumerated_sexpable (module Example))
            ~to_string:(fun x -> [%sexp_of: Example.t] x |> Sexp.to_string)
            ~arg_placeholder:"(Foo|Bar)"
        ;;
      ]} *)
  type 'a t =
    { arg_type : 'a Command.Arg_type.t
    ; to_string : ('a -> string) Staged.t
    ; arg_placeholder : string
    (** For example, int [-foo FOO the foo to use], [FOO] is the [arg_placeholder].

        ["_"] is a common default if there is not a clear placeholder. This is often used
        when valid values are enumerated in [arg_type]. *)
    }
  [@@deriving fields ~getters]

  (** Creating [Roundtrippable_arg_type.t] achieves better compatibility with
      [Roundtrippable_command_param]. [arg_type] and [to_string] defines both how parsing
      and formatting works, respectively. Together they define the roundtrippable behavior
      of an arg_type. [arg_placeholder] is intended to be used when generating
      documentation for a param. *)
  val%template create
    :  arg_type:'a Command.Arg_type.t @ p
    -> to_string:('a -> string) @ p
    -> arg_placeholder:string
    -> 'a t @ p
  [@@mode p = (nonportable, portable)]

  val of_arg_type_and_to_string
    :  (module Arg_type_and_to_string with type t = 'a)
    -> arg_placeholder:string
    -> 'a t

  module Of_arg_type_and_to_string (M : Arg_for_include_functor) : sig
    val roundtrippable_arg_type : M.t t
  end

  (** Provide one or more separated by commas.

      Note that nothing checks that the [to_string] representation doesn't have any
      commas, so this is not guaranteed to round-trip perfectly. *)
  val comma_separated : 'a t -> 'a list t

  val map : 'a t -> f_output:('a -> 'b) -> f_input:('b -> 'a) -> 'b t
end
