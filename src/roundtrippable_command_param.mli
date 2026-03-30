open Base
open! Import

(** A roundtrippable command param is a command param that also understands how to
    serialize a value into command line arguments. This is useful for situations where you
    need to shell out to a command whose interface you control, ensuring that the
    [Command.Param.t] and string arguments match up.

    See the README in lib/roundtrippable_command_param/doc for a tutorial-style
    introduction.

    This library chooses to be opinionated about default arguments: it never omits them
    unless the user specifically requests for that to happen (e.g. passing [None] into
    [create_optional_with_default].) The library chooses this position in order to make
    command lines that may diverge (e.g. the default value for a param changes over time)
    obvious and easy to understand. *)

module T2 : sig
  (** A [(a, b) t] is like a roundtrippable [a Command.Param.t]. It can also convert a [b]
      into a list of strings to be used as command line arguments. In practice, [a] and
      [b] will usually be the same, but allowing them to differ means it is possible to
      support things like [optional_with_default] params. *)
  type (+'a, -'b) t

  (** The [command_args] argument must agree with the [param] argument. This is a low
      level constructor, most users should consider to use other constructors. *)
  val create : param:'a Command.Param.t -> command_args:('b -> string list) -> ('a, 'b) t
end

(** The main type of the library is presented in this simplified form. In practice the two
    arguments of [T2.t] usually appear in signatures as the same type. *)
type 'a t = ('a, 'a) T2.t

(** The [Profunctor] implementation makes [Roundtrippable_command_param] compatible with
    [Record_builder]. *)
include Profunctor.S with type ('a, 'b) t := ('a, 'b) T2.t

module Record_builder :
  Profunctor.Record_builder
  with type ('a, 'b) profunctor = ('a, 'b) T2.t
   and type 'a profunctor_term = ('a, 'a) T2.t

(** This [Applicative] implementation is the same as [Command.Param]'s. It requires the
    secondary type to match everywhere, but you can correct mismatching types using the
    [contra_map] function from the [Profunctor] signature. *)
include Applicative.S2 with type ('a, 'b) t := ('a, 'b) T2.t

(** Convert a roundtrippable command param into a plain [Command.Param.t]. *)
val param : ('a, _) T2.t -> 'a Command.Param.t

(** Given a roundtrippable command param, convert a value into a list of arguments that
    can be parsed by the param. *)
val command_args : (_, 'b) T2.t -> 'b -> string list

(** Given a parameter name, return how it would show up on the command line: prefixed with
    a dash if it isn't already. *)
val name_with_dash : string -> string

(** Create an optional flag. The [to_string] argument must agree with the
    [Command.Arg_type.t] argument. *)
val create_optional
  :  ?aliases:string list
  -> ?full_flag_required:unit
  -> string
  -> 'a Command.Arg_type.t
  -> doc:string
  -> to_string:('a -> string)
  -> 'a option t

(** [create_optional_with_default ?aliases ?default_value_doc_string name arg_type ~default ~to_string ~doc]
    is analagous to [Command.Flag.optional_with_default]. If [default_value_doc_string] is
    provided, it will be used for the default value specified in the help documentation;
    otherwise [to_string default] will be used. When the associated value is [None], the
    default is not serialized to command arguments. [to_string] is used to document the
    default value and to serialize values for the command line. *)
val create_optional_with_default
  :  ?aliases:string list
  -> ?default_value_doc_string:string
  -> string
  -> 'a Command.Arg_type.t
  -> default:'a
  -> to_string:('a -> string)
  -> doc:string
  -> ('a, 'a option) T2.t

(** Like [create_optional_with_default], but automatically drops the default value from
    the command args. *)
val create_optional_with_default_drop_default
  :  ?aliases:string list
  -> ?default_value_doc_string:string
  -> string
  -> 'a Command.Arg_type.t
  -> default:'a
  -> equal:('a -> 'a -> bool)
  -> to_string:('a -> string)
  -> doc:string
  -> 'a t

(** Create an optional flag from an [Enum.t]. This function is preferred over
    [create_optional] because it fills in the possible inputs in the documentation. *)
val create_optional_from_enum
  :  ?case_sensitive:bool
  -> ?represent_choice_with:string
  -> ?list_values_in_help:bool
  -> ?aliases:string list
  -> ?key:'a Univ_map.Multi.Key.t
  -> string
  -> 'a Enum.t
  -> doc:string
  -> 'a option t

(** Create an optional with default flag from an [Enum.t]. This function is preferred over
    [create_optional_with_default] because it fills in the possible inputs in the
    documentation. *)
val create_optional_with_default_from_enum
  :  ?case_sensitive:bool
  -> ?represent_choice_with:string
  -> ?list_values_in_help:bool
  -> ?aliases:string list
  -> ?key:'a Univ_map.Multi.Key.t
  -> string
  -> 'a Enum.t
  -> default:'a
  -> doc:string
  -> ('a, 'a option) T2.t

(** Create a listed flag. The [to_string] argument must agree with the
    [Command.Arg_type.t] argument. *)
val create_listed
  :  ?aliases:string list
  -> string
  -> 'a Command.Arg_type.t
  -> doc:string
  -> to_string:('a -> string)
  -> 'a list t

(** Create a listed flag that must be passed one or more times. The [to_string] argument
    must agree with the [Command.Arg_type.t] argument. *)
val create_one_or_more_as_pair
  :  ?aliases:string list
  -> string
  -> 'a Command.Arg_type.t
  -> doc:string
  -> to_string:('a -> string)
  -> ('a * 'a list) t

(** Like [create_one_or_more_as_pair], but the flag values are given as a list. *)
val create_one_or_more_as_list
  :  ?aliases:string list
  -> string
  -> 'a Command.Arg_type.t
  -> doc:string
  -> to_string:('a -> string)
  -> 'a list t

(** Create a required flag. The [to_string] argument must agree with the
    [Command.Arg_type.t] argument. *)
val create_required
  :  ?aliases:string list
  -> string
  -> 'a Command.Arg_type.t
  -> to_string:('a -> string)
  -> doc:string
  -> 'a t

(** Create a required flag from an [Enum.t]. This function is preferred over
    [create_required] because it fills in the possible inputs in the documentation. *)
val create_required_from_enum
  :  ?case_sensitive:bool
  -> ?represent_choice_with:string
  -> ?list_values_in_help:bool
  -> ?aliases:string list
  -> ?key:'a Univ_map.Multi.Key.t
  -> string
  -> 'a Enum.t
  -> doc:string
  -> 'a t

(** Creates a set of flags from an [Enum.t]. Callers must pass exactly one of these flags. *)
val create_flags_from_enum
  :  ?aliases:('a -> string list)
  -> 'a Enum.t
  -> doc:('a -> string)
  -> 'a t

(** Create a flag with no arguments. *)
val create_no_arg : ?aliases:string list -> string -> doc:string -> bool t

(** [create_no_arg_some name value] creates a flag with no arguments that returns
    [Some value] if the flag is passed. *)
val create_no_arg_some : ?aliases:string list -> string -> 'a -> doc:string -> 'a option t

(** Creates a required flag with no arguments. *)
val create_no_arg_required : ?aliases:string list -> string -> doc:string -> unit t

module Anons : sig
  module T2 : sig
    type ('a, 'b) t
  end

  type 'a t = ('a, 'a) T2.t

  (** Create an anonymous argument. The [to_string] argument must agree with the
      [Command.Arg_type.t] argument. *)
  val one : string -> 'a Command.Arg_type.t -> to_string:('a -> string) -> 'a t

  (** Make an anonymous argument optional. *)
  val maybe : 'a t -> 'a option t

  (** Make an anonymous argument a repeated version of the anonymous argument. *)
  val sequence : 'a t -> 'a list t

  (** [non_empty_sequence_as_pair anons] is like [sequence anons] except that the list
      cannot be empty *)
  val non_empty_sequence_as_pair : 'a t -> ('a * 'a list) t

  (** [non_empty_sequence_as_list anons] is like [sequence anons] except that an exception
      will be thrown if the list is empty *)
  val non_empty_sequence_as_list : 'a t -> 'a list t

  (** Make an anonymous argument optional with a default. Passing in [None] when
      serializing will return the default value upon deserialization *)
  val maybe_with_default : 'a -> 'a t -> ('a, 'a option) T2.t

  (** [t2], [t3], and [t4] each concatenate multiple anonymous argument specs into a
      single one. These allow multiple arguments to be passed to functions like [maybe]
      and [sequence], above. See [Command.Param]'s documentation of the [t2], [t3], and
      [t4] functions for more information. *)

  val t2 : 'a t -> 'b t -> ('a * 'b) t
  val t3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val t4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

  (** Maps the parsed anonymous argument (output) type *)
  val map : ('a, 'c) T2.t -> f:('a -> 'b) -> ('b, 'c) T2.t

  (** Maps the serialized anonymous argument (input) type *)
  val contra_map : ('a, 'b) T2.t -> f:('c -> 'b) -> ('a, 'c) T2.t
end

(** Users of [anon] should be careful when combining more than one anonymous parameter
    when constructing a full command line - the position of strings determines which
    anonymous argument is provided. For example, if the [Profunctor] interface to this
    module is used in conjunction with [Record_builder] and a field is reordered, the
    corresponding command must also have the same reordering. *)
val anon : ('a, 'b) Anons.T2.t -> ('a, 'b) T2.t

(** {2 Variants}

    The [*variant*] functions can be used to build a [t] from a variant type where each
    constructor uses a separate param. It's intended to work with [Variants.make_matcher]
    from [@@deriving variants].

    See lib/roundtrippable_command_param/doc/variant_builder.mdx for examples. *)

module Variant_builder : sig
  type 'a t

  module Match_result : sig
    type t
  end

  module Case : sig
    type nonrec ('constructor, 'variant, 'matcher) t =
      'constructor Variant.t -> 'variant t -> 'matcher * 'variant t
  end

  module If_nothing_chosen : sig
    (** The [roundtrippable_command_param] analog for [Command.If_nothing_chosen.t] *)
    type (_, _, _) t =
      | Default_to : 'a -> ('a, 'a, 'a option) t
      | Raise : ('a, 'a, 'a) t
      | Return_none : ('a, 'a option, 'a option) t
  end
end

(** [build_variant] returns a [t] for a variant type, using the result of applying
    [Variants.make_matcher] to the appropriate [variant*] functions below. *)
val build_variant
  :  ('variant Variant_builder.t
      -> ('variant -> Variant_builder.Match_result.t) * 'variant Variant_builder.t)
  -> if_nothing_chosen:('variant, 'b, 'c) Variant_builder.If_nothing_chosen.t
  -> ('b, 'c) T2.t

(** [build_set] returns a [t] for a set containing values of the variant type, using the
    result of applying [Variants.make_matcher] to the appropriate [variant*] functions
    below. *)
val build_set
  :  ('variant Variant_builder.t
      -> ('variant -> Variant_builder.Match_result.t) * 'variant Variant_builder.t)
  -> comparable:
       (module Comparable.S with type t = 'variant and type comparator_witness = 'cmp)
  -> (('variant, 'cmp) Set.t, ('variant, 'cmp) Set.t) T2.t

(** The [variant*] functions produce the [make_matcher] case for variants of 0...N
    anonymous arguments. They expect that parsing the command line will make all or none
    of the N arguments be [Some _], and raises otherwise.

    Each of the variant cases will be their own flags, to use a sexp for choices instead
    see [variant]. *)

val variant0
  :  bool t
  -> ('variant, 'variant, unit -> Variant_builder.Match_result.t) Variant_builder.Case.t

val variant1
  :  'a option t
  -> ( 'a -> 'variant
       , 'variant
       , 'a -> Variant_builder.Match_result.t )
       Variant_builder.Case.t

val variant2
  :  'a option t
  -> 'b option t
  -> ( 'a -> 'b -> 'variant
       , 'variant
       , 'a -> 'b -> Variant_builder.Match_result.t )
       Variant_builder.Case.t

val variant3
  :  'a option t
  -> 'b option t
  -> 'c option t
  -> ( 'a -> 'b -> 'c -> 'variant
       , 'variant
       , 'a -> 'b -> 'c -> Variant_builder.Match_result.t )
       Variant_builder.Case.t

(** [variant] is the most general of the [variant*] functions. It works for constructors
    of arbitrary arity, and for constructors with inline records.

    The value of the variant will be passed in through a single flag via a sexp. To
    produce a flag per variant option, use [variant{0,1,2,3}].

    The easiest way to understand the ['matcher] variable is to look at the [variant*]
    functions: ['matcher] is a function like ['constructor], but returning
    [Match_result.t] instead of ['variant]. *)
val variant
  :  'a option t
  -> to_variant:('constructor -> 'a -> 'variant)
  -> to_matcher:(('a -> Variant_builder.Match_result.t) -> 'matcher)
  -> ('constructor, 'variant, 'matcher) Variant_builder.Case.t

(** {2 Convenience functions} *)

(** Create an optional int argument. This is just a convenience function around
    [create_optional]. *)
val int_option : string -> doc:string -> int option t

(** Create an optional string argument. This is just a convenience function around
    [create_optional]. *)
val string_option : string -> doc:string -> string option t

(** [optional_params params] makes a "required" roundtrippable command param optional.

    If [params] required flags [-foo] and [-bar], providing _both_ or _neither_ would be
    valid for [optional_params params] (resulting in [Some _] or [None], respectively. *)
val optional_params : 'a t -> 'a option t

(** [alias_params t params ~if_nothing_chosen] allows [params] to be parsed for a [t]. The
    serialization will not use any of [params]. Users may accept other inputs (e.g. new or
    old versions of a param) using this function. *)
val alias_params
  :  ('a option, 'b) T2.t
  -> 'a option Command.Param.t list
  -> if_nothing_chosen:('a, 'b) Command.Param.If_nothing_chosen.t
  -> 'b t

module Let_syntax : sig
  val return : 'a -> ('a, _) T2.t

  include module type of Applicative_infix

  module Let_syntax : sig
    val return : 'a -> ('a, _) T2.t
    val map : ('a, 'e) T2.t -> f:('a -> 'b) -> ('b, 'e) T2.t
    val both : ('a, 'e) T2.t -> ('b, 'e) T2.t -> ('a * 'b, 'e) T2.t

    module Open_on_rhs : sig
      val contra_map : ('u, 'b) T2.t -> f:('a -> 'b) -> ('u, 'a) T2.t
    end
  end
end

module Variant_builder_non_optional : sig
  type 'a t

  module Match_result : sig
    type t
  end

  module Case : sig
    type nonrec ('constructor, 'variant, 'matcher) t =
      'constructor Variant.t -> 'variant t -> 'matcher * 'variant t
  end

  (** [build_variant] returns a [t] for a variant type, using the result of applying
      [Variants.make_matcher] to the appropriate [variant*] functions below. *)
  val build_variant
    :  ('variant t -> ('variant -> Match_result.t) * 'variant t)
    -> if_nothing_chosen:('variant, 'b, 'c) Variant_builder.If_nothing_chosen.t
    -> ('b, 'c) T2.t

  (** [variant] is the most general of the [variant*] functions. It works for constructors
      of arbitrary arity, and for constructors with inline records.

      The value of the variant will be passed in through a single flag via a sexp. To
      produce a flag per variant option, use [variant{0,1,2,3}].

      The easiest way to understand the ['matcher] variable is to look at the [variant*]
      functions: ['matcher] is a function like ['constructor], but returning
      [Match_result.t] instead of ['variant]. *)
  val variant
    :  ('a, 'a) T2.t
    -> to_variant:('constructor -> 'a -> 'variant)
    -> to_matcher:(('a -> Match_result.t) -> 'matcher)
    -> ('constructor, 'variant, 'matcher) Case.t

  val variant0 : (unit, unit) T2.t -> ('variant, 'variant, unit -> Match_result.t) Case.t
  val variant1 : ('a, 'a) T2.t -> ('a -> 'variant, 'variant, 'a -> Match_result.t) Case.t

  val variant2
    :  ('a, 'a) T2.t
    -> ('b, 'b) T2.t
    -> ('a -> 'b -> 'variant, 'variant, 'a -> 'b -> Match_result.t) Case.t

  val variant3
    :  ('a, 'a) T2.t
    -> ('b, 'b) T2.t
    -> ('c, 'c) T2.t
    -> ('a -> 'b -> 'c -> 'variant, 'variant, 'a -> 'b -> 'c -> Match_result.t) Case.t

  (** [alias_params t params ~if_nothing_chosen] allows [params] to be parsed for a [t].
      The serialization will not use any of [params]. Users may accept other inputs (e.g.
      new or old versions of a param) using this function. *)
  val alias_params
    :  ('a, 'b) T2.t
    -> 'a Command.Param.t list
    -> if_nothing_chosen:('a, 'b) Command.Param.If_nothing_chosen.t
    -> ('b, 'b) T2.t
end
