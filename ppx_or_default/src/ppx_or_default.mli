open! Base

(** [or_default] creates a new module [With_defaults] module from the annotated type.

    It makes it easy to write code of the following form:
    {[
      type t = { some_int : int }

      module With_defaults = struct
        type derived_on = t
        type t = { some_int : int Or_default.t }

        val create : derived_on -> t
        val resolve : t -> derived_on
      end
    ]}

    You can use the ppx like this:
    {[
      type t = { some_int : int [@default 42] } [@deriving or_default]
    ]}

    See the README.mdx in the parent directory for more examples. *)
val or_default : Ppxlib.Deriving.t
