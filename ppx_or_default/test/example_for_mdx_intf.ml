[@@@disable_unused_warnings]

open! Core

(** {1 Overview} *)

module M : sig
  type t

  (* $MDX part-begin=signature *)
  module With_defaults : sig
    type derived_on = t

    type t =
      { queue_length : int Or_default.t
      ; user_name : string
      }

    val resolve : t -> derived_on
  end
  (* $MDX part-end *)
end = struct
  (* $MDX part-begin=record-1 *)
  type t =
    { queue_length : int [@default 5]
    ; user_name : string
    }
  [@@deriving or_default] (* $MDX part-end *) [@@deriving_inline or_default]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    (* $MDX part-begin=record-1-with-defaults *)
    module With_defaults = struct
      type nonrec derived_on = t

      type t =
        { queue_length : int Or_default.t
        ; user_name : string
        }

      let resolve =
        (fun { queue_length; user_name } ->
           { queue_length = Or_default.resolve queue_length ~default:5; user_name }
         : t -> derived_on)
      ;;

      (* $MDX part-end *)

      let _ = resolve
      (* $MDX part-begin=record-1-with-defaults-end *)
    end
    (* $MDX part-end *)
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

(** {1 Attributes} *)

module T1 = struct
  (* $MDX part-begin=attribute-demo-type *)
  type t =
    { a : int [@default 10]
    ; b : string [@default "hi"]
    ; c : float [@default 12.5]
    ; d : bool [@default false]
    ; e : char [@default 'a'] (* etc *)
    ; no_default : int
    }
  [@@deriving_inline or_default]
  (* $MDX part-end *)

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    (* $MDX part-begin=attribute-demo-type-with-defaults *)
    module With_defaults = struct
      type nonrec derived_on = t

      type t =
        { a : int Or_default.t
        ; b : string Or_default.t
        ; c : float Or_default.t
        ; d : bool Or_default.t
        ; e : char Or_default.t
        ; no_default : int
        }

      let resolve =
        (fun { a; b; c; d; e; no_default } ->
           { a = Or_default.resolve a ~default:10
           ; b = Or_default.resolve b ~default:"hi"
           ; c = Or_default.resolve c ~default:12.5
           ; d = Or_default.resolve d ~default:false
           ; e = Or_default.resolve e ~default:'a'
           ; no_default
           }
         : t -> derived_on)
      ;;

      (* $MDX part-end *)

      let _ = resolve
      (* $MDX part-begin=attribute-demo-type-with-defaults-end *)
    end
    (* $MDX part-end *)
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

module T3 = struct
  (* $MDX part-begin=attribute-demo-nesting *)

  module Inner = struct
    type t =
      { a : int [@default 10]
      ; b : string [@default "hi"]
      ; no_default : int
      }
    [@@deriving or_default]
  end

  type t =
    { inner : Inner.t [@with_defaults]
    ; c : float [@default 12.5]
    ; d : bool [@default false]
    ; e : char [@default 'a'] (* etc *)
    }
  [@@deriving_inline or_default]
  (* $MDX part-end *)

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    (* $MDX part-begin=attribute-demo-nesting-with-defaults *)

    module With_defaults = struct
      type nonrec derived_on = t

      type t =
        { inner : Inner.With_defaults.t
        ; c : float Or_default.t
        ; d : bool Or_default.t
        ; e : char Or_default.t
        }

      let resolve =
        (fun { inner; c; d; e } ->
           { inner = Inner.With_defaults.resolve inner
           ; c = Or_default.resolve c ~default:12.5
           ; d = Or_default.resolve d ~default:false
           ; e = Or_default.resolve e ~default:'a'
           }
         : t -> derived_on)
      ;;

      (* $MDX part-end *)

      let _ = resolve
      (* $MDX part-begin=attribute-demo-nesting-with-defaults-end *)
    end
    (* $MDX part-end *)
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

(** {1 Naming} *)

module T2 = struct
  (* $MDX part-begin=naming-type *)
  type s =
    { a : int
    ; b : string
    }
  [@@deriving_inline or_default]
  (* $MDX part-end *)

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : s) -> ()

    (* $MDX part-begin=naming-type-with-defaults *)
    module S_with_defaults = struct
      type nonrec derived_on = s

      type s =
        { a : int
        ; b : string
        }

      let resolve_s = (fun { a; b } -> { a; b } : s -> derived_on)
      (* $MDX part-end *)

      let _ = resolve_s
      (* $MDX part-begin=naming-type-with-defaults-end *)
    end
    (* $MDX part-end *)
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

(** {1 Stable flag} *)

module T4 = struct
  (* $MDX part-begin=stable-flag-type *)
  type t =
    { queue_length : int [@default 5]
    ; user_name : string
    }
  [@@deriving bin_io, or_default ~stable, sexp]
  (* $MDX part-end *)
  [@@deriving_inline or_default ~stable]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    (* $MDX part-begin=stable-flag-with-defaults *)
    module With_defaults = struct
      type nonrec derived_on = t

      type t =
        { queue_length : int Or_default.Stable.V1.t
        ; user_name : string
        }
      [@@deriving bin_io, sexp]

      let resolve =
        (fun { queue_length; user_name } ->
           { queue_length = Or_default.resolve queue_length ~default:5; user_name }
         : t -> derived_on)
      ;;

      (* $MDX part-end *)

      let _ = resolve
      (* $MDX part-begin=stable-flag-with-defaults-end *)
    end
    (* $MDX part-end *)
  end [@@ocaml.doc "@inline"]

  [@@@end]
end
