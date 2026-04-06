open Core

module type Test_signatures = sig
  module _ : sig
    type 'a t [@@deriving_inline or_default]

    include sig
      [@@@ocaml.warning "-32-60"]

      module With_defaults : sig
        type nonrec 'a derived_on = 'a t
        type 'a t

        val resolve : 'a t -> 'a derived_on
      end
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module _ : sig
    type t = { some_int : int } [@@deriving_inline or_default]

    include sig
      [@@@ocaml.warning "-32-60"]

      module With_defaults : sig
        type nonrec derived_on = t
        type t = { some_int : int }

        val resolve : t -> derived_on
      end
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

module Single_field : sig
  type t = { some_int : int [@default] }
  [@@deriving fields ~getters] [@@deriving_inline or_default]

  include sig
    [@@@ocaml.warning "-32-60"]

    module With_defaults : sig
      type nonrec derived_on = t
      type t = { some_int : int Or_default.t } [@@deriving fields ~getters]

      val resolve : t -> derived_on
    end
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = { some_int : int [@default 5] }
  [@@deriving fields ~getters] [@@deriving_inline or_default]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    module With_defaults = struct
      type nonrec derived_on = t
      type t = { some_int : int Or_default.t } [@@deriving fields ~getters]

      let resolve =
        (fun { some_int } -> { some_int = Or_default.resolve some_int ~default:5 }
         : t -> derived_on)
      ;;

      let _ = resolve
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

let%expect_test "one defaulted field" =
  let with_defaults = Single_field.With_defaults.{ some_int = Default } in
  let resolved_defaults = Single_field.With_defaults.resolve with_defaults in
  Single_field.some_int resolved_defaults |> Int.to_string |> print_endline;
  [%expect {| 5 |}]
;;

module _ = struct
  type single_field = Single_field.t

  module Single_field_with_defaults = struct
    include Single_field.With_defaults

    type single_field = t

    let resolve_single_field = resolve
  end

  type single_field_with_defaults = Single_field.With_defaults.t

  let resolve_single_field_with_defaults = Single_field.With_defaults.resolve

  module With_defaults = struct
    type single_field = Single_field.With_defaults.t

    let resolve_single_field = Single_field.With_defaults.resolve
  end

  module Nested : sig
    type t =
      { single_field : Single_field.t [@with_defaults]
      ; single_field_custom : Single_field.t
           [@with_defaults: Single_field_with_defaults.t]
      ; single_field_custom_not_t : Single_field.t
           [@with_defaults: With_defaults.single_field]
      ; single_field_top_level : single_field [@with_defaults]
      ; single_field_custom_top_level : Single_field.t
           [@with_defaults: single_field_with_defaults]
      }
    [@@deriving fields ~getters] [@@deriving_inline or_default]

    include sig
      [@@@ocaml.warning "-32-60"]

      module With_defaults : sig
        type nonrec derived_on = t

        type t =
          { single_field : Single_field.With_defaults.t
          ; single_field_custom : Single_field_with_defaults.t
          ; single_field_custom_not_t : With_defaults.single_field
          ; single_field_top_level : Single_field_with_defaults.single_field
          ; single_field_custom_top_level : single_field_with_defaults
          }
        [@@deriving fields ~getters]

        val resolve : t -> derived_on
      end
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end = struct
    type t =
      { single_field : Single_field.t [@with_defaults]
      ; single_field_custom : Single_field.t
           [@with_defaults: Single_field_with_defaults.t]
      ; single_field_custom_not_t : Single_field.t
           [@with_defaults: With_defaults.single_field]
      ; single_field_top_level : single_field [@with_defaults]
      ; single_field_custom_top_level : Single_field.t
           [@with_defaults: single_field_with_defaults]
      }
    [@@deriving fields ~getters] [@@deriving_inline or_default]

    include struct
      [@@@ocaml.warning "-60"]

      let _ = fun (_ : t) -> ()

      module With_defaults = struct
        type nonrec derived_on = t

        type t =
          { single_field : Single_field.With_defaults.t
          ; single_field_custom : Single_field_with_defaults.t
          ; single_field_custom_not_t : With_defaults.single_field
          ; single_field_top_level : Single_field_with_defaults.single_field
          ; single_field_custom_top_level : single_field_with_defaults
          }
        [@@deriving fields ~getters]

        let resolve =
          (fun { single_field
               ; single_field_custom
               ; single_field_custom_not_t
               ; single_field_top_level
               ; single_field_custom_top_level
               } ->
             { single_field = Single_field.With_defaults.resolve single_field
             ; single_field_custom =
                 Single_field_with_defaults.resolve single_field_custom
             ; single_field_custom_not_t =
                 With_defaults.resolve_single_field single_field_custom_not_t
             ; single_field_top_level =
                 Single_field_with_defaults.resolve_single_field single_field_top_level
             ; single_field_custom_top_level =
                 resolve_single_field_with_defaults single_field_custom_top_level
             }
           : t -> derived_on)
        ;;

        let _ = resolve
      end
    end [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module _ = Nested
end

module Multiple_fields = struct
  type t =
    { first_int : int [@default 5]
    ; second_int : int [@default 10]
    ; first_string : string [@default "a"]
    }
  [@@deriving or_default, fields ~getters, sexp]
end

let%expect_test "many defaulted field" =
  let with_defaults =
    Multiple_fields.With_defaults.
      { first_int = Default; second_int = Default; first_string = Default }
  in
  let resolved_defaults = Multiple_fields.With_defaults.resolve with_defaults in
  print_s [%sexp (resolved_defaults : Multiple_fields.t)];
  [%expect {| ((first_int 5) (second_int 10) (first_string a)) |}]
;;

module No_defaulted = struct
  type t =
    { first_int : int
    ; second_int : int
    ; first_string : string
    }
  [@@deriving or_default, fields ~getters, sexp]
end

let%expect_test "no defaulted field" =
  let with_defaults =
    No_defaulted.With_defaults.{ first_int = 42; second_int = 12; first_string = "hi!" }
  in
  let resolved_defaults = No_defaulted.With_defaults.resolve with_defaults in
  print_s [%sexp (resolved_defaults : No_defaulted.t)];
  [%expect {| ((first_int 42) (second_int 12) (first_string hi!)) |}]
;;

module Different_type_name = struct
  type some_other_name = { first_int : int [@default 5] }
  [@@deriving or_default, fields ~getters, sexp]
end

let%expect_test "field with different type name" =
  let with_defaults =
    Different_type_name.Some_other_name_with_defaults.{ first_int = Default }
  in
  let resolved_defaults =
    Different_type_name.Some_other_name_with_defaults.resolve_some_other_name
      with_defaults
  in
  print_s [%sexp (resolved_defaults : Different_type_name.some_other_name)];
  [%expect {| ((first_int 5)) |}]
;;

module Manifest_types = struct
  type original_type = { first_int : int }

  type t = original_type = { first_int : int [@default 5] }
  [@@deriving fields ~getters, sexp] [@@deriving_inline or_default]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    module With_defaults = struct
      type nonrec derived_on = t
      type t = { first_int : int Or_default.t } [@@deriving fields ~getters, sexp]

      let resolve =
        (fun { first_int } -> { first_int = Or_default.resolve first_int ~default:5 }
         : t -> derived_on)
      ;;

      let _ = resolve
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

let%expect_test "manifest types" =
  let with_defaults = Manifest_types.With_defaults.{ first_int = Default } in
  let resolved_defaults = Manifest_types.With_defaults.resolve with_defaults in
  print_s [%sexp (resolved_defaults : Manifest_types.t)];
  [%expect {| ((first_int 5)) |}]
;;

module Polymorphic_type = struct
  type 'a t =
    { first_int : int [@default 12]
    ; polymorphic_field : 'a
    }
  [@@deriving or_default, sexp_of]
end

let%expect_test "polymorphic types" =
  let with_defaults =
    Polymorphic_type.With_defaults.{ first_int = Default; polymorphic_field = 5 }
  in
  let resolved_defaults = Polymorphic_type.With_defaults.resolve with_defaults in
  print_s [%sexp (resolved_defaults : int Polymorphic_type.t)];
  [%expect {| ((first_int 12) (polymorphic_field 5)) |}]
;;

module A_bit_of_everything = struct
  module Inner = struct
    type t =
      { first_inner : int [@default 7]
      ; second_inner : int
      }
    [@@deriving or_default, fields ~getters, sexp]
  end

  type 'a some_original_type =
    { first_int : int
    ; second_int : int
    ; first_string : string
    ; mutable first_mutable_string : string
    ; mutable second_mutable_int_with_default : int
    ; polymorphic_field : 'a
    ; mutable polymorphic_mutable_field : 'a
    ; inner : Inner.t
    }

  type 'a t = 'a some_original_type =
    { first_int : int [@default 5]
    ; second_int : int
    ; first_string : string [@default "a"]
    ; mutable first_mutable_string : string
    ; mutable second_mutable_int_with_default : int [@default 12]
    ; polymorphic_field : 'a
    ; mutable polymorphic_mutable_field : 'a
    ; inner : Inner.t [@with_defaults]
    }
  [@@deriving fields ~getters, sexp] [@@deriving_inline or_default]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : 'a t) -> ()

    module With_defaults = struct
      type nonrec 'a derived_on = 'a t

      type 'a t =
        { first_int : int Or_default.t
        ; second_int : int
        ; first_string : string Or_default.t
        ; mutable first_mutable_string : string
        ; mutable second_mutable_int_with_default : int Or_default.t
        ; polymorphic_field : 'a
        ; mutable polymorphic_mutable_field : 'a
        ; inner : Inner.With_defaults.t
        }
      [@@deriving fields ~getters, sexp]

      let resolve =
        (fun { first_int
             ; second_int
             ; first_string
             ; first_mutable_string
             ; second_mutable_int_with_default
             ; polymorphic_field
             ; polymorphic_mutable_field
             ; inner
             } ->
           { first_int = Or_default.resolve first_int ~default:5
           ; second_int
           ; first_string = Or_default.resolve first_string ~default:"a"
           ; first_mutable_string
           ; second_mutable_int_with_default =
               Or_default.resolve second_mutable_int_with_default ~default:12
           ; polymorphic_field
           ; polymorphic_mutable_field
           ; inner = Inner.With_defaults.resolve inner
           }
         : 'a t -> 'a derived_on)
      ;;

      let _ = resolve
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

let%expect_test "some defaulted field" =
  let with_defaults =
    A_bit_of_everything.With_defaults.
      { first_int = Default
      ; second_int = 12
      ; first_string = Default
      ; first_mutable_string = "first"
      ; second_mutable_int_with_default = Default
      ; polymorphic_field = 15
      ; polymorphic_mutable_field = 42
      ; inner = { first_inner = Default; second_inner = 9 }
      }
  in
  let resolved_defaults = A_bit_of_everything.With_defaults.resolve with_defaults in
  print_s [%sexp (resolved_defaults : int A_bit_of_everything.t)];
  [%expect
    {|
    ((first_int 5) (second_int 12) (first_string a) (first_mutable_string first)
     (second_mutable_int_with_default 12) (polymorphic_field 15)
     (polymorphic_mutable_field 42) (inner ((first_inner 7) (second_inner 9))))
    |}]
;;

module Type_with_modalities = struct
  type with_modalities =
    { global_ first : string
    ; global_ second : string [@default "text"]
    }
  [@@deriving_inline or_default]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : with_modalities) -> ()

    module With_modalities_with_defaults = struct
      type nonrec derived_on = with_modalities

      type with_modalities =
        { global_ first : string
        ; global_ second : string Or_default.t
        }

      let resolve_with_modalities =
        (fun { first; second } ->
           { first; second = Or_default.resolve second ~default:"text" }
         : with_modalities -> derived_on)
      ;;

      let _ = resolve_with_modalities
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

module _ = Type_with_modalities

module Attributes_on_fields = struct
  type t = { first : string [@sexp.default "test"] }
  [@@deriving sexp] [@@deriving_inline or_default]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    module With_defaults = struct
      type nonrec derived_on = t
      type t = { first : string } [@@deriving sexp]

      let resolve = (fun { first } -> { first } : t -> derived_on)
      let _ = resolve
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

module _ = Attributes_on_fields

module With_other_derivers = struct
  type t =
    { num : int [@default 42]
    ; word : string
    }
  [@@deriving compare, equal, or_default, sexp]
end

let%expect_test "with other derivers including compare and equal" =
  (* Test that the original type has all the derivers *)
  let t1 = { With_other_derivers.num = 42; word = "hello" } in
  let t2 = { With_other_derivers.num = 42; word = "world" } in
  print_s [%sexp ([%compare: With_other_derivers.t] t1 t2 : int)];
  [%expect {| -1 |}];
  print_s [%sexp (With_other_derivers.equal t1 t1 : bool)];
  [%expect {| true |}];
  print_s [%sexp (With_other_derivers.equal t1 t2 : bool)];
  [%expect {| false |}];
  (* Test that the With_defaults module also has the derivers *)
  let wd1 = { With_other_derivers.With_defaults.num = Default; word = "hello" } in
  let wd2 = { With_other_derivers.With_defaults.num = Custom 100; word = "world" } in
  print_s [%sexp ([%compare: With_other_derivers.With_defaults.t] wd1 wd2 : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%equal: With_other_derivers.With_defaults.t] wd1 wd1 : bool)];
  [%expect {| true |}];
  print_s [%sexp ([%equal: With_other_derivers.With_defaults.t] wd1 wd2 : bool)];
  [%expect {| false |}];
  print_s [%sexp (With_other_derivers.With_defaults.resolve wd1 : With_other_derivers.t)];
  [%expect {| ((num 42) (word hello)) |}];
  print_s [%sexp (With_other_derivers.With_defaults.resolve wd2 : With_other_derivers.t)];
  [%expect {| ((num 100) (word world)) |}]
;;

module With_stable = struct
  type t =
    { num : int [@default 42]
    ; word : string
    }
  [@@deriving bin_io, sexp, equal] [@@deriving_inline or_default ~stable]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    module With_defaults = struct
      type nonrec derived_on = t

      type t =
        { num : int Or_default.Stable.V1.t
        ; word : string
        }
      [@@deriving bin_io, sexp, equal]

      let resolve =
        (fun { num; word } -> { num = Or_default.resolve num ~default:42; word }
         : t -> derived_on)
      ;;

      let _ = resolve
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

module type Bin_and_equal = sig
  include Binable.S
  include Expect_test_helpers_core.With_equal with type t := t
end

let%expect_test "with bin_io and stable flag" =
  (* Test that the original type has bin_io *)
  let test_bin_roundtrip (type a) (module M : Bin_and_equal with type t = a) (orig : a) =
    let bin_io_formatted = Binable.to_bigstring (module M) orig in
    let roundtripped = Binable.of_bigstring (module M) bin_io_formatted in
    Expect_test_helpers_core.require_equal (module M) orig roundtripped;
    print_s [%sexp (orig : M.t)]
  in
  test_bin_roundtrip (module With_stable) { With_stable.num = 42; word = "hello" };
  [%expect {| ((num 42) (word hello)) |}];
  let with_defaults = { With_stable.With_defaults.num = Custom 100; word = "hello" } in
  print_s [%sexp (With_stable.With_defaults.resolve with_defaults : With_stable.t)];
  [%expect {| ((num 100) (word hello)) |}];
  test_bin_roundtrip (module With_stable.With_defaults) with_defaults;
  [%expect {| ((num (Custom 100)) (word hello)) |}]
;;
