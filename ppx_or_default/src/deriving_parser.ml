open Base
open Ppxlib

type parsed_args = Args of (arg_label * expression) list

let generator_name_of_id _loc id =
  try Some (Longident.flatten_exn id |> String.concat ~sep:".") with
  | _ -> None
;;

let parse_arguments l = Args l

let deriving_attr_pattern () =
  (* The following code is copied from external/ppxlib/src/deriving.ml: *)
  Ast_pattern.(
    let generator_name () =
      map' (pexp_ident __) ~f:(fun loc f id -> f (generator_name_of_id loc id))
    in
    let generator () =
      map (generator_name ()) ~f:(fun f x -> f (x, Args []))
      ||| pack2 (pexp_apply (generator_name ()) (map1 (many __) ~f:parse_arguments))
    in
    let generators =
      pexp_tuple (many (generator ())) ||| map (generator ()) ~f:(fun f x -> f [ x ])
    in
    pstr (pstr_eval generators nil ^:: nil))
;;

(* End copy *)

module Extracted_attribute_details = struct
  (** A convenient subset of the information in an attribute *)
  type t =
    { name : string
    ; loc : Location.t
    ; generators : (string * parsed_args) list
    }
end

let rec consume (attributes : attributes)
  : (attributes * Extracted_attribute_details.t) option
  =
  match attributes with
  | [] -> None
  | { attr_name =
        { txt =
            ("deriving" | "deriving_inline" | "ppxlib.deriving" | "ppxlib.deriving_inline")
            as name
        ; _
        }
    ; attr_payload = payload
    ; attr_loc = loc
    ; _
    }
    :: attributes ->
    let generators =
      let%bind.Option generators =
        Ast_pattern.parse_res (deriving_attr_pattern ()) loc payload Fn.id |> Result.ok
      in
      List.map generators ~f:(fun (name, args) ->
        Option.map name ~f:(fun name -> name, args))
      |> Option.all
    in
    (match generators with
     | Some generators -> Some (attributes, { name; loc; generators })
     | None -> consume attributes)
  | _ :: attributes -> consume attributes
;;

let to_attribute ({ name; loc; generators } : Extracted_attribute_details.t)
  : attribute option
  =
  let open (val Ast_builder.make loc) in
  match Nonempty_list.of_list generators with
  | None -> None
  | Some generators ->
    let entries =
      Nonempty_list.map generators ~f:(fun (name, Args args) ->
        let name = evar name in
        if List.is_empty args then name else pexp_apply name args)
    in
    Some
      (attribute
         ~name:{ loc; txt = name }
         ~payload:
           (PStr
              [ pstr_eval
                  (match entries with
                   | [ entry ] -> entry
                   | _ :: _ :: _ as entries -> pexp_tuple (Nonempty_list.to_list entries))
                  []
              ]))
;;

let rec remove_from_deriving_attributes attributes ~exclude =
  match consume attributes with
  | None -> attributes
  | Some (type_decl, { name; loc; generators }) ->
    let attributes = remove_from_deriving_attributes type_decl ~exclude in
    let generators =
      List.filter generators ~f:(fun (name, _args) -> String.(name <> exclude))
    in
    let attribute = to_attribute { name; loc; generators } in
    (match attribute with
     | None -> attributes
     | Some attr -> attr :: attributes)
;;
