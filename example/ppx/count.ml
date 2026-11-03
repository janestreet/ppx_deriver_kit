open! Base
open Ppxlib
open Ppxlib.Ast_builder.Default
open Ppxlib_jane.Ast_builder.Default
module Variant_clause = Ppx_deriver_kit.Variant_clause
module Polymorphic_variant_clause = Ppx_deriver_kit.Polymorphic_variant_clause

let count_var_name var = "_count_'" ^ var
let gen_symbol () = gen_symbol ~prefix:"ppx_count" ()

let sum_int_exprs ~loc = function
  | [] -> [%expr 0]
  | hd :: tl ->
    List.fold tl ~init:hd ~f:(fun acc e -> [%expr Stdlib.Int.add [%e acc] [%e e]])
;;

(* A boxed value allocates a heap block, which counts as [1]; a flattened (unboxed) form
   allocates no block and adds nothing. *)
let block_overhead ~loc ~unboxed contents =
  if unboxed then contents else [%expr 1] :: contents
;;

module Count_info = struct
  (* $MDX part-begin=count-info *)
  type t = pattern * expression

  let to_fn ~loc (pat, body) = [%expr fun [%p pat] -> [%e body]]
  (* $MDX part-end *)

  (* $MDX part-begin=args-info *)
  module Signature_config = struct
    type t = unit

    let args = Ppx_helpers.Deriving_args.no_args
  end

  module Structure_config = struct
    type t = unit

    let args = Ppx_helpers.Deriving_args.no_args
  end
  (* $MDX part-end *)

  (* $MDX part-begin=support-unboxed *)
  let support_unboxed = true
  (* $MDX part-end *)

  (* $MDX part-begin=count-names *)
  let deriver_name = "count"
  let ppx_name = "ppx_count"
  let extension_name = deriver_name

  let derived_value_name type_name =
    match Ppx_helpers.mangle_unboxed type_name with
    | "t" -> "count"
    | "t_u" -> "count_u"
    | type_name -> "count_" ^ type_name
  ;;

  let derived_type ~loc core_type = [%type: [%t core_type] -> Stdlib.Int.t]

  (* $MDX part-end *)

  let derived_modalities ~loc:_ ~config:() = []
  let on_error ~loc extension = ppat_any ~loc, pexp_extension ~loc extension
  let on_custom_attribute ~loc ~config:() ~core_type:_ expr = Ok (ppat_any ~loc, expr)

  let on_type_variable ~loc ~config:() ~core_type:_ var =
    let name = gen_symbol () in
    Ok (pvar ~loc name, [%expr [%e evar ~loc (count_var_name var)] [%e evar ~loc name]])
  ;;

  (* $MDX part-begin=named-type *)
  let on_named_type ~loc ~config:() ~core_type id params =
    let name = gen_symbol () in
    let expr =
      [%expr
        ([%e
           List.map params ~f:(fun param -> to_fn ~loc param)
           |> type_constr_conv ~loc id ~f:derived_value_name]
         : [%t derived_type ~loc core_type])]
    in
    Ok (pvar ~loc name, [%expr [%e expr] [%e evar ~loc name]])
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=tuple *)
  let on_tuple ~loc ~config:() ~core_type:_ ~unboxed tuple =
    let labeled_pats, counts =
      List.map tuple ~f:(fun (label, (pat, count)) -> (label, pat), count) |> List.unzip
    in
    let pat =
      if unboxed
      then ppat_unboxed_tuple ~loc labeled_pats Closed
      else ppat_tuple ~loc labeled_pats Closed
    in
    Ok (pat, sum_int_exprs ~loc (block_overhead ~loc ~unboxed counts))
  ;;

  (* $MDX part-end *)

  let count_constructor ~loc:_ ({ loc; name; args; _ } : t Variant_clause.t) =
    let arg_pat, contents =
      match args with
      | Tuple tuple ->
        let pats, counts = List.unzip tuple in
        ppat_tuple_opt ~loc pats, counts
      | Record fields ->
        let labeled_pats, counts =
          List.map fields ~f:(fun (label, (pat, count)) ->
            (Located.map_lident label, pat), count)
          |> List.unzip
        in
        Some (ppat_record ~loc labeled_pats Closed), counts
    in
    let lhs = ppat_construct ~loc (Located.map_lident name) arg_pat in
    case
      ~lhs
      ~guard:None
      ~rhs:(sum_int_exprs ~loc (block_overhead ~loc ~unboxed:false contents))
  ;;

  let count_polyvar_clause ~loc clause =
    match (clause : _ Polymorphic_variant_clause.t) with
    | Tag { label; param = None } ->
      case ~lhs:(ppat_variant ~loc label.txt None) ~guard:None ~rhs:[%expr 1]
    | Tag { label; param = Some (pat, count) } ->
      case
        ~lhs:(ppat_variant ~loc label.txt (Some pat))
        ~guard:None
        ~rhs:(sum_int_exprs ~loc (block_overhead ~loc ~unboxed:false [ count ]))
    | Inherit { id; derived } ->
      let name = gen_symbol () in
      case
        ~lhs:(ppat_alias ~loc (ppat_type ~loc id) (Located.mk ~loc name))
        ~guard:None
        ~rhs:[%expr [%e to_fn ~loc derived] [%e evar ~loc name]]
  ;;

  let on_polymorphic_variant ~loc ~config:() ~core_type:_ ~is_closed clauses =
    match is_closed with
    | false -> Error (Loc.make ~loc "open polymorphic variant not supported")
    | true ->
      let name = gen_symbol () in
      Ok
        ( pvar ~loc name
        , pexp_match
            ~loc
            (evar ~loc name)
            (List.map clauses ~f:(count_polyvar_clause ~loc)) )
  ;;

  let on_record ~loc ~config:() ~td:_ ~unboxed fields =
    let labeled_pats, counts =
      List.map fields ~f:(fun (label, (pat, count)) ->
        (Located.map_lident label, pat), count)
      |> List.unzip
    in
    let pat =
      if unboxed
      then ppat_record_unboxed_product ~loc labeled_pats Closed
      else ppat_record ~loc labeled_pats Closed
    in
    Ok (pat, sum_int_exprs ~loc (block_overhead ~loc ~unboxed counts))
  ;;

  let on_variant ~loc ~config:() ~td clauses =
    let name = gen_symbol () in
    let ast_cases =
      match clauses with
      | [] ->
        let ty = core_type_of_type_declaration td in
        [ case ~lhs:[%pat? (_ : [%t ty])] ~guard:None ~rhs:(pexp_unreachable ~loc) ]
      | _ :: _ -> List.map clauses ~f:(count_constructor ~loc)
    in
    Ok (pvar ~loc name, pexp_match ~loc (evar ~loc name) ast_cases)
  ;;

  let on_abstract ~loc ~config:() ~td:_ =
    Error (Loc.make ~loc "abstract type not supported")
  ;;

  (* $MDX part-begin=struct-registration *)
  let to_structure_items ~loc ~config:() rec_flag tds_and_results =
    List.map tds_and_results ~f:(fun (td, result) ->
      let loc = { td.ptype_loc with loc_ghost = true } in
      let pat = pvar ~loc (derived_value_name td.ptype_name.txt) in
      let expr =
        eabstract
          ~loc
          (Ppx_helpers.type_param_conv_pats
             ~param_name:count_var_name
             ~param_type:derived_type
             td)
          (to_fn ~loc result)
      in
      let ty =
        combinator_type_of_type_declaration td ~f:derived_type
        |> Ppxlib_jane.Ast_builder.Default.ptyp_poly
             ~loc
             (List.map td.ptype_params ~f:Ppxlib_jane.get_type_param_name_and_jkind)
      in
      value_binding
        ~loc
        ~pat
        ~expr
        ~constraint_:(Some (Pvc_constraint { locally_abstract_univars = []; typ = ty }))
        ~modes:[])
    |> pstr_value_list ~loc rec_flag
    |> Result.return
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=extension *)
  let extension_config = ()

  let expand_extension ~loc ~core_type counted =
    Ok (pexp_constraint ~loc (to_fn ~loc counted) (Some (derived_type ~loc core_type)) [])
  ;;
  (* $MDX part-end *)
end

(* $MDX part-begin=registration *)
let () = Ppx_deriver_kit.register (module Count_info) |> Deriving.ignore
(* $MDX part-end *)
