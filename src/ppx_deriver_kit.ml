open! Stdppx
open! Ppx_helpers.Base_lite
open Ppxlib
open Ast_builder.Default
open Ppxlib_jane.Ast_builder.Default
include Ppx_deriver_kit_intf
include Ppx_deriver_kit_intf.Definitions

let ghost = Ppx_helpers.ghoster

module Kit_config = struct
  type 'a t =
    { config : 'a
    ; unboxed : bool
    }

  let args ~support_unboxed (Deriving_args.T user_args) =
    let make unboxed =
      Deriving_args.both unboxed user_args
      |> Deriving_args.map ~f:(fun (unboxed, config) -> { config; unboxed })
      |> Deriving_args.pack
    in
    if support_unboxed
    then make (Deriving_args.flag "unboxed")
    else make (Deriving_args.return false)
  ;;
end

module Register (Impl : S) = struct
  let handle_result ~drop_attributes ~on_error = function
    | Ok x -> x
    | Error { loc; txt = message } ->
      drop_attributes ();
      Location.error_extensionf
        ~loc:{ loc with loc_ghost = true }
        "%s: %s"
        Impl.ppx_name
        message
      |> on_error ~loc
  ;;

  let unsupported ~loc feature =
    Error { loc; txt = Printf.sprintf "%s not supported" feature }
  ;;

  (* Declare the [<name>.custom] attribute *)
  let custom_attribute =
    Attribute.declare
      (Impl.deriver_name ^ ".custom")
      Core_type
      Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
      Fn.id
  ;;

  let rec of_core_type ~config ~is_extension core_type : Impl.t =
    let core_type = ghost#core_type core_type in
    let loc = core_type.ptyp_loc in
    let result =
      match Attribute.get custom_attribute core_type with
      | Some expr ->
        Impl.on_custom_attribute
          ~loc
          ~config
          ~core_type:(Ppx_helpers.strip_attributes#core_type core_type)
          expr
      | None ->
        let core_type_desc =
          Ppxlib_jane.Shim.Core_type_desc.of_parsetree core_type.ptyp_desc
        in
        (* Shadow original [core_type] with the stripped version to hand to callbacks *)
        let core_type = Ppx_helpers.strip_attributes#core_type core_type in
        (match core_type_desc with
         | Ptyp_var (var, _) when not is_extension ->
           Impl.on_type_variable ~loc ~config ~core_type var
         | Ptyp_constr (id, params) ->
           List.map params ~f:(of_core_type ~config ~is_extension)
           |> Impl.on_named_type ~loc ~config ~core_type id
         | Ptyp_tuple tuple ->
           List.map tuple ~f:(fun (label, type_) ->
             label, of_core_type ~config ~is_extension type_)
           |> Impl.on_tuple ~loc ~config ~core_type ~unboxed:false
         | Ptyp_unboxed_tuple tuple ->
           List.map tuple ~f:(fun (label, type_) ->
             label, of_core_type ~config ~is_extension type_)
           |> Impl.on_tuple ~loc ~config ~core_type ~unboxed:true
         | Ptyp_variant (clauses, closed, _) ->
           let clauses = List.map clauses ~f:(of_polyvar_clause ~config ~is_extension) in
           (match Result.all clauses with
            | Error (error_loc, error) -> unsupported ~loc:error_loc error
            | Ok clauses ->
              let is_closed =
                match closed with
                | Closed -> true
                | Open -> false
              in
              Impl.on_polymorphic_variant ~loc ~config ~core_type ~is_closed clauses)
         | desc ->
           unsupported ~loc (Ppxlib_jane.Language_feature_name.of_core_type_desc desc))
    in
    handle_result result ~on_error:Impl.on_error ~drop_attributes:(fun () ->
      Attribute.explicitly_drop#core_type core_type)

  and of_polyvar_clause ~config ~is_extension clause
    : (Impl.t Polymorphic_variant_clause.t, location * string) result
    =
    match clause.prf_desc with
    | Rtag (label, true, []) -> Ok (Tag { label; param = None })
    | Rtag (label, false, [ payload ]) ->
      let payload = Some (of_core_type ~config ~is_extension payload) in
      Ok (Tag { label; param = payload })
    | Rinherit core_type ->
      (match Ppxlib_jane.Shim.Core_type_desc.of_parsetree core_type.ptyp_desc with
       | Ptyp_constr (id, _) ->
         Ok (Inherit { id; derived = of_core_type ~config ~is_extension core_type })
       | _ ->
         Error
           ( clause.prf_loc
           , "inherited polymorphic variant from type that is not just a name" ))
    | Rtag (_, false, []) ->
      Error
        ( clause.prf_loc
        , "polymorphic variant with empty intersection type (accepts neither with nor \
           without argument)" )
    | Rtag (_, false, _ :: _ :: _) ->
      Error
        ( clause.prf_loc
        , "polymorphic variant with non-trivial intersection type (multiple argument \
           types accepted)" )
    | Rtag (_, true, _ :: _) ->
      Error
        ( clause.prf_loc
        , "polymorphic variant with nonsensical intersection type (accepts both with and \
           without argument)" )
  ;;

  let of_tuple_arguments ~config tuple =
    List.map tuple ~f:(fun arg ->
      of_core_type
        ~config
        ~is_extension:false
        (Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type arg))
  ;;

  let of_record_fields ~config fields =
    List.map fields ~f:(fun ld ->
      ld.pld_name, of_core_type ~config ~is_extension:false ld.pld_type)
  ;;

  let of_variant_clause ~config clause : Impl.t Variant_clause.t =
    let (args : Impl.t Variant_clause.args) =
      match clause.pcd_args with
      | Pcstr_tuple tuple -> Tuple (of_tuple_arguments ~config tuple)
      | Pcstr_record fields -> Record (of_record_fields ~config fields)
    in
    { loc = clause.pcd_loc; name = clause.pcd_name; args; gadt_result = clause.pcd_res }
  ;;

  let of_type_declaration ~config td =
    let loc = ghost#location td.ptype_loc in
    let result =
      match Ppxlib_jane.Shim.Type_kind.of_parsetree td.ptype_kind with
      | Ptype_abstract ->
        (match td.ptype_manifest with
         | None -> Impl.on_abstract ~loc ~config ~td
         | Some manifest -> Ok (of_core_type ~config ~is_extension:false manifest))
      | Ptype_record fields ->
        Impl.on_record ~loc ~config ~td ~unboxed:false (of_record_fields ~config fields)
      | Ptype_record_unboxed_product fields ->
        Impl.on_record ~loc ~config ~td ~unboxed:true (of_record_fields ~config fields)
      | Ptype_variant clauses ->
        Impl.on_variant ~loc ~config ~td (List.map clauses ~f:(of_variant_clause ~config))
      | Ptype_open -> unsupported ~loc "open type"
    in
    handle_result result ~on_error:Impl.on_error ~drop_attributes:(fun () ->
      Attribute.explicitly_drop#type_declaration td)
  ;;

  module Signature = struct
    let to_signature_items ~config tds =
      List.map tds ~f:(fun td ->
        let td = name_type_params_in_td td in
        let loc = ghost#location td.ptype_loc in
        value_description
          ~loc
          ~name:(Loc.map td.ptype_name ~f:Impl.derived_value_name)
          ~type_:
            (Ppx_helpers.combinator_type_of_type_declaration td ~f:Impl.derived_type
             |> Ppx_helpers.Polytype.to_core_type
                  ~universally_quantify_only_if_jkind_annotation:true)
          ~modalities:(Impl.derived_modalities ~loc ~config)
          ~prim:[]
        |> psig_value ~loc)
    ;;

    let expand
      ~ctxt
      (_rec_flag, tds)
      ({ config; unboxed } : Impl.Signature_config.t Kit_config.t)
      =
      let loc = Expansion_context.Deriver.derived_item_loc ctxt in
      let tds = Ppx_helpers.with_implicit_unboxed_types ~loc ~unboxed tds in
      to_signature_items ~config tds
    ;;
  end

  module Structure = struct
    let really_recursive rec_flag tds =
      (object
         inherit type_is_recursive rec_flag tds as super

         method! core_type core_type =
           match Attribute.get custom_attribute ~mark_as_seen:false core_type with
           | Some _ -> ()
           | None -> super#core_type core_type
      end)
        #go
        ()
    ;;

    let to_structure_items ~loc ~config rec_flag tds =
      let results = List.map tds ~f:(fun td -> td, of_type_declaration ~config td) in
      let rec_flag = really_recursive rec_flag tds in
      Impl.to_structure_items ~loc ~config rec_flag results
    ;;

    let expand
      ~ctxt
      (rec_flag, tds)
      ({ config; unboxed } : Impl.Structure_config.t Kit_config.t)
      =
      let loc = Expansion_context.Deriver.derived_item_loc ctxt in
      let tds =
        List.map tds ~f:(fun td -> ghost#type_declaration (name_type_params_in_td td))
      in
      let tds = Ppx_helpers.with_implicit_unboxed_types ~loc ~unboxed tds in
      to_structure_items ~loc ~config rec_flag tds
      |> handle_result
           ~on_error:(fun ~loc ext -> [ pstr_extension ~loc ext [] ])
           ~drop_attributes:(fun () ->
             List.iter tds ~f:Attribute.explicitly_drop#type_declaration)
    ;;

    let to_extension expand ~config ~loc ~path:_ core_type =
      let loc = ghost#location loc in
      let core_type = ghost#core_type core_type in
      of_core_type ~config ~is_extension:true core_type
      |> expand ~loc ~core_type:(Ppx_helpers.strip_attributes#core_type core_type)
      |> handle_result ~on_error:pexp_extension ~drop_attributes:(fun () ->
        Attribute.explicitly_drop#core_type core_type)
    ;;
  end

  let sig_type_decl =
    Kit_config.args Impl.Signature_config.args ~support_unboxed:Impl.support_unboxed
    |> Deriving_args.to_generator_v2 ~f:Signature.expand
  ;;

  let str_type_decl =
    Kit_config.args Impl.Structure_config.args ~support_unboxed:Impl.support_unboxed
    |> Deriving_args.to_generator_v2 ~f:Structure.expand
  ;;

  let register_extension_separately expand =
    let ext =
      Extension.declare
        Impl.extension_name
        Extension.Context.expression
        Ast_pattern.(ptyp __)
        expand
    in
    Driver.V2.register_transformation
      (Printf.sprintf "%s.%s" Impl.ppx_name Impl.extension_name)
      ~rules:[ Context_free.Rule.extension ext ]
  ;;

  let deriver =
    let expand =
      Structure.to_extension Impl.expand_extension ~config:Impl.extension_config
    in
    let extension =
      if String.equal Impl.extension_name Impl.deriver_name
      then Some expand
      else (
        register_extension_separately expand;
        None)
    in
    Deriving.add Impl.deriver_name ~sig_type_decl ~str_type_decl ?extension
  ;;
end

let register (module Impl : S) =
  let module M = Register (Impl) in
  M.deriver
;;
