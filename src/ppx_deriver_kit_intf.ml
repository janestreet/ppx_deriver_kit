open! Stdppx
open Ppxlib
module Deriving_args = Ppx_helpers.Deriving_args

module Definitions = struct
  (** One clause of a polymorphic variant type, where ['a] is the deriver's [t] type. *)
  module Polymorphic_variant_clause = struct
    type 'a t =
      | Tag of
          { label : string loc
          ; param : 'a option
          }
      | Inherit of
          { id : longident_loc
          ; derived : 'a
          }
  end

  (** One clause of a variant type, where ['a] is the deriver's [t] type. *)
  module Variant_clause = struct
    type 'a args =
      | Tuple of 'a list
      | Record of (string loc * 'a) list

    type 'a t =
      { loc : location
      ; name : string loc
      ; args : 'a args
      ; gadt_result : core_type option
      (** e.g. the [int t] in [type t = T : string -> int t]. *)
      }
  end

  module type Config = sig
    (** Represents configuration passed in via deriver arguments. For example:

        {[
          type t =
            { mutable x : int
            ; mutable y : int
            }
          [@@deriving fields ~getters ~setters]
        ]}

        Here, [t] would express that [~getters] and [~setters] were passed. *)
    type t

    (** Syntax type being derived. *)
    type ast

    (** Specifies args used to configure this deriver. *)
    val args : (t, ast) Deriving_args.packed
  end

  module type S = sig
    module Signature_config : Config with type ast := signature_item list
    module Structure_config : Config with type ast := structure_item list

    (** The deriver's representation of what it derives from a type. The final [t] is used
        to generate code. *)
    type t

    (** Name of the deriver which is used for registration, e.g. [enumerate] in
        [[@@deriving enumerate]]. *)
    val deriver_name : string

    (** Name of the ppx, e.g. [ppx_enumerate]. Used for error reporting. *)
    val ppx_name : string

    (** Name of the extension being registered, e.g. [all] for [[%all]] from
        [ppx_enumerate]. *)
    val extension_name : string

    (** Names the generated value, e.g. [t -> "all"], [foo -> "all_of_foo"]. *)
    val derived_value_name : string -> string

    (** Builds the generated value's type from the input type, e.g.
        [fun ~loc ty -> [%type: [%t ty] list]] for [enumerate]. *)
    val derived_type : loc:location -> core_type -> core_type

    (** [derived_modalities] converts the configuration for a signature into the
        modalities for a value description. *)
    val derived_modalities
      :  loc:location
      -> config:Signature_config.t
      -> Ppxlib_jane.modalities

    (** When [support_unboxed] is [true], the deriver accepts the [~unboxed] flag. When
        the flag is supplied by the user:
        - On an unboxed record type [t], the deriver will derive a value for [t] and its
          unboxed record representation, [t#].
        - On an alias such as [type t = float], the deriver will also derive a value for
          [type t# = float#].

        When [~unboxed] is supplied on other types, the deriver will emit an error as no
        implicit unboxed representation exists. *)
    val support_unboxed : bool

    (** Each [on_*] callback helps construct the deriver's [t] for one syntax. Every
        callback receives the (ghosted) location, the config, and the [core_type] or
        [type_declaration]. In the [core_type] case, all attributes have been stripped
        already. It also receives the [t] for each subterm.

        Other than [on_error], each callback should return a [(t, string loc) result]. If
        there is an error, the kit will mark all attributes as seen accordingly, and emit
        the error node with message [<ppx_name>: <string>] using [on_error]. *)

    (** [on_error ~loc extension] determines what to do when the deriver kit encounters an
        unsupported case. The [extension] is a pre-constructed AST extension node
        reporting the error message. *)
    val on_error : loc:location -> extension -> t

    (** A core type carrying a [[@<deriver_name>.custom]] attribute. [on_custom_attribute]
        receives its payload and is used for that type in place of the other callbacks. *)
    val on_custom_attribute
      :  loc:location
      -> config:Structure_config.t
      -> core_type:core_type
      -> expression
      -> (t, string loc) result

    (** A type variable such as ['a], where the [string] is its name ["a"]. *)
    val on_type_variable
      :  loc:location
      -> config:Structure_config.t
      -> core_type:core_type
      -> string
      -> (t, string loc) result

    (** A type constructor, such as [int] or [(char, string) Foo.t]. *)
    val on_named_type
      :  loc:location
      -> config:Structure_config.t
      -> core_type:core_type
      -> longident_loc
      -> t list
      -> (t, string loc) result

    (** A tuple type, such as [int * string] or [#(i:int * s:string)]. *)
    val on_tuple
      :  loc:location
      -> config:Structure_config.t
      -> core_type:core_type
      -> unboxed:bool
      -> (string option * t) list
      -> (t, string loc) result

    (** A polymorphic variant type such as [[ `A | `B of int | c_or_d ]]. [is_closed] is
        [false] for open types, such as [[> `A ]], and [true] otherwise. *)
    val on_polymorphic_variant
      :  loc:location
      -> config:Structure_config.t
      -> core_type:core_type
      -> is_closed:bool
      -> t Polymorphic_variant_clause.t list
      -> (t, string loc) result

    (** A record type, such as [type t = { x : int; y : bool }] or
        [type t = #{ x : float#; y : float# }]. *)
    val on_record
      :  loc:location
      -> config:Structure_config.t
      -> td:type_declaration
      -> unboxed:bool
      -> (string loc * t) list
      -> (t, string loc) result

    (** A variant type, such as [type t = A | B of int | C of { x : float; y : float }]. *)
    val on_variant
      :  loc:location
      -> config:Structure_config.t
      -> td:type_declaration
      -> t Variant_clause.t list
      -> (t, string loc) result

    (** An abstract type with no exposed representation, such as [type 'a t]. *)
    val on_abstract
      :  loc:location
      -> config:Structure_config.t
      -> td:type_declaration
      -> (t, string loc) result

    (** Builds structure items, given type declarations, a pre-processed [rec_flag] for if
        they are truly recursive, and their corresponding [t]. *)
    val to_structure_items
      :  loc:location
      -> config:Structure_config.t
      -> rec_flag
      -> (type_declaration * t) list
      -> (structure_item list, string loc) result

    (** Configuration for extensions, since they don't have deriver arguments. *)
    val extension_config : Structure_config.t

    (** [expand_extension ~loc core_type t] is the expression produced for a payload
        [core_type] with the deriver kit's result [t]. *)
    val expand_extension
      :  loc:location
      -> core_type:core_type
      -> t
      -> (expression, string loc) result
  end
end

module type Ppx_deriver_kit = sig
  include module type of struct
    include Definitions
  end

  (** [register (module Impl)] registers and returns a ppx deriver based on the
      implementation [Impl]. It additionally registers the extension specified by [Impl]
      as well as a custom attribute on core types, whose payload replaces the derived
      code. *)
  val register : (module S) -> Deriving.t
end
