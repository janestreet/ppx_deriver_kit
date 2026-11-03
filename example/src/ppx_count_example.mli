open! Base

module type Abstract = sig
  type t [@@deriving_inline count]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Simple : sig
  type t = char [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Named : sig
  type foo = float [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count_foo : foo -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Unary : sig
  type 'a t = 'a array [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : ('a -> Stdlib.Int.t) -> 'a t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Binary : sig
  type ('a, 'b) t =
    { left : 'a
    ; right : 'b
    }
  [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : ('a -> Stdlib.Int.t) -> ('b -> Stdlib.Int.t) -> ('a, 'b) t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Alias : sig
  type t = int list [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Record : sig
  type t =
    { x : int
    ; y : string
    }
  [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Variant : sig
  type t =
    | A
    | B of int
    | C of string * bool
  [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Multiple : sig
  type t = int64
  and foo = bool
  and 'a bar = 'a ref [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
    val count_foo : foo -> Stdlib.Int.t
    val count_bar : ('a -> Stdlib.Int.t) -> 'a bar -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Empty_variant : sig
  type t = | [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Primitive_alias : sig
  type t = int [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Type_application_alias : sig
  type t = int list [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Tuple_alias : sig
  type t = int * string * bool [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Record_structure : sig
  type t =
    { x : int
    ; y : string list
    }
  [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Variant_structure : sig
  type t =
    | A
    | B of int
    | C of string * bool
  [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Inline_record_variant : sig
  type t =
    | A of
        { x : int
        ; y : string
        }
  [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Polymorphic_variant_alias : sig
  type t =
    [ `A
    | `B of int
    | `C of string * bool
    ]
  [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Type_variable_alias : sig
  type 'a t = 'a option [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : ('a -> Stdlib.Int.t) -> 'a t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Multiple_structure : sig
  type t = int
  and foo = string list
  and 'a bar = 'a option [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
    val count_foo : foo -> Stdlib.Int.t
    val count_bar : ('a -> Stdlib.Int.t) -> 'a bar -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Recursive_structure : sig
  type t = A of t list [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Mutually_recursive_structure : sig
  type t = A of u
  and u = B of t list [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
    val count_u : u -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Mutually_recursive_parameterized_structure : sig
  type 'a t =
    | A of 'a u
    | Leaf

  and 'a u = B of 'a t * 'a [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : ('a -> Stdlib.Int.t) -> 'a t -> Stdlib.Int.t
    val count_u : ('a -> Stdlib.Int.t) -> 'a u -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Record_with_nested_tuple : sig
  type t =
    { x : int * string
    ; y : bool
    }
  [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Tuple : sig
  type t = int * string [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Mixed_variant : sig
  type t =
    | A of
        { x : int
        ; y : string
        }
    | B of int
    | C
  [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Polymorphic_variant_inherit : sig
  type base =
    [ `A
    | `B of int
    ]
  [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count_base : base -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  type t =
    [ base
    | `C of string
    ]
  [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module GADT : sig
  type 'a t =
    | Empty : 'a t
    | Rec : 'a t -> 'a t
  [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : ('a -> Stdlib.Int.t) -> 'a t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Module_path : sig
  type t = Variant.t [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Module_path_application : sig
  type t = Variant.t list [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Module_path_non_t : sig
  type t = Named.foo [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Custom_attribute : sig
  type t = char [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Custom_attribute_on_field : sig
  type t =
    { x : int
    ; y : string
    }
  [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Custom_attribute_on_parameter : sig
  type t = int list [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Custom_attribute_hides_recursion : sig
  type t = { next : t } [@@deriving_inline count]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Implicit_unboxed_record : sig
  type t =
    { x : int
    ; y : string
    }
  [@@deriving_inline count ~unboxed] [@@deriving box, sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
    val count_u : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Implicit_unboxed_alias : sig
  type t = float [@@deriving_inline count ~unboxed] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
    val count_u : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Jkind_annotation : sig
  type 'a t = { value : 'a } [@@deriving_inline count] [@@deriving sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val count : 'a. ('a -> Stdlib.Int.t) -> 'a t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end
