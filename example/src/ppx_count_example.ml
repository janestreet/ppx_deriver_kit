open! Base
open Ppx_deriver_kit_example_runtime.Export

module type Abstract = sig
  type t [@@deriving_inline count]

  include sig
    [@@@ocaml.warning "-32"]

    val count : t -> Stdlib.Int.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Simple = struct
  type t = char [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__001_ -> (count_char : char -> Stdlib.Int.t) ppx_count__001_
  ;;

  let _ = count

  [@@@end]
end

module Named = struct
  type foo = float [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : foo) -> ()

  let count_foo : foo -> Stdlib.Int.t =
    fun ppx_count__002_ -> (count_float : float -> Stdlib.Int.t) ppx_count__002_
  ;;

  let _ = count_foo

  [@@@end]
end

module Unary = struct
  type 'a t = 'a array [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : 'a t) -> ()

  let count : 'a. ('a -> Stdlib.Int.t) -> 'a t -> Stdlib.Int.t =
    fun (_count_'a : 'a -> Stdlib.Int.t) ppx_count__006_ ->
    (count_array (fun ppx_count__005_ -> _count_'a ppx_count__005_)
     : 'a array -> Stdlib.Int.t)
      ppx_count__006_
  ;;

  let _ = count

  [@@@end]
end

module Binary = struct
  type ('a, 'b) t =
    { left : 'a
    ; right : 'b
    }
  [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : ('a, 'b) t) -> ()

  let count
    : 'a 'b. ('a -> Stdlib.Int.t) -> ('b -> Stdlib.Int.t) -> ('a, 'b) t -> Stdlib.Int.t
    =
    fun (_count_'a : 'a -> Stdlib.Int.t)
      (_count_'b : 'b -> Stdlib.Int.t)
      { left = ppx_count__014_; right = ppx_count__015_ } ->
    Stdlib.Int.add
      (Stdlib.Int.add 1 (_count_'a ppx_count__014_))
      (_count_'b ppx_count__015_)
  ;;

  let _ = count

  [@@@end]
end

module Alias = struct
  type t = int list [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__018_ ->
    (count_list (fun ppx_count__017_ -> (count_int : int -> Stdlib.Int.t) ppx_count__017_)
     : int list -> Stdlib.Int.t)
      ppx_count__018_
  ;;

  let _ = count

  [@@@end]
end

module Record = struct
  type t =
    { x : int
    ; y : string
    }
  [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun { x = ppx_count__024_; y = ppx_count__025_ } ->
    Stdlib.Int.add
      (Stdlib.Int.add 1 ((count_int : int -> Stdlib.Int.t) ppx_count__024_))
      ((count_string : string -> Stdlib.Int.t) ppx_count__025_)
  ;;

  let _ = count

  [@@@end]
end

module Variant = struct
  type t =
    | A
    | B of int
    | C of string * bool
  [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__035_ ->
    match ppx_count__035_ with
    | A -> 1
    | B ppx_count__032_ ->
      Stdlib.Int.add 1 ((count_int : int -> Stdlib.Int.t) ppx_count__032_)
    | C (ppx_count__033_, ppx_count__034_) ->
      Stdlib.Int.add
        (Stdlib.Int.add 1 ((count_string : string -> Stdlib.Int.t) ppx_count__033_))
        ((count_bool : bool -> Stdlib.Int.t) ppx_count__034_)
  ;;

  let _ = count

  [@@@end]
end

module Multiple = struct
  type t = int64
  and foo = bool
  and 'a bar = 'a ref [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()
  let _ = fun (_ : foo) -> ()
  let _ = fun (_ : 'a bar) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__038_ -> (count_int64 : int64 -> Stdlib.Int.t) ppx_count__038_

  and count_foo : foo -> Stdlib.Int.t =
    fun ppx_count__039_ -> (count_bool : bool -> Stdlib.Int.t) ppx_count__039_

  and count_bar : 'a. ('a -> Stdlib.Int.t) -> 'a bar -> Stdlib.Int.t =
    fun (_count_'a : 'a -> Stdlib.Int.t) ppx_count__041_ ->
    (count_ref (fun ppx_count__040_ -> _count_'a ppx_count__040_)
     : 'a ref -> Stdlib.Int.t)
      ppx_count__041_
  ;;

  let _ = count
  and _ = count_foo
  and _ = count_bar

  [@@@end]
end

module Empty_variant = struct
  type t = | [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__042_ ->
    match ppx_count__042_ with
    | (_ : t) -> .
  ;;

  let _ = count

  [@@@end]
end

module Primitive_alias = struct
  type t = int [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__043_ -> (count_int : int -> Stdlib.Int.t) ppx_count__043_
  ;;

  let _ = count

  [@@@end]
end

module Type_application_alias = struct
  type t = int list [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__046_ ->
    (count_list (fun ppx_count__045_ -> (count_int : int -> Stdlib.Int.t) ppx_count__045_)
     : int list -> Stdlib.Int.t)
      ppx_count__046_
  ;;

  let _ = count

  [@@@end]
end

module Tuple_alias = struct
  type t = int * string * bool [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun (ppx_count__053_, ppx_count__054_, ppx_count__055_) ->
    Stdlib.Int.add
      (Stdlib.Int.add
         (Stdlib.Int.add 1 ((count_int : int -> Stdlib.Int.t) ppx_count__053_))
         ((count_string : string -> Stdlib.Int.t) ppx_count__054_))
      ((count_bool : bool -> Stdlib.Int.t) ppx_count__055_)
  ;;

  let _ = count

  [@@@end]
end

module Record_structure = struct
  type t =
    { x : int
    ; y : string list
    }
  [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun { x = ppx_count__061_; y = ppx_count__063_ } ->
    Stdlib.Int.add
      (Stdlib.Int.add 1 ((count_int : int -> Stdlib.Int.t) ppx_count__061_))
      ((count_list (fun ppx_count__062_ ->
          (count_string : string -> Stdlib.Int.t) ppx_count__062_)
        : string list -> Stdlib.Int.t)
         ppx_count__063_)
  ;;

  let _ = count

  [@@@end]
end

module Variant_structure = struct
  type t =
    | A
    | B of int
    | C of string * bool
  [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__073_ ->
    match ppx_count__073_ with
    | A -> 1
    | B ppx_count__070_ ->
      Stdlib.Int.add 1 ((count_int : int -> Stdlib.Int.t) ppx_count__070_)
    | C (ppx_count__071_, ppx_count__072_) ->
      Stdlib.Int.add
        (Stdlib.Int.add 1 ((count_string : string -> Stdlib.Int.t) ppx_count__071_))
        ((count_bool : bool -> Stdlib.Int.t) ppx_count__072_)
  ;;

  let _ = count

  [@@@end]
end

module Inline_record_variant = struct
  type t =
    | A of
        { x : int
        ; y : string
        }
  [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__081_ ->
    match ppx_count__081_ with
    | A { x = ppx_count__079_; y = ppx_count__080_ } ->
      Stdlib.Int.add
        (Stdlib.Int.add 1 ((count_int : int -> Stdlib.Int.t) ppx_count__079_))
        ((count_string : string -> Stdlib.Int.t) ppx_count__080_)
  ;;

  let _ = count

  [@@@end]
end

module Polymorphic_variant_alias = struct
  type t =
    [ `A
    | `B of int
    | `C of string * bool
    ]
  [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__091_ ->
    match ppx_count__091_ with
    | `A -> 1
    | `B ppx_count__088_ ->
      Stdlib.Int.add 1 ((count_int : int -> Stdlib.Int.t) ppx_count__088_)
    | `C (ppx_count__089_, ppx_count__090_) ->
      Stdlib.Int.add
        1
        (Stdlib.Int.add
           (Stdlib.Int.add 1 ((count_string : string -> Stdlib.Int.t) ppx_count__089_))
           ((count_bool : bool -> Stdlib.Int.t) ppx_count__090_))
  ;;

  let _ = count

  [@@@end]
end

module Type_variable_alias = struct
  type 'a t = 'a option [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : 'a t) -> ()

  let count : 'a. ('a -> Stdlib.Int.t) -> 'a t -> Stdlib.Int.t =
    fun (_count_'a : 'a -> Stdlib.Int.t) ppx_count__095_ ->
    (count_option (fun ppx_count__094_ -> _count_'a ppx_count__094_)
     : 'a option -> Stdlib.Int.t)
      ppx_count__095_
  ;;

  let _ = count

  [@@@end]
end

module Multiple_structure = struct
  type t = int
  and foo = string list
  and 'a bar = 'a option [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()
  let _ = fun (_ : foo) -> ()
  let _ = fun (_ : 'a bar) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__099_ -> (count_int : int -> Stdlib.Int.t) ppx_count__099_

  and count_foo : foo -> Stdlib.Int.t =
    fun ppx_count__101_ ->
    (count_list (fun ppx_count__100_ ->
       (count_string : string -> Stdlib.Int.t) ppx_count__100_)
     : string list -> Stdlib.Int.t)
      ppx_count__101_

  and count_bar : 'a. ('a -> Stdlib.Int.t) -> 'a bar -> Stdlib.Int.t =
    fun (_count_'a : 'a -> Stdlib.Int.t) ppx_count__103_ ->
    (count_option (fun ppx_count__102_ -> _count_'a ppx_count__102_)
     : 'a option -> Stdlib.Int.t)
      ppx_count__103_
  ;;

  let _ = count
  and _ = count_foo
  and _ = count_bar

  [@@@end]
end

module Recursive_structure = struct
  type t = A of t list [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let rec count : t -> Stdlib.Int.t =
    fun ppx_count__108_ ->
    match ppx_count__108_ with
    | A ppx_count__107_ ->
      Stdlib.Int.add
        1
        ((count_list (fun ppx_count__106_ -> (count : t -> Stdlib.Int.t) ppx_count__106_)
          : t list -> Stdlib.Int.t)
           ppx_count__107_)
  ;;

  let _ = count

  [@@@end]
end

module Mutually_recursive_structure = struct
  type t = A of u
  and u = B of t list [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()
  let _ = fun (_ : u) -> ()

  let rec count : t -> Stdlib.Int.t =
    fun ppx_count__114_ ->
    match ppx_count__114_ with
    | A ppx_count__113_ ->
      Stdlib.Int.add 1 ((count_u : u -> Stdlib.Int.t) ppx_count__113_)

  and count_u : u -> Stdlib.Int.t =
    fun ppx_count__117_ ->
    match ppx_count__117_ with
    | B ppx_count__116_ ->
      Stdlib.Int.add
        1
        ((count_list (fun ppx_count__115_ -> (count : t -> Stdlib.Int.t) ppx_count__115_)
          : t list -> Stdlib.Int.t)
           ppx_count__116_)
  ;;

  let _ = count
  and _ = count_u

  [@@@end]
end

module Mutually_recursive_parameterized_structure = struct
  type 'a t =
    | A of 'a u
    | Leaf

  and 'a u = B of 'a t * 'a [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : 'a t) -> ()
  let _ = fun (_ : 'a u) -> ()

  let rec count : 'a. ('a -> Stdlib.Int.t) -> 'a t -> Stdlib.Int.t =
    fun (_count_'a : 'a -> Stdlib.Int.t) ppx_count__130_ ->
    match ppx_count__130_ with
    | A ppx_count__129_ ->
      Stdlib.Int.add
        1
        ((count_u (fun ppx_count__128_ -> _count_'a ppx_count__128_)
          : 'a u -> Stdlib.Int.t)
           ppx_count__129_)
    | Leaf -> 1

  and count_u : 'a. ('a -> Stdlib.Int.t) -> 'a u -> Stdlib.Int.t =
    fun (_count_'a : 'a -> Stdlib.Int.t) ppx_count__134_ ->
    match ppx_count__134_ with
    | B (ppx_count__132_, ppx_count__133_) ->
      Stdlib.Int.add
        (Stdlib.Int.add
           1
           ((count (fun ppx_count__131_ -> _count_'a ppx_count__131_)
             : 'a t -> Stdlib.Int.t)
              ppx_count__132_))
        (_count_'a ppx_count__133_)
  ;;

  let _ = count
  and _ = count_u

  [@@@end]
end

module Record_with_nested_tuple = struct
  type t =
    { x : int * string
    ; y : bool
    }
  [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun { x = ppx_count__144_, ppx_count__145_; y = ppx_count__146_ } ->
    Stdlib.Int.add
      (Stdlib.Int.add
         1
         (Stdlib.Int.add
            (Stdlib.Int.add 1 ((count_int : int -> Stdlib.Int.t) ppx_count__144_))
            ((count_string : string -> Stdlib.Int.t) ppx_count__145_)))
      ((count_bool : bool -> Stdlib.Int.t) ppx_count__146_)
  ;;

  let _ = count

  [@@@end]
end

module Tuple = struct
  type t = int * string [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun (ppx_count__151_, ppx_count__152_) ->
    Stdlib.Int.add
      (Stdlib.Int.add 1 ((count_int : int -> Stdlib.Int.t) ppx_count__151_))
      ((count_string : string -> Stdlib.Int.t) ppx_count__152_)
  ;;

  let _ = count

  [@@@end]
end

module Mixed_variant = struct
  type t =
    | A of
        { x : int
        ; y : string
        }
    | B of int
    | C
  [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__163_ ->
    match ppx_count__163_ with
    | A { x = ppx_count__160_; y = ppx_count__161_ } ->
      Stdlib.Int.add
        (Stdlib.Int.add 1 ((count_int : int -> Stdlib.Int.t) ppx_count__160_))
        ((count_string : string -> Stdlib.Int.t) ppx_count__161_)
    | B ppx_count__162_ ->
      Stdlib.Int.add 1 ((count_int : int -> Stdlib.Int.t) ppx_count__162_)
    | C -> 1
  ;;

  let _ = count

  [@@@end]
end

module Polymorphic_variant_inherit = struct
  type base =
    [ `A
    | `B of int
    ]
  [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : base) -> ()

  let count_base : base -> Stdlib.Int.t =
    fun ppx_count__166_ ->
    match ppx_count__166_ with
    | `A -> 1
    | `B ppx_count__165_ ->
      Stdlib.Int.add 1 ((count_int : int -> Stdlib.Int.t) ppx_count__165_)
  ;;

  let _ = count_base

  [@@@end]

  type t =
    [ base
    | `C of string
    ]
  [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__171_ ->
    match ppx_count__171_ with
    | #base as ppx_count__172_ ->
      (fun ppx_count__169_ -> (count_base : base -> Stdlib.Int.t) ppx_count__169_)
        ppx_count__172_
    | `C ppx_count__170_ ->
      Stdlib.Int.add 1 ((count_string : string -> Stdlib.Int.t) ppx_count__170_)
  ;;

  let _ = count

  [@@@end]
end

module GADT = struct
  type 'a t =
    | Empty : 'a t
    | Rec : 'a t -> 'a t
  [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : 'a t) -> ()

  let rec count : 'a. ('a -> Stdlib.Int.t) -> 'a t -> Stdlib.Int.t =
    fun (_count_'a : 'a -> Stdlib.Int.t) ppx_count__179_ ->
    match ppx_count__179_ with
    | Empty -> 1
    | Rec ppx_count__178_ ->
      Stdlib.Int.add
        1
        ((count (fun ppx_count__177_ -> _count_'a ppx_count__177_) : 'a t -> Stdlib.Int.t)
           ppx_count__178_)
  ;;

  let _ = count

  [@@@end]
end

module Module_path = struct
  type t = Variant.t [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__180_ -> (Variant.count : Variant.t -> Stdlib.Int.t) ppx_count__180_
  ;;

  let _ = count

  [@@@end]
end

module Module_path_application = struct
  type t = Variant.t list [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__183_ ->
    (count_list (fun ppx_count__182_ ->
       (Variant.count : Variant.t -> Stdlib.Int.t) ppx_count__182_)
     : Variant.t list -> Stdlib.Int.t)
      ppx_count__183_
  ;;

  let _ = count

  [@@@end]
end

module Module_path_non_t = struct
  type t = Named.foo [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__184_ -> (Named.count_foo : Named.foo -> Stdlib.Int.t) ppx_count__184_
  ;;

  let _ = count

  [@@@end]
end

module Custom_attribute = struct
  type t = (char[@count.custom 5]) [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()
  let count : t -> Stdlib.Int.t = fun _ -> 5
  let _ = count

  [@@@end]
end

module Custom_attribute_on_field = struct
  type t =
    { x : int
    ; y : (string[@count.custom 7])
    }
  [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun { x = ppx_count__190_; y = _ } ->
    Stdlib.Int.add
      (Stdlib.Int.add 1 ((count_int : int -> Stdlib.Int.t) ppx_count__190_))
      7
  ;;

  let _ = count

  [@@@end]
end

module Custom_attribute_on_parameter = struct
  type t = (int[@count.custom 2]) list [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__192_ ->
    (count_list (fun _ -> 2) : int list -> Stdlib.Int.t) ppx_count__192_
  ;;

  let _ = count

  [@@@end]
end

module Custom_attribute_hides_recursion = struct
  type t = { next : (t[@count.custom 0]) } [@@deriving_inline count]

  let _ = fun (_ : t) -> ()
  let count : t -> Stdlib.Int.t = fun { next = _ } -> Stdlib.Int.add 1 0
  let _ = count

  [@@@end]
end

module Implicit_unboxed_record = struct
  type t =
    { x : int
    ; y : string
    }
  [@@deriving_inline count ~unboxed] [@@deriving box, sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun { x = ppx_count__198_; y = ppx_count__199_ } ->
    Stdlib.Int.add
      (Stdlib.Int.add 1 ((count_int : int -> Stdlib.Int.t) ppx_count__198_))
      ((count_string : string -> Stdlib.Int.t) ppx_count__199_)

  and count_u : t -> Stdlib.Int.t =
    fun { x = ppx_count__200_; y = ppx_count__201_ } ->
    Stdlib.Int.add
      ((count_int : int -> Stdlib.Int.t) ppx_count__200_)
      ((count_string : string -> Stdlib.Int.t) ppx_count__201_)
  ;;

  let _ = count
  and _ = count_u

  [@@@end]
end

module Implicit_unboxed_alias = struct
  type t = float [@@deriving_inline count ~unboxed] [@@deriving sexp_of]

  let _ = fun (_ : t) -> ()

  let count : t -> Stdlib.Int.t =
    fun ppx_count__202_ -> (count_float : float -> Stdlib.Int.t) ppx_count__202_

  and count_u : t -> Stdlib.Int.t =
    fun ppx_count__203_ -> (count_float_u : float -> Stdlib.Int.t) ppx_count__203_
  ;;

  let _ = count
  and _ = count_u

  [@@@end]
end

module Jkind_annotation = struct
  type 'a t = { value : 'a } [@@deriving_inline count] [@@deriving sexp_of]

  let _ = fun (_ : 'a t) -> ()

  let count : 'a. ('a -> Stdlib.Int.t) -> 'a t -> Stdlib.Int.t =
    fun (_count_'a : 'a -> Stdlib.Int.t) { value = ppx_count__208_ } ->
    Stdlib.Int.add 1 (_count_'a ppx_count__208_)
  ;;

  let _ = count

  [@@@end]
end
