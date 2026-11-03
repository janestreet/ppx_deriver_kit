open! Base
open Expect_test_helpers_base
open Ppx_deriver_kit_example_runtime.Export
module Ppx_count_example = Ppx_deriver_kit_example.Ppx_count_example

module type S = sig
  type t [@@deriving sexp_of]

  val count : t -> int
end

let test (type a) (module Type : S with type t = a) (examples : a list) =
  List.map examples ~f:(fun example ->
    [%sexp
      { count = (Type.count example : int); example = (Type.sexp_of_t example : Sexp.t) }])
  |> [%sexp_of: Sexp.t list]
  |> print_s ~sexp_style:Sexp_style.simple_pretty
;;

module Sample = struct
  let int = [ 0; 100 ]
  let string = [ ""; "abc"; "lorem ipsum dolor sit amet" ]
  let bool = Bool.all
  let char = [ 'A'; 'b' ]
  let float = [ 0.0; 1.5 ]
end

module _ : module type of struct
  include Ppx_count_example
end = struct
  module type Abstract = Ppx_count_example.Abstract

  module Simple = Ppx_count_example.Simple

  let%expect_test "Simple" =
    test (module Simple) Sample.char;
    [%expect {| (((count 1) (example A)) ((count 1) (example b))) |}]
  ;;

  module Named = Ppx_count_example.Named

  let%expect_test "Named" =
    test
      (module struct
        type t = Named.foo [@@deriving sexp_of]

        let count = Named.count_foo
      end)
      Sample.float;
    [%expect {| (((count 1) (example 0)) ((count 1) (example 1.5))) |}]
  ;;

  module Unary = Ppx_count_example.Unary

  let%expect_test "Unary" =
    test
      (module struct
        type t = int Unary.t [@@deriving sexp_of]

        let count = Unary.count count_int
      end)
      [ Array.of_list Sample.int ];
    [%expect {| (((count 2) (example (0 100)))) |}]
  ;;

  module Binary = Ppx_count_example.Binary

  let%expect_test "Binary" =
    test
      (module struct
        type t = (int, string) Binary.t [@@deriving sexp_of]

        let count = Binary.count count_int count_string
      end)
      (let%map.List left = Sample.int
       and right = Sample.string in
       ({ left; right } : (int, string) Binary.t));
    [%expect
      {|
      (((count 2) (example ((left 0) (right ""))))
       ((count 5) (example ((left 0) (right abc))))
       ((count 28) (example ((left 0) (right "lorem ipsum dolor sit amet"))))
       ((count 2) (example ((left 100) (right ""))))
       ((count 5) (example ((left 100) (right abc))))
       ((count 28) (example ((left 100) (right "lorem ipsum dolor sit amet")))))
      |}]
  ;;

  module Alias = Ppx_count_example.Alias

  let%expect_test "Alias" =
    test (module Alias) [ Sample.int ];
    [%expect {| (((count 2) (example (0 100)))) |}]
  ;;

  module Record = Ppx_count_example.Record

  let%expect_test "Record" =
    test
      (module Record)
      (let%map.List x = Sample.int
       and y = Sample.string in
       ({ x; y } : Record.t));
    [%expect
      {|
      (((count 2) (example ((x 0) (y ""))))
       ((count 5) (example ((x 0) (y abc))))
       ((count 28) (example ((x 0) (y "lorem ipsum dolor sit amet"))))
       ((count 2) (example ((x 100) (y ""))))
       ((count 5) (example ((x 100) (y abc))))
       ((count 28) (example ((x 100) (y "lorem ipsum dolor sit amet")))))
      |}]
  ;;

  module Variant = Ppx_count_example.Variant

  let%expect_test "Variant" =
    test
      (module Variant)
      ([ Variant.A ]
       @ List.map Sample.int ~f:(fun int -> Variant.B int)
       @
       let%map.List string = Sample.string
       and bool = Sample.bool in
       Variant.C (string, bool));
    [%expect
      {|
      (((count 1) (example A))
       ((count 2) (example (B 0)))
       ((count 2) (example (B 100)))
       ((count 2) (example (C "" false)))
       ((count 2) (example (C "" true)))
       ((count 5) (example (C abc false)))
       ((count 5) (example (C abc true)))
       ((count 28) (example (C "lorem ipsum dolor sit amet" false)))
       ((count 28) (example (C "lorem ipsum dolor sit amet" true))))
      |}]
  ;;

  module Multiple = Ppx_count_example.Multiple

  let%expect_test "Multiple" =
    test (module Multiple) [ 5L ];
    [%expect {| (((count 1) (example 5))) |}];
    test
      (module struct
        type t = Multiple.foo [@@deriving sexp_of]

        let count = Multiple.count_foo
      end)
      Sample.bool;
    [%expect {| (((count 1) (example false)) ((count 1) (example true))) |}];
    test
      (module struct
        type t = int Multiple.bar [@@deriving sexp_of]

        let count = Multiple.count_bar count_int
      end)
      (List.map Sample.int ~f:ref);
    [%expect {| (((count 2) (example 0)) ((count 2) (example 100))) |}]
  ;;

  module Empty_variant = Ppx_count_example.Empty_variant

  let%expect_test "Empty_variant" =
    test (module Empty_variant) [];
    [%expect {| () |}]
  ;;

  module Primitive_alias = Ppx_count_example.Primitive_alias

  let%expect_test "Primitive_alias" =
    test (module Primitive_alias) Sample.int;
    [%expect {| (((count 1) (example 0)) ((count 1) (example 100))) |}]
  ;;

  module Type_application_alias = Ppx_count_example.Type_application_alias

  let%expect_test "Type_application_alias" =
    test (module Type_application_alias) [ []; Sample.int ];
    [%expect {| (((count 0) (example ())) ((count 2) (example (0 100)))) |}]
  ;;

  module Tuple_alias = Ppx_count_example.Tuple_alias

  let%expect_test "Tuple_alias" =
    test
      (module Tuple_alias)
      (let%map.List int = Sample.int
       and string = Sample.string
       and bool = Sample.bool in
       int, string, bool);
    [%expect
      {|
      (((count 3) (example (0 "" false)))
       ((count 3) (example (0 "" true)))
       ((count 6) (example (0 abc false)))
       ((count 6) (example (0 abc true)))
       ((count 29) (example (0 "lorem ipsum dolor sit amet" false)))
       ((count 29) (example (0 "lorem ipsum dolor sit amet" true)))
       ((count 3) (example (100 "" false)))
       ((count 3) (example (100 "" true)))
       ((count 6) (example (100 abc false)))
       ((count 6) (example (100 abc true)))
       ((count 29) (example (100 "lorem ipsum dolor sit amet" false)))
       ((count 29) (example (100 "lorem ipsum dolor sit amet" true))))
      |}]
  ;;

  module Record_structure = Ppx_count_example.Record_structure

  let%expect_test "Record_structure" =
    test
      (module Record_structure)
      (let%map.List x = Sample.int
       and y = [ Sample.string ] in
       ({ x; y } : Record_structure.t));
    [%expect
      {|
      (((count 31) (example ((x 0) (y ("" abc "lorem ipsum dolor sit amet")))))
       ((count 31) (example ((x 100) (y ("" abc "lorem ipsum dolor sit amet"))))))
      |}]
  ;;

  module Variant_structure = Ppx_count_example.Variant_structure

  let%expect_test "Variant_structure" =
    test
      (module Variant_structure)
      ([ Variant_structure.A ]
       @ List.map Sample.int ~f:(fun int -> Variant_structure.B int)
       @
       let%map.List string = Sample.string
       and bool = Sample.bool in
       Variant_structure.C (string, bool));
    [%expect
      {|
      (((count 1) (example A))
       ((count 2) (example (B 0)))
       ((count 2) (example (B 100)))
       ((count 2) (example (C "" false)))
       ((count 2) (example (C "" true)))
       ((count 5) (example (C abc false)))
       ((count 5) (example (C abc true)))
       ((count 28) (example (C "lorem ipsum dolor sit amet" false)))
       ((count 28) (example (C "lorem ipsum dolor sit amet" true))))
      |}]
  ;;

  module Inline_record_variant = Ppx_count_example.Inline_record_variant

  let%expect_test "Inline_record_variant" =
    test
      (module Inline_record_variant)
      (let%map.List x = Sample.int
       and y = Sample.string in
       Inline_record_variant.A { x; y });
    [%expect
      {|
      (((count 2) (example (A (x 0) (y ""))))
       ((count 5) (example (A (x 0) (y abc))))
       ((count 28) (example (A (x 0) (y "lorem ipsum dolor sit amet"))))
       ((count 2) (example (A (x 100) (y ""))))
       ((count 5) (example (A (x 100) (y abc))))
       ((count 28) (example (A (x 100) (y "lorem ipsum dolor sit amet")))))
      |}]
  ;;

  module Polymorphic_variant_alias = Ppx_count_example.Polymorphic_variant_alias

  let%expect_test "Polymorphic_variant_alias" =
    test
      (module Polymorphic_variant_alias)
      ([ `A ]
       @ List.map Sample.int ~f:(fun int -> `B int)
       @
       let%map.List string = Sample.string
       and bool = Sample.bool in
       `C (string, bool));
    [%expect
      {|
      (((count 1) (example A))
       ((count 2) (example (B 0)))
       ((count 2) (example (B 100)))
       ((count 3) (example (C ("" false))))
       ((count 3) (example (C ("" true))))
       ((count 6) (example (C (abc false))))
       ((count 6) (example (C (abc true))))
       ((count 29) (example (C ("lorem ipsum dolor sit amet" false))))
       ((count 29) (example (C ("lorem ipsum dolor sit amet" true)))))
      |}]
  ;;

  module Type_variable_alias = Ppx_count_example.Type_variable_alias

  let%expect_test "Type_variable_alias" =
    test
      (module struct
        type t = int Type_variable_alias.t [@@deriving sexp_of]

        let count = Type_variable_alias.count count_int
      end)
      (None :: List.map Sample.int ~f:Option.some);
    [%expect
      {|
      (((count 1) (example ()))
       ((count 2) (example (0)))
       ((count 2) (example (100))))
      |}]
  ;;

  module Multiple_structure = Ppx_count_example.Multiple_structure

  let%expect_test "Multiple_structure" =
    test (module Multiple_structure) Sample.int;
    [%expect {| (((count 1) (example 0)) ((count 1) (example 100))) |}];
    test
      (module struct
        type t = Multiple_structure.foo [@@deriving sexp_of]

        let count = Multiple_structure.count_foo
      end)
      [ Sample.string ];
    [%expect {| (((count 29) (example ("" abc "lorem ipsum dolor sit amet")))) |}];
    test
      (module struct
        type t = int Multiple_structure.bar [@@deriving sexp_of]

        let count = Multiple_structure.count_bar count_int
      end)
      (None :: List.map Sample.int ~f:Option.some);
    [%expect
      {|
      (((count 1) (example ()))
       ((count 2) (example (0)))
       ((count 2) (example (100))))
      |}]
  ;;

  module Recursive_structure = Ppx_count_example.Recursive_structure

  let%expect_test "Recursive_structure" =
    test (module Recursive_structure) [ A []; A [ A []; A [] ] ];
    [%expect
      {| (((count 1) (example (A ()))) ((count 3) (example (A ((A ()) (A ())))))) |}]
  ;;

  module Mutually_recursive_structure = Ppx_count_example.Mutually_recursive_structure

  let%expect_test "Mutually_recursive_structure" =
    test (module Mutually_recursive_structure) [ A (B []); A (B [ A (B []) ]) ];
    [%expect
      {| (((count 2) (example (A (B ())))) ((count 4) (example (A (B ((A (B ())))))))) |}]
  ;;

  module Mutually_recursive_parameterized_structure =
    Ppx_count_example.Mutually_recursive_parameterized_structure

  let%expect_test "Mutually_recursive_parameterized_structure" =
    test
      (module struct
        type t = int Mutually_recursive_parameterized_structure.t [@@deriving sexp_of]

        let count = Mutually_recursive_parameterized_structure.count count_int
      end)
      (Mutually_recursive_parameterized_structure.Leaf
       :: List.map Sample.int ~f:(fun int ->
         Mutually_recursive_parameterized_structure.A
           (Mutually_recursive_parameterized_structure.B
              (Mutually_recursive_parameterized_structure.Leaf, int))));
    [%expect
      {|
      (((count 1) (example Leaf))
       ((count 4) (example (A (B Leaf 0))))
       ((count 4) (example (A (B Leaf 100)))))
      |}]
  ;;

  module Record_with_nested_tuple = Ppx_count_example.Record_with_nested_tuple

  let%expect_test "Record_with_nested_tuple" =
    test
      (module Record_with_nested_tuple)
      (let%map.List int = Sample.int
       and string = Sample.string
       and bool = Sample.bool in
       ({ x = int, string; y = bool } : Record_with_nested_tuple.t));
    [%expect
      {|
      (((count 4) (example ((x (0 "")) (y false))))
       ((count 4) (example ((x (0 "")) (y true))))
       ((count 7) (example ((x (0 abc)) (y false))))
       ((count 7) (example ((x (0 abc)) (y true))))
       ((count 30) (example ((x (0 "lorem ipsum dolor sit amet")) (y false))))
       ((count 30) (example ((x (0 "lorem ipsum dolor sit amet")) (y true))))
       ((count 4) (example ((x (100 "")) (y false))))
       ((count 4) (example ((x (100 "")) (y true))))
       ((count 7) (example ((x (100 abc)) (y false))))
       ((count 7) (example ((x (100 abc)) (y true))))
       ((count 30) (example ((x (100 "lorem ipsum dolor sit amet")) (y false))))
       ((count 30) (example ((x (100 "lorem ipsum dolor sit amet")) (y true)))))
      |}]
  ;;

  module Tuple = Ppx_count_example.Tuple

  let%expect_test "Tuple" =
    test
      (module Tuple)
      (let%map.List int = Sample.int
       and string = Sample.string in
       int, string);
    [%expect
      {|
      (((count 2) (example (0 "")))
       ((count 5) (example (0 abc)))
       ((count 28) (example (0 "lorem ipsum dolor sit amet")))
       ((count 2) (example (100 "")))
       ((count 5) (example (100 abc)))
       ((count 28) (example (100 "lorem ipsum dolor sit amet"))))
      |}]
  ;;

  module Mixed_variant = Ppx_count_example.Mixed_variant

  let%expect_test "Mixed_variant" =
    test
      (module Mixed_variant)
      ((let%map.List x = Sample.int
        and y = Sample.string in
        Mixed_variant.A { x; y })
       @ List.map Sample.int ~f:(fun int -> Mixed_variant.B int)
       @ [ Mixed_variant.C ]);
    [%expect
      {|
      (((count 2) (example (A (x 0) (y ""))))
       ((count 5) (example (A (x 0) (y abc))))
       ((count 28) (example (A (x 0) (y "lorem ipsum dolor sit amet"))))
       ((count 2) (example (A (x 100) (y ""))))
       ((count 5) (example (A (x 100) (y abc))))
       ((count 28) (example (A (x 100) (y "lorem ipsum dolor sit amet"))))
       ((count 2) (example (B 0)))
       ((count 2) (example (B 100)))
       ((count 1) (example C)))
      |}]
  ;;

  module Polymorphic_variant_inherit = Ppx_count_example.Polymorphic_variant_inherit

  let%expect_test "Polymorphic_variant_inherit" =
    test
      (module struct
        type t = Polymorphic_variant_inherit.base [@@deriving sexp_of]

        let count = Polymorphic_variant_inherit.count_base
      end)
      (`A :: List.map Sample.int ~f:(fun int -> `B int));
    [%expect
      {|
      (((count 1) (example A))
       ((count 2) (example (B 0)))
       ((count 2) (example (B 100))))
      |}];
    test
      (module struct
        type t = Polymorphic_variant_inherit.t [@@deriving sexp_of]

        let count = Polymorphic_variant_inherit.count
      end)
      (`A
       :: (List.map Sample.int ~f:(fun int -> `B int)
           @ List.map Sample.string ~f:(fun string -> `C string)));
    [%expect
      {|
      (((count 1) (example A))
       ((count 2) (example (B 0)))
       ((count 2) (example (B 100)))
       ((count 1) (example (C "")))
       ((count 4) (example (C abc)))
       ((count 27) (example (C "lorem ipsum dolor sit amet"))))
      |}]
  ;;

  module GADT = Ppx_count_example.GADT

  let%expect_test "GADT" =
    test
      (module struct
        type t = int GADT.t [@@deriving sexp_of]

        let count = GADT.count count_int
      end)
      [ Empty; Rec Empty; Rec (Rec Empty) ];
    [%expect
      {|
      (((count 1) (example Empty))
       ((count 2) (example (Rec Empty)))
       ((count 3) (example (Rec (Rec Empty)))))
      |}]
  ;;

  module Module_path = Ppx_count_example.Module_path

  let%expect_test "Module_path" =
    test
      (module Module_path)
      ([ Variant.A ]
       @ List.map Sample.int ~f:(fun int -> Variant.B int)
       @
       let%map.List string = Sample.string
       and bool = Sample.bool in
       Variant.C (string, bool));
    [%expect
      {|
      (((count 1) (example A))
       ((count 2) (example (B 0)))
       ((count 2) (example (B 100)))
       ((count 2) (example (C "" false)))
       ((count 2) (example (C "" true)))
       ((count 5) (example (C abc false)))
       ((count 5) (example (C abc true)))
       ((count 28) (example (C "lorem ipsum dolor sit amet" false)))
       ((count 28) (example (C "lorem ipsum dolor sit amet" true))))
      |}]
  ;;

  module Module_path_application = Ppx_count_example.Module_path_application

  let%expect_test "Module_path_application" =
    test
      (module Module_path_application)
      [ []; Variant.A :: List.map Sample.int ~f:(fun int -> Variant.B int) ];
    [%expect {| (((count 0) (example ())) ((count 5) (example (A (B 0) (B 100))))) |}]
  ;;

  module Module_path_non_t = Ppx_count_example.Module_path_non_t

  let%expect_test "Module_path_non_t" =
    test (module Module_path_non_t) Sample.float;
    [%expect {| (((count 1) (example 0)) ((count 1) (example 1.5))) |}]
  ;;

  module Custom_attribute = Ppx_count_example.Custom_attribute

  let%expect_test "custom_attribute" =
    test (module Custom_attribute) Sample.char;
    [%expect {| (((count 5) (example A)) ((count 5) (example b))) |}]
  ;;

  module Custom_attribute_on_field = Ppx_count_example.Custom_attribute_on_field

  let%expect_test "custom_attribute_on_field" =
    test
      (module Custom_attribute_on_field)
      (let%map.List x = Sample.int
       and y = Sample.string in
       ({ x; y } : Custom_attribute_on_field.t));
    [%expect
      {|
      (((count 9) (example ((x 0) (y ""))))
       ((count 9) (example ((x 0) (y abc))))
       ((count 9) (example ((x 0) (y "lorem ipsum dolor sit amet"))))
       ((count 9) (example ((x 100) (y ""))))
       ((count 9) (example ((x 100) (y abc))))
       ((count 9) (example ((x 100) (y "lorem ipsum dolor sit amet")))))
      |}]
  ;;

  module Custom_attribute_on_parameter = Ppx_count_example.Custom_attribute_on_parameter

  let%expect_test "custom_attribute_on_parameter" =
    test (module Custom_attribute_on_parameter) [ Sample.int ];
    [%expect {| (((count 4) (example (0 100)))) |}]
  ;;

  module Custom_attribute_hides_recursion =
    Ppx_count_example.Custom_attribute_hides_recursion

  let%expect_test "custom_attribute_hides_recursion" =
    let rec example : Custom_attribute_hides_recursion.t = { next = example } in
    test
      (module struct
        type t = Custom_attribute_hides_recursion.t

        let sexp_of_t (_ : t) = Sexp.Atom "<custom sexp>"
        let count = Custom_attribute_hides_recursion.count
      end)
      [ example ];
    [%expect {| (((count 1) (example "<custom sexp>"))) |}]
  ;;

  module Implicit_unboxed_record = Ppx_count_example.Implicit_unboxed_record

  let%expect_test "Implicit_unboxed_record" =
    let examples =
      let%map.List x = Sample.int
      and y = Sample.string in
      ({ x; y } : Implicit_unboxed_record.t)
    in
    test (module Implicit_unboxed_record) examples;
    [%expect
      {|
      (((count 2) (example ((x 0) (y ""))))
       ((count 5) (example ((x 0) (y abc))))
       ((count 28) (example ((x 0) (y "lorem ipsum dolor sit amet"))))
       ((count 2) (example ((x 100) (y ""))))
       ((count 5) (example ((x 100) (y abc))))
       ((count 28) (example ((x 100) (y "lorem ipsum dolor sit amet")))))
      |}];
    test
      (module struct
        type t = Implicit_unboxed_record.t

        let sexp_of_t = Implicit_unboxed_record.sexp_of_t
        let count t = Implicit_unboxed_record.count_u (Implicit_unboxed_record.unbox t)
      end)
      examples;
    [%expect
      {|
      (((count 1) (example ((x 0) (y ""))))
       ((count 4) (example ((x 0) (y abc))))
       ((count 27) (example ((x 0) (y "lorem ipsum dolor sit amet"))))
       ((count 1) (example ((x 100) (y ""))))
       ((count 4) (example ((x 100) (y abc))))
       ((count 27) (example ((x 100) (y "lorem ipsum dolor sit amet")))))
      |}]
  ;;

  module Implicit_unboxed_alias = Ppx_count_example.Implicit_unboxed_alias

  let%expect_test "Implicit_unboxed_alias" =
    let examples = [ 0.; 5. ] in
    test (module Implicit_unboxed_alias) examples;
    [%expect {| (((count 1) (example 0)) ((count 1) (example 5))) |}];
    test
      (module struct
        type t = Implicit_unboxed_alias.t

        let sexp_of_t = Implicit_unboxed_alias.sexp_of_t
        let count t = Implicit_unboxed_alias.count_u (Float_u.of_float t)
      end)
      examples;
    [%expect {| (((count 1) (example 0)) ((count 1) (example 5))) |}]
  ;;

  module Jkind_annotation = Ppx_count_example.Jkind_annotation

  let%expect_test "jkind_annotation" =
    test
      (module struct
        type t = int Jkind_annotation.t [@@deriving sexp_of]

        let count = Jkind_annotation.count count_int
      end)
      (List.map Sample.int ~f:(fun value -> { Jkind_annotation.value }));
    [%expect
      {| (((count 2) (example ((value 0)))) ((count 2) (example ((value 100))))) |}]
  ;;
end

let test_extension (type example) count_example sexp_of_example examples =
  test
    (module struct
      type t = example [@@deriving count, sexp_of]
    end)
    examples
;;

let%expect_test "extension" =
  test_extension [%count: int] [%sexp_of: int] Sample.int;
  [%expect {| (((count 1) (example 0)) ((count 1) (example 100))) |}];
  test_extension [%count: int list] [%sexp_of: int list] [ Sample.int ];
  [%expect {| (((count 2) (example (0 100)))) |}];
  test_extension
    [%count: int * string]
    [%sexp_of: int * string]
    (let%map.List int = Sample.int
     and string = Sample.string in
     int, string);
  [%expect
    {|
    (((count 2) (example (0 "")))
     ((count 5) (example (0 abc)))
     ((count 28) (example (0 "lorem ipsum dolor sit amet")))
     ((count 2) (example (100 "")))
     ((count 5) (example (100 abc)))
     ((count 28) (example (100 "lorem ipsum dolor sit amet"))))
    |}];
  test_extension
    [%count: [ `A | `B of int ]]
    [%sexp_of: [ `A | `B of int ]]
    (`A :: List.map Sample.int ~f:(fun int -> `B int));
  [%expect
    {|
    (((count 1) (example A))
     ((count 2) (example (B 0)))
     ((count 2) (example (B 100))))
    |}];
  test_extension [%count: (int[@count.custom 2]) list] [%sexp_of: int list] [ Sample.int ];
  [%expect {| (((count 4) (example (0 100)))) |}];
  test_extension [%count: ('a list[@count.custom 5])] [%sexp_of: int list] [ Sample.int ];
  [%expect {| (((count 5) (example (0 100)))) |}];
  test_extension [%count: ('a[@count.custom 2]) list] [%sexp_of: int list] [ Sample.int ];
  [%expect {| (((count 4) (example (0 100)))) |}]
;;
