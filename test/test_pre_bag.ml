module String_key = struct
  type t = string
  type part = char

  let parts s = List.init (String.length s) (String.get s)
  let compare_part = Char.compare
  let equal = String.equal
end

module Bag = Pre_bag.Make (String_key)

let bag_of_list = List.fold_left (fun acc x -> Bag.add x acc) Bag.empty

let test_size_and_counts () =
  let bag = Bag.(empty |> add "foo" |> add "bar" |> add "foo") in
  Alcotest.(check int) "size tracks multiplicities" 3 (Bag.size bag);
  Alcotest.(check int) "count of foo" 2 (Bag.count "foo" bag);
  Alcotest.(check bool) "membership" true (Bag.mem "bar" bag);
  Alcotest.(check bool) "missing element" false (Bag.mem "qux" bag)

let test_remove_prunes () =
  let bag = Bag.(empty |> add "x" |> add "x" |> add "y") in
  let bag = Bag.remove "x" bag in
  Alcotest.(check int) "size after remove" 2 (Bag.size bag);
  Alcotest.(check int) "count after remove" 1 (Bag.count "x" bag);
  let bag = Bag.remove "x" bag in
  Alcotest.(check bool) "missing after all removes" false (Bag.mem "x" bag)

let test_filter_and_map () =
  let bag = Bag.(empty |> add "alpha" |> add "beta" |> add "gamma") in
  let filtered = Bag.filter (fun s -> String.length s = 4) bag in
  Alcotest.(check int) "filter keeps even length" 1 (Bag.size filtered);
  let mapped = Bag.map String.uppercase_ascii filtered in
  Alcotest.(check bool) "mapped element" true (Bag.mem "BETA" mapped)

let test_fold_projection () =
  let bag = Bag.(empty |> add "a" |> add "bbb" |> add "cc") in
  let total_len = Bag.fold_left (fun acc s -> acc + String.length s) 0 bag in
  Alcotest.(check int) "sum of lengths" 6 total_len;
  let concatenated = Bag.fold_right (fun s acc -> s ^ acc) bag "" in
  Alcotest.(check bool)
    "fold_right preserves multiplicity" true
    (String.length concatenated = 6)

open QCheck

let arb_string =
  let gen = Gen.string_size ~gen:Gen.printable Gen.(int_bound 8) in
  make ~print:Print.string ~shrink:Shrink.string gen

let arb_strings = list_of_size Gen.(int_bound 20) arb_string

let prop_union_left_identity =
  Test.make ~name:"union left identity" arb_strings (fun strings ->
      let bag = bag_of_list strings in
      Bag.equal (Bag.union Bag.empty bag) bag)

let prop_union_right_identity =
  Test.make ~name:"union right identity" arb_strings (fun strings ->
      let bag = bag_of_list strings in
      Bag.equal (Bag.union bag Bag.empty) bag)

let prop_union_associative =
  Test.make ~name:"union associative"
    (triple arb_strings arb_strings arb_strings) (fun (a, b, c) ->
      let a = bag_of_list a in
      let b = bag_of_list b in
      let c = bag_of_list c in
      Bag.equal (Bag.union (Bag.union a b) c) (Bag.union a (Bag.union b c)))

let prop_remove_reverts_add =
  Test.make ~name:"remove after add restores" (pair arb_string arb_strings)
    (fun (value, items) ->
      let bag = bag_of_list items in
      Bag.equal (Bag.remove value (Bag.add value bag)) bag)

let prop_union_count_additive =
  Test.make ~name:"union adds counts"
    (triple arb_string arb_strings arb_strings) (fun (value, left, right) ->
      let left_bag = bag_of_list left in
      let right_bag = bag_of_list right in
      let union_bag = Bag.union left_bag right_bag in
      Bag.count value union_bag
      = Bag.count value left_bag + Bag.count value right_bag)

let prop_size_matches_list =
  Test.make ~name:"size equals list length" arb_strings (fun strings ->
      let bag = bag_of_list strings in
      Bag.size bag = List.length (Bag.to_list bag))

let prop_map_id =
  Test.make ~name:"map id is identity" arb_strings (fun strings ->
      let bag = bag_of_list strings in
      Bag.equal (Bag.map (fun x -> x) bag) bag)

let prop_filter_true =
  Test.make ~name:"filter true is identity" arb_strings (fun strings ->
      let bag = bag_of_list strings in
      Bag.equal (Bag.filter (fun _ -> true) bag) bag)

let property_tests =
  List.map QCheck_alcotest.to_alcotest
    [
      prop_union_left_identity;
      prop_union_right_identity;
      prop_union_associative;
      prop_remove_reverts_add;
      prop_union_count_additive;
      prop_size_matches_list;
      prop_map_id;
      prop_filter_true;
    ]

let () =
  Alcotest.run "pre_bag"
    [
      ( "core",
        [
          Alcotest.test_case "size and counts" `Quick test_size_and_counts;
          Alcotest.test_case "remove shrinks bag" `Quick test_remove_prunes;
          Alcotest.test_case "filter and map" `Quick test_filter_and_map;
          Alcotest.test_case "fold projections" `Quick test_fold_projection;
        ] );
      ("properties", property_tests);
    ]
