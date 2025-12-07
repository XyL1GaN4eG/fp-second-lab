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

let random_string state =
  let len = Random.State.int state 9 in
  String.init len (fun _ -> Char.chr (97 + Random.State.int state 26))

let random_strings state =
  let len = Random.State.int state 20 in
  List.init len (fun _ -> random_string state)

let check_property name ?(trials = 200) property =
  Alcotest.test_case name `Quick (fun () ->
      let state = Random.State.make [| Hashtbl.hash name |] in
      let rec loop i =
        if i <= 0 then ()
        else if property state then loop (i - 1)
        else Alcotest.failf "%s failed after %d trials" name (trials - i + 1)
      in
      loop trials)

let property_union_left_identity state =
  let bag = bag_of_list (random_strings state) in
  Bag.equal (Bag.union Bag.empty bag) bag

let property_union_right_identity state =
  let bag = bag_of_list (random_strings state) in
  Bag.equal (Bag.union bag Bag.empty) bag

let property_union_associative state =
  let a = bag_of_list (random_strings state) in
  let b = bag_of_list (random_strings state) in
  let c = bag_of_list (random_strings state) in
  Bag.equal (Bag.union (Bag.union a b) c) (Bag.union a (Bag.union b c))

let property_remove_reverts_add state =
  let items = random_strings state in
  let value = random_string state in
  let bag = bag_of_list items in
  Bag.equal (Bag.remove value (Bag.add value bag)) bag

let property_tests =
  [
    check_property "union left identity" property_union_left_identity;
    check_property "union right identity" property_union_right_identity;
    check_property "union associative" property_union_associative;
    check_property "remove after add restores" property_remove_reverts_add;
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
