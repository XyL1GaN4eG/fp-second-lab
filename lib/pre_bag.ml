module type KEY = sig
  type t
  type part

  val parts : t -> part list
  val compare_part : part -> part -> int
  val equal : t -> t -> bool
end

module type S = sig
  type elt
  type t

  val empty : t
  val singleton : elt -> t
  val of_list : elt list -> t
  val add : elt -> t -> t
  val remove : elt -> t -> t
  val mem : elt -> t -> bool
  val count : elt -> t -> int
  val size : t -> int
  val union : t -> t -> t
  val equal : t -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val map : (elt -> elt) -> t -> t
  val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val fold_right : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val to_list : t -> elt list
end

module Make (Key : KEY) : S with type elt = Key.t = struct
  module PartMap = Map.Make (struct
    type t = Key.part

    let compare = Key.compare_part
  end)

  type elt = Key.t
  type node = { count : int; value : elt option; children : node PartMap.t }
  type t = { total : int; root : node }

  let empty_node = { count = 0; value = None; children = PartMap.empty }
  let empty = { total = 0; root = empty_node }
  let is_empty_node node = node.count = 0 && PartMap.is_empty node.children

  let rec apply_n n f acc v =
    if n <= 0 then acc else apply_n (n - 1) f (f acc v) v

  let rec apply_n_right n f v acc =
    if n <= 0 then acc else f v (apply_n_right (n - 1) f v acc)

  let add elt { total; root } =
    let rec add_parts node = function
      | [] ->
          let value =
            Option.fold
              ~none:(Some elt)
              ~some:(fun existing ->
                assert (Key.equal existing elt);
                Some existing)
              node.value
          in
          { node with count = node.count + 1; value }
      | part :: rest ->
          let child =
            Option.value
              ~default:empty_node
              (PartMap.find_opt part node.children)
          in
          let updated = add_parts child rest in
          { node with children = PartMap.add part updated node.children }
    in
    { total = total + 1; root = add_parts root (Key.parts elt) }

  let singleton elt = add elt empty

  let remove elt ({ total; root } as bag) =
    let rec remove_parts node = function
      | [] ->
          if node.count = 0 then (node, false)
          else
            let count = node.count - 1 in
            let value = if count = 0 then None else node.value in
            ({ node with count; value }, true)
      | part :: rest ->
          Option.fold
            ~none:(node, false)
            ~some:(fun child ->
              let child', removed = remove_parts child rest in
              if not removed then (node, false)
              else
                let children =
                  if is_empty_node child' then
                    PartMap.remove part node.children
                  else PartMap.add part child' node.children
                in
                ({ node with children }, true))
            (PartMap.find_opt part node.children)
    in
    let updated_root, removed = remove_parts root (Key.parts elt) in
    if not removed then bag else { total = total - 1; root = updated_root }

  let count elt { root; _ } =
    let rec count_parts node = function
      | [] -> node.count
      | part :: rest ->
          Option.fold
            ~none:0
            ~some:(fun child -> count_parts child rest)
            (PartMap.find_opt part node.children)
    in
    count_parts root (Key.parts elt)

  let mem elt t = count elt t > 0
  let size { total; _ } = total

  let rec merge_nodes left right =
    let children =
      PartMap.merge
        (fun _ l r ->
          (function
            | None, None -> None
            | Some c, None | None, Some c -> Some c
            | Some l, Some r -> Some (merge_nodes l r))
            (l, r))
        left.children right.children
    in
    let count = left.count + right.count in
    let value =
      (function
        | Some l, Some r ->
            assert (Key.equal l r);
            Some l
        | Some v, None -> Some v
        | None, Some v -> Some v
        | None, None -> None)
        (left.value, right.value)
    in
    { count; value; children }

  let union left right =
    {
      total = left.total + right.total;
      root = merge_nodes left.root right.root;
    }

  let rec equal_nodes left right =
    left.count = right.count
    && Option.equal Key.equal left.value right.value
    && PartMap.equal equal_nodes left.children right.children

  let equal left right =
    left.total = right.total && equal_nodes left.root right.root

  let of_list items = List.fold_left (fun acc elt -> add elt acc) empty items

  let rec fold_node_left f acc node =
    let acc =
      Option.fold ~none:acc ~some:(fun v -> apply_n node.count f acc v)
        node.value
    in
    PartMap.fold
      (fun _ child acc -> fold_node_left f acc child)
      node.children acc

  let fold_left f init { root; _ } = fold_node_left f init root

  let rec fold_node_right f node acc =
    let acc =
      List.fold_right
        (fun (_, child) acc -> fold_node_right f child acc)
        (PartMap.bindings node.children)
        acc
    in
    Option.fold ~none:acc ~some:(fun v -> apply_n_right node.count f v acc)
      node.value

  let fold_right f { root; _ } init = fold_node_right f root init
  let to_list bag = fold_right (fun elt acc -> elt :: acc) bag []

  let filter pred bag =
    fold_left (fun acc elt -> if pred elt then add elt acc else acc) empty bag

  let map f bag = fold_left (fun acc elt -> add (f elt) acc) empty bag
end
