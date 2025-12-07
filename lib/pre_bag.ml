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
    let rec add_parts parts node =
      match parts with
      | [] -> { node with count = node.count + 1; value = Some elt }
      | part :: rest ->
          let child =
            match PartMap.find_opt part node.children with
            | Some c -> c
            | None -> empty_node
          in
          let updated = add_parts rest child in
          { node with children = PartMap.add part updated node.children }
    in
    { total = total + 1; root = add_parts (Key.parts elt) root }

  let singleton elt = add elt empty

  let remove elt ({ total; root } as bag) =
    let rec remove_parts parts node =
      match parts with
      | [] ->
          if node.count = 0 then (node, false)
          else
            let count = node.count - 1 in
            let value = if count = 0 then None else node.value in
            ({ node with count; value }, true)
      | part :: rest -> (
          match PartMap.find_opt part node.children with
          | None -> (node, false)
          | Some child ->
              let child', removed = remove_parts rest child in
              if not removed then (node, false)
              else
                let children =
                  if is_empty_node child' then PartMap.remove part node.children
                  else PartMap.add part child' node.children
                in
                ({ node with children }, true))
    in
    let updated_root, removed = remove_parts (Key.parts elt) root in
    if not removed then bag else { total = total - 1; root = updated_root }

  let count elt { root; _ } =
    let rec count_parts parts node =
      match parts with
      | [] -> node.count
      | part :: rest -> (
          match PartMap.find_opt part node.children with
          | None -> 0
          | Some child -> count_parts rest child)
    in
    count_parts (Key.parts elt) root

  let mem elt t = count elt t > 0
  let size { total; _ } = total

  let rec merge_nodes left right =
    let children =
      PartMap.merge
        (fun _ l r ->
          match (l, r) with
          | None, None -> None
          | Some c, None | None, Some c -> Some c
          | Some l, Some r -> Some (merge_nodes l r))
        left.children right.children
    in
    let count = left.count + right.count in
    let value =
      match (left.value, right.value) with
      | Some v, _ when left.count > 0 -> Some v
      | _, Some v when right.count > 0 -> Some v
      | _ -> None
    in
    { count; value; children }

  let union left right =
    {
      total = left.total + right.total;
      root = merge_nodes left.root right.root;
    }

  let rec equal_nodes left right =
    left.count = right.count
    && (match (left.value, right.value) with
      | None, None -> true
      | Some l, Some r -> Key.equal l r
      | _ -> false)
    && PartMap.cardinal left.children = PartMap.cardinal right.children
    && PartMap.for_all
         (fun key lchild ->
           match PartMap.find_opt key right.children with
           | None -> false
           | Some rchild -> equal_nodes lchild rchild)
         left.children

  let equal left right =
    left.total = right.total && equal_nodes left.root right.root

  let of_list items = List.fold_left (fun acc elt -> add elt acc) empty items

  let rec fold_node_left f acc node =
    let acc =
      match node.value with None -> acc | Some v -> apply_n node.count f acc v
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
    match node.value with
    | None -> acc
    | Some v -> apply_n_right node.count f v acc

  let fold_right f { root; _ } init = fold_node_right f root init
  let to_list bag = fold_right (fun elt acc -> elt :: acc) bag []

  let filter pred bag =
    fold_left (fun acc elt -> if pred elt then add elt acc else acc) empty bag

  let map f bag = fold_left (fun acc elt -> add (f elt) acc) empty bag
end
