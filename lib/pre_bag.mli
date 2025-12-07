(** Immutable multiset implemented as a prefix tree (trie). *)

module type KEY = sig
  type t
  (** Element type. *)

  type part
  (** Comparable fragment used to navigate the trie. *)

  val parts : t -> part list
  (** Splits an element into an ordered list of comparable fragments. *)

  val compare_part : part -> part -> int
  (** Total order on fragments. *)

  val equal : t -> t -> bool
  (** Equality on elements. *)
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

module Make (Key : KEY) : S with type elt = Key.t
