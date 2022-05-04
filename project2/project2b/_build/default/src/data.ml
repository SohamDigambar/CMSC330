open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode (x, None, IntLeaf, IntLeaf, IntLeaf)
  | IntNode (first_val, Some second_val, l_tree, m_tree, r_tree) ->
      if x < first_val then
        IntNode (first_val, Some second_val, int_insert x l_tree, m_tree, r_tree)
      else if x > second_val then
        IntNode (first_val, Some second_val, l_tree, m_tree, int_insert x r_tree)
      else if x > first_val && x < second_val then
        IntNode (first_val, Some second_val, l_tree, int_insert x m_tree, r_tree)
      else t
  | IntNode (first_val, None, l_tree, m_tree, r_tree) ->
      if x > first_val then IntNode (first_val, Some x, l_tree, m_tree, r_tree)
      else if x < first_val then
        IntNode (x, Some first_val, l_tree, m_tree, r_tree)
      else t

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (first_val, Some second_val, l_tree, m_tree, r_tree) ->
      if x < first_val then int_mem x l_tree
      else if x > second_val then int_mem x r_tree
      else if x > first_val && x < second_val then int_mem x m_tree
      else true
  | IntNode (first_val, None, l_tree, m_tree, r_tree) ->
      if x < first_val then int_mem x l_tree
      else if x > first_val then int_mem x r_tree
      else true

let rec int_size t =
  match t with
  | IntLeaf -> 0
  | IntNode (first_val, Some second_val, l_tree, m_tree, r_tree) ->
      int_size l_tree + int_size r_tree + int_size m_tree + 2
  | IntNode (first_val, None, l_tree, m_tree, r_tree) ->
      int_size l_tree + int_size r_tree + int_size m_tree + 1

let rec int_max t =
  let rec max max_val t =
    match t with
    | IntLeaf -> max_val
    | IntNode (first_val, None, l, m, r) -> first_val
    | IntNode (first_val, Some second_val, l_tree, m_tree, r_tree) ->
        max second_val r_tree
  in
  match t with
  | IntLeaf -> invalid_arg "int_max"
  | IntNode (first_val, Some second_val, l, m, r) -> max second_val t
  | IntNode (first_val, None, l, m, r) -> max first_val t
(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of
      (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t =
  let ret_exp = invalid_arg "map_put" in
  match t with
  | MapLeaf -> MapNode ((k, v), None, MapLeaf, MapLeaf, MapLeaf)
  | MapNode ((a, b), Some (c, d), l_tree, m_tree, r_tree) ->
      if k < a then
        MapNode ((a, b), Some (c, d), map_put k v l_tree, m_tree, r_tree)
      else if k > c then
        MapNode ((a, b), Some (c, d), l_tree, m_tree, map_put k v r_tree)
      else if k > a && k < c then
        MapNode ((a, b), Some (c, d), l_tree, map_put k v m_tree, r_tree)
      else ret_exp
  | MapNode ((a, b), None, l_tree, m_tree, r_tree) ->
      if k < a then MapNode ((k, v), Some (a, b), l_tree, m_tree, r_tree)
      else if k > a then MapNode ((a, b), Some (k, v), l_tree, m_tree, r_tree)
      else ret_exp

let rec map_contains k t =
  match t with
  | MapLeaf -> false
  | MapNode ((a, b), Some (c, d), l_tree, m_tree, r_tree) ->
      if k < a then map_contains k l_tree
      else if k > c then map_contains k r_tree
      else if k > a && k < c then map_contains k m_tree
      else true
  | MapNode ((a, b), None, l_tree, m_tree, r_tree) ->
      if k < a then map_contains k l_tree
      else if k > a then map_contains k r_tree
      else true

let rec map_get k t = failwith "unimplemented"

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = unit

let empty_table : lookup_table = ()
let push_scope (table : lookup_table) : lookup_table = failwith "unimplemented"
let pop_scope (table : lookup_table) : lookup_table = failwith "unimplemented"

let add_var name value (table : lookup_table) : lookup_table =
  failwith "unimplemented"

let rec lookup name (table : lookup_table) = failwith "unimplemented"
