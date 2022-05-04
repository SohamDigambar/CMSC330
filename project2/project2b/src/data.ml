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
  | IntNode (first_val, Some second_val, l_t, m_t, r_t) ->
      if x < first_val then
        IntNode (first_val, Some second_val, int_insert x l_t, m_t, r_t)
      else if x > second_val then
        IntNode (first_val, Some second_val, l_t, m_t, int_insert x r_t)
      else if x > first_val && x < second_val then
        IntNode (first_val, Some second_val, l_t, int_insert x m_t, r_t)
      else t
  | IntNode (first_val, None, l_t, m_t, r_t) ->
      if x > first_val then IntNode (first_val, Some x, l_t, m_t, r_t)
      else if x < first_val then IntNode (x, Some first_val, l_t, m_t, r_t)
      else t

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (first_val, Some second_val, l_t, m_t, r_t) ->
      if x < first_val then int_mem x l_t
      else if x > second_val then int_mem x r_t
      else if x > first_val && x < second_val then int_mem x m_t
      else true
  | IntNode (first_val, None, l_t, m_t, r_t) ->
      if x < first_val then int_mem x l_t
      else if x > first_val then int_mem x r_t
      else true

let rec int_size t =
  match t with
  | IntLeaf -> 0
  | IntNode (_, Some _, l_t, m_t, r_t) ->
      int_size l_t + int_size r_t + int_size m_t + 2
  | IntNode (_, None, l_t, m_t, r_t) ->
      int_size l_t + int_size r_t + int_size m_t + 1

let rec int_max t =
  let rec max max_val t =
    match t with
    | IntLeaf -> max_val
    | IntNode (first_val, None, _, _, _) -> first_val
    | IntNode (_, Some second_val, l_t, m_t, r_t) -> max second_val r_t
  in
  match t with
  | IntLeaf -> raise (Invalid_argument "int_max")
  | IntNode (_, Some second_val, _, _, _) -> max second_val t
  | IntNode (first_val, None, _, _, _) -> max first_val t
(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of
      (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t =
  match t with
  | MapLeaf -> MapNode ((k, v), None, MapLeaf, MapLeaf, MapLeaf)
  | MapNode ((a, b), Some (c, d), l_t, m_t, r_t) ->
      if k < a then MapNode ((a, b), Some (c, d), map_put k v l_t, m_t, r_t)
      else if k > c then MapNode ((a, b), Some (c, d), l_t, m_t, map_put k v r_t)
      else if k > a && k < c then
        MapNode ((a, b), Some (c, d), l_t, map_put k v m_t, r_t)
      else raise (Invalid_argument "map_put")
  | MapNode ((a, b), None, l_t, m_t, r_t) ->
      if k < a then MapNode ((k, v), Some (a, b), l_t, m_t, r_t)
      else if k > a then MapNode ((a, b), Some (k, v), l_t, m_t, r_t)
      else raise (Invalid_argument "map_put")

let rec map_contains k t =
  match t with
  | MapLeaf -> false
  | MapNode ((a, b), Some (c, d), l_t, m_t, r_t) ->
      if k < a then map_contains k l_t
      else if k > c then map_contains k r_t
      else if k > a && k < c then map_contains k m_t
      else true
  | MapNode ((a, b), None, _, _, _) -> k = a

let rec map_get k t =
  match t with
  | MapLeaf -> raise (Invalid_argument "map_get")
  | MapNode ((a, b), Some (c, d), l_t, m_t, r_t) ->
      if k < a then map_get k l_t
      else if k > c then map_get k r_t
      else if k > a && k < c then map_get k m_t
      else if k = a then b
      else d
  | MapNode ((a, b), None, _, _, _) ->
      if k = a then b else raise (Invalid_argument "map_get")

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = (string * int) list list

let empty_table : lookup_table = []
let push_scope (table : lookup_table) : lookup_table = [] :: table

let pop_scope (table : lookup_table) : lookup_table =
  match table with [] -> failwith "No scopes remain!" | h :: t -> t

let rec if_exists name scope =
  match scope with
  | [] -> false
  | (str, _) :: t -> if name <> str then if_exists name t else true

let add_var name value (table : lookup_table) : lookup_table =
  match table with
  | [] -> failwith "There are no scopes to add a variable to!"
  | h :: t ->
      if if_exists name h then failwith "Duplicate variable binding in scope!"
      else ((name, value) :: h) :: t

let rec lookup name (table : lookup_table) =
  let rec get name scope =
    match scope with
    | [] -> 0
    | (str, v) :: t -> if str <> name then get name t else v
  in
  match table with
  | [] -> failwith "Variable not found!"
  | h :: _ ->
      if if_exists name h then get name h else failwith "Variable not found!"
