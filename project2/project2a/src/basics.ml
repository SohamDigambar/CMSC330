(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = match tup with a, b, c -> (c, b, a)
let is_odd x = abs x mod 2 = 1

let area x y =
  match (x, y) with (x1, y1), (x2, y2) -> abs ((y2 - y1) * (x2 - x1))

let volume x y =
  match (x, y) with
  | (x1, y1, z1), (x2, y2, z2) -> abs ((y2 - y1) * (x2 - x1) * (z2 - z1))

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n =
  match n with 0 -> 0 | 1 -> 1 | _ -> fibonacci (n - 1) + fibonacci (n - 2)

let rec pow x y = match y with 0 -> 1 | 1 -> x | _ -> x * pow x (y - 1)
let rec log x y = if y < x then 0 else 1 + log x (y / x)
let rec gcf x y = if y = 0 then x else gcf y (x mod y)

let rec is_prime x =
  if x <= 1 then false
  else
    let rec check_quotient x i =
      if i >= x then true
      else if x mod i = 0 then false
      else check_quotient x (i + 1)
    in
    check_quotient x 2

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst =
  let i = 0 in
  match lst with
  | [] -> failwith "Out of bounds"
  | h :: lst -> if i = idx then h else get (idx - 1) lst

let rec larger lst1 lst2 =
  let rec counter lst1 lst2 count1 count2 =
    match (lst1, lst2) with
    | [], [] -> if count1 > count2 then 1 else if count1 < count2 then 2 else 0
    | [], h2 :: t2 -> counter [] t2 count1 (count2 + 1)
    | h1 :: t1, [] -> counter t1 [] (count1 + 1) count2
    | h1 :: t1, h2 :: t2 -> counter t1 t2 (count1 + 1) (count2 + 1)
  in
  let x = counter lst1 lst2 0 0 in
  if x = 1 then lst1 else if x = 2 then lst2 else []

let reverse lst =
  let rec append_rev list1 list2 =
    match list1 with [] -> list2 | h :: t -> append_rev t (h :: list2)
  in
  append_rev lst []

let rec combine lst1 lst2 =
  match lst1 with [] -> lst2 | h :: t -> h :: combine t lst2

let rec merge lst1 lst2 =
  match (lst1, lst2) with
  | [], lst2 -> lst2
  | lst1, [] -> lst1
  | h_lst1 :: t_lst1, h_lst2 :: t_lst2 ->
      if h_lst1 < h_lst2 then h_lst1 :: merge t_lst1 lst2
      else h_lst2 :: merge lst1 t_lst2

let rec length lst = match lst with [] -> 0 | h :: t -> 1 + length t

let rec rotate shift lst =
  let shift = shift mod length lst in
  let rec rotate_once lst =
    match lst with [] -> [] | h :: t -> combine t [ h ]
  in
  match shift with 0 -> lst | _ -> rotate (shift - 1) (rotate_once lst)

let rec is_palindrome lst = lst = reverse lst