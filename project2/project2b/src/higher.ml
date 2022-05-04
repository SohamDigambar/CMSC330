open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = fold (fun a x -> if x = e then true else a) false lst
let is_present lst x = map (fun a -> if a = x then 1 else 0) lst

let count_occ lst target =
  fold ( + ) 0 (map (fun a -> if a = target then 1 else 0) lst)

let uniq lst =
  let append_check ls elem =
    if contains_elem ls elem = false then elem :: ls else ls
  in
  fold append_check [] lst

let assoc_list lst = map (fun a -> (a, count_occ lst a)) (uniq lst)

(* @ will add f(arg) to the end of accumulator list *)
let ap fns args = fold (fun a x -> a @ map x args) [] fns
