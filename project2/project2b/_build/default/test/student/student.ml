open OUnit2
open P2b.Data
open P2b.Funs
open P2b.Higher

let test_sanity _ = 
  assert_equal 1 1

let suite =
  "student" >::: [
    "sanity" >:: test_sanity
  ]

let _ = run_test_tt_main suite

let test1 = let t0 = empty_tree_map in
let t1 = map_put 5 "Hello" t0 in
let t2 = map_put 2 "Goodbye" t1 in
let t3 = map_put 4 "World" t2 in

if t0 = MapLeaf then 
  if t1 = MapNode ((5, "Hello"), None, MapLeaf, MapLeaf, MapLeaf) then 
    if t2 = MapNode ((2, "Goodbye"), Some (5, "Hello"), MapLeaf, MapLeaf, MapLeaf) then
      if t3 = MapNode ((2, "Goodbye"), Some (5, "Hello"), MapLeaf, MapNode ((4, "World"), None, MapLeaf, MapLeaf, MapLeaf), MapLeaf) then 
        "All true"
      else
        "4th failed"
    else
      "3rd failed"
  else
    "2nd failed"
else
"1st failed"
