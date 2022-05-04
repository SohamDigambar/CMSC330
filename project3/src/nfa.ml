open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma : 's list;
  qs : 'q list;
  q0 : 'q;
  fs : 'q list;
  delta : ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s : string) : char list =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* checks if lst1 contains any elements from lst2 *)
let rec contains lst1 lst2 =
  match lst1 with
  | [] -> false
  | h :: t -> if elem h lst2 then true else contains t lst2

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa : ('q, 's) nfa_t) (qs : 'q list) (s : 's option) : 'q list =
  let rec add_states acc delta =
    match delta with
    | [] -> acc
    | (q, option, f) :: t ->
        if option = s && elem q qs && elem f acc = false then
          add_states (f :: acc) t
        else add_states acc t
  in
  add_states [] nfa.delta

let e_closure (nfa : ('q, 's) nfa_t) (qs : 'q list) : 'q list =
  let rec e_closure_helper r' =
    let r = r' in
    if union r (move nfa r None) <> r then
      e_closure_helper (union r (move nfa r None))
    else r
  in
  e_closure_helper qs

let accept (nfa : ('q, char) nfa_t) (s : string) : bool =
  let rec accept_state seen char_lst =
    (* List of all epsilon transitions from seen list *)
    let epsilon_moves = e_closure nfa seen in
    match char_lst with
    | [] ->
        (* Checks if any state in epsilon moves is in final states list*)
        contains epsilon_moves nfa.fs
    | ch :: t -> accept_state (move nfa epsilon_moves (Some ch)) t
    (* This finds all the moves with Some char from any of the epsilon transition 
     * states and checks if any of it's episonal transitions is a final state *)
  in
  accept_state [ nfa.q0 ] (explode s)

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

(* Uses currying to map the e_closure with all the states that have been accumulated *)
let new_states (nfa : ('q, 's) nfa_t) (qs : 'q list) : 'q list list =
  rev
    (map (e_closure nfa)
       (fold_left
          (fun acc ch ->
            if elem (move nfa qs (Some ch)) acc then acc
            else move nfa qs (Some ch) :: acc)
          [] nfa.sigma))

let new_trans (nfa : ('q, 's) nfa_t) (qs : 'q list) :
    ('q list, 's) transition list =
  let trans_func ch acc =
    (qs, Some ch, e_closure nfa (move nfa qs (Some ch))) :: acc
  in
  fold_right trans_func nfa.sigma []

let new_finals (nfa : ('q, 's) nfa_t) (qs : 'q list) : 'q list list =
  if contains qs nfa.fs then [ qs ] else []

let rec nfa_to_dfa_step (nfa : ('q, 's) nfa_t) (dfa : ('q list, 's) nfa_t)
    (work : 'q list list) : ('q list, 's) nfa_t =
  match work with
  | [] -> dfa
  | curr_state :: t ->
      let dfa_acc =
        {
          sigma = dfa.sigma;
          qs = insert curr_state dfa.qs;
          q0 = dfa.q0;
          fs = union dfa.fs (new_finals nfa curr_state);
          delta = union dfa.delta (new_trans nfa curr_state);
        }
      in
      nfa_to_dfa_step nfa dfa_acc
        (diff (union t (new_states nfa curr_state)) dfa_acc.qs)

let nfa_to_dfa (nfa : ('q, 's) nfa_t) : ('q list, 's) nfa_t =
  let dfa =
    {
      sigma = nfa.sigma;
      qs = [ e_closure nfa [ nfa.q0 ] ];
      q0 = e_closure nfa [ nfa.q0 ];
      fs = [];
      delta = [];
    }
  in
  nfa_to_dfa_step nfa dfa [ e_closure nfa [ nfa.q0 ] ]
