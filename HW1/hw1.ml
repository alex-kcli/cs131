(* 1 *)
let rec subset a b = 
  match a with
  | [] -> true
  | x::xs -> (List.mem x b) && (subset xs b)



(* 2 *)
let equal_sets a b = 
  (subset a b) && (subset b a)



(* 3 *)
let rec set_union a b =
  match a with
  | [] -> b
  | [x] -> if (List.mem x b) then b else x::b
  | x::xs -> if (List.mem x b) then (set_union xs b) else x::(set_union xs b)



(* 4 *)
let rec set_all_union a = 
  match a with
  | [] -> []
  | [x] -> x
  | x::y::ys -> set_all_union ((set_union x y)::ys)



(* 5 *)
(* We are unable to write such a function in OCaml as the argument s we pass in "self_member s" would need to have some arbitrary
 * type. Since sets are represented as lists, every element within that list would also be of a certain type. Russell paradox suggests 
 * that if we have a set being member of itself, the set and the set that contains itself would need to be the same type for them to be
 * considered as the same set containing itself. This would not be possible for OCaml since a certain list and its element could not be
 * of the same type since they are the same as all normal list. In other words, we cannot represent such data structure in a way that
 * would make sense to OCaml and thus such function cannot be represented in OCaml *)



(* 6 *)
let rec computed_fixed_point eq f x = 
  if eq (f x) x then x
  else computed_fixed_point eq f (f x)



(* 7 *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal


let get_first a =
  match a with
  | (x, y) -> x


let get_second a =
  match a with
  | (x, y) -> y


let rec computed_fixed_point_rec eq f x y = 
  if eq (get_second(f x y)) y then (f x y)
  else computed_fixed_point_rec eq f x (get_second(f x y))


let rec get_nonterminal a =
  match a with
  | [] -> []
  | N x::xs -> x::(get_nonterminal xs)
  | T y::ys -> (get_nonterminal ys)


let rec get_reachable_symbols rules reachable_symbols =
  match rules with
  | [] -> (rules, reachable_symbols)
  | temp_rule::rest_rules -> match temp_rule with
                             | temp_symbol,rhs -> if List.mem temp_symbol reachable_symbols 
                                                  then get_reachable_symbols rest_rules (set_union reachable_symbols (get_nonterminal rhs))
                                                  else get_reachable_symbols rest_rules reachable_symbols


let filter_reachable g =
  let start_symbol = get_first g in
  let rules = get_second g in
  let _, reachable_symbols = computed_fixed_point_rec equal_sets get_reachable_symbols rules [start_symbol] in
  let filtered_rules = List.filter(fun x -> List.mem (get_first x) reachable_symbols) rules in
  (start_symbol, filtered_rules)
