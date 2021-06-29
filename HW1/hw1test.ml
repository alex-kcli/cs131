(*Sample Test Cases*)

(* 1 *)
let my_subset_test0 = subset [1;2;3] [2;1;3]
let my_subset_test1 = subset [10;20;30] [10;20;30;40]
let my_subset_test2 = subset [10;10] [10;40;40;40]


(* 2 *)
let my_equal_sets_test0 = equal_sets [7;9] [9;7;9;7]
let my_equal_sets_test1 = equal_sets [1;2;3] [3;2;1]
let my_equal_sets_test2 = equal_sets [1;1;1;1;1] [1;1]


(* 3 *)
let my_set_union_test0 = equal_sets (set_union [1;2;4] [1;2;3]) [1;2;3;4]
let my_set_union_test1 = equal_sets (set_union [1;6;8] [1;8;8]) [1;6;8;8]
let my_set_union_test2 = equal_sets (set_union [1;2;3;4] [2;2;2;2;2;2;5]) [1;2;3;4;5]


(* 4 *)
let my_set_all_union_test0 = equal_sets (set_all_union [[1];[2];[3];[4];[1];[2]]) [1;2;3;4]
let my_set_all_union_test1 = equal_sets (set_all_union [[1;2;3;4]; [6;3;6]; [3;2;5]]) [1;2;3;4;5;6]
let my_set_all_union_test2 = equal_sets (set_all_union [[1]; [1]; [1]]) [1]


(* 5 *)
(* Russell Paradox Question *)


(* 6 *)
let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x/10) 100 = 0
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x/50) 2500 = 0
let my_computed_fixed_point_test2 = computed_fixed_point (=) (fun x -> x *. 5.) 2. = infinity


(* 7 *)
type my_nonterminals = JOHN | MARY

let my_rules =
  [JOHN, [N MARY; T "read"; T "write"; T "listen"];
  MARY, [T "play"; T "walk"; T "swim"]]

let my_filter_reachable_test0 = filter_reachable (JOHN, my_rules) = (JOHN, my_rules)
let my_filter_reachable_test1 = filter_reachable (MARY, my_rules) = (MARY, [MARY, [T "play"; T "walk"; T "swim"]])
let my_filter_reachable_test0 = not (filter_reachable (MARY, my_rules) = (MARY, my_rules))
