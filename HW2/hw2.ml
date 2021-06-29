(* Tuple Manipulation *)
let get_first a =
  match a with
  | (x, y) -> x


let get_second a =
  match a with
  | (x, y) -> y


(* 1 convert_grammar *)
let convert_grammar gram1 =
  match gram1 with
  | (start, rules) -> let rec get_rhs_lst symbol rule_list = 
                               match rule_list with
                               | [] -> []
                               | x::xs -> match x with
                                          | (sym, rle) -> if sym = symbol then (rle::(get_rhs_lst symbol xs))
                                                                          else (get_rhs_lst symbol xs)
                               in
  let gram2 = (fun one_sym -> get_rhs_lst one_sym rules) in
  (start, gram2)




(* 2 parse_tree_leaves *)
let parse_tree_leaves tree =
  let start_list = match tree with
    | (Node (symbol, subtree)) -> subtree
    | (Leaf terminal) -> [Leaf terminal]
    in
  let rec parse_tree_leaves_list lst =
    match lst with
    | [] -> []
    | x::xs -> match x with
               | (Node (sym, sub_tree)) -> List.append (parse_tree_leaves_list sub_tree) (parse_tree_leaves_list xs)
               | (Leaf tmnl) -> List.append [tmnl] (parse_tree_leaves_list xs)
    in
  (parse_tree_leaves_list start_list)




(* 3 make_matcher *)
let make_matcher gram =
  let start = get_first gram in
  let fnctn = get_second gram in

  let match_nothing accept frag = None in
  let match_empty accept frag = accept frag in

  let rec make_or_matcher = function
  | [] -> match_nothing
  | head::tail ->
      let head_matcher = make_a_matcher head
      and tail_matcher = make_or_matcher tail
      in fun accept frag ->
	   let ormatch = head_matcher accept frag
	   in match ormatch with
		| None -> tail_matcher accept frag
		| _ -> ormatch
  
  and 
  
  make_a_matcher = function
  | [] -> match_empty
  | rulehd::ruletl -> match rulehd with
                      | (N nontermhd) -> fun accept frag ->
                                             let my_acceptor = (fun frag -> make_a_matcher ruletl accept frag) in
                                             (make_or_matcher (fnctn nontermhd) my_acceptor frag)
                   
                      | (T termhd) ->    fun accept frag -> match frag with
                                         | [] -> None
                                         | fraghd::fragtl -> if fraghd = termhd 
                                                             then make_a_matcher ruletl accept fragtl
                                                             else None
    in

  let match_making = (fun accept frag -> make_or_matcher (fnctn start) accept frag) in
  (match_making)




(* 4 *)
let make_parser gram frag = None

(*

let make_parser gram = 
  let start = get_first gram in
  let fnctn = get_second gram in

  let accept frag parse =
  match frag with
    | [] -> Some parse
    | x::xs -> None
    in

  let match_nothing accept frag parse = None in
  let match_empty accept frag parse = accept frag parse in

  let rec make_or_matcher = function
  | [] -> match_nothing
  | head::tail ->
      let head_matcher = make_a_matcher head
      and tail_matcher = make_or_matcher tail
      in fun accept frag parse ->
	   let ormatch = head_matcher accept frag (head::parse) 
	   in match ormatch with
		| None -> tail_matcher accept frag parse
		| _ -> ormatch
  
  and make_a_matcher = function
  | [] -> match_empty
  | rulehd::ruletl -> match rulehd with
                      | (N nontermhd) -> fun accept frag parse ->
                                             let new_rules = (fnctn nontermhd) in
                                             let new_accept = (fun frag parse -> make_a_matcher ruletl accept frag parse) in
                                             make_or_matcher new_rules new_accept frag parse
                   
                      | (T termhd) ->    fun accept frag parse -> match frag with
                                         | [] -> None
                                         | fraghd::fragtl -> if fraghd = termhd 
                                                             then make_a_matcher ruletl accept fragtl parse
                                                             else None
    in



  let parse_making = (fun frag -> let rev_parse = make_or_matcher (fnctn start) accept frag [] in 
                                  match rev_parse with 
                                  | None -> None
                                  | Some rev_par -> Some (List.rev rev_par)) in 
  (parse_making)
  

*)



