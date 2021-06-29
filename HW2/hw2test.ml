let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type simple_sentence_nonterminals =
  | Subject | Person | Action | Object | Adjective

let simple_sentence_grammar =
  (Subject,
   function
     | Subject ->
         [[N Person];
          [N Object];
          [N Person; N Action; N Object]]
     | Person ->
	 [[T "John"];
          [T "Mary"]]
     | Action ->
         [[T "plays"]]
     | Object ->
         [[N Adjective; N Object];
          [T "soccer"];
	  [T "backetball"]]
     | Adjective ->
	 [[T "beautiful"];
	  [T "horrible"]])

let make_matcher_test =
        ((make_matcher simple_sentence_grammar accept_empty_suffix ["John";"plays";"horrible";"basketball"]) = Some [])

let make_parser_test = 
  let frag = ["Mary";"plays";"beautiful";"soccer"] in 
  let parse_tree = make_parser simple_sentence_grammar frag in
  match parse_tree with
  | Some tree -> parse_tree_leaves tree = frag
  | _ -> false

