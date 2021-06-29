% From TA hint code
% https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).



% From TA hint code
% set domain, list L has 1 through N
within_domain([], _).
within_domain([HD | TL], N) :-
    % http://www.gprolog.org/manual/html_node/gprolog057.html fd_domain(Vars, Lower, Upper)
    fd_domain(HD, 1, N),
    within_domain(TL, N).



% From TA hint code
% check row length
len_row(T, N) :-
    length(T, N).



% From TA hint code
% check col length
len_col([], _).
len_col([HD | TL], N) :-
    length(HD, N),
    len_col(TL, N).



% get value from a coordinate
get_value([I|J], T, Value) :-
    nth(I, T, Row),
    nth(J, Row, Value).



% get list of values from a list of coordinate
get_value_list([], _, []).
get_value_list([X], T, V) :-
    get_value(X, T, V).
get_value_list([Head | Tail], T, Value_list) :-
    get_value(Head, T, V),
    Value_list = [V | Value_lst],
    get_value_list(Tail, T, Value_lst).
    
    

% get sum of values from a list of values
get_sum([], 0).
get_sum([L], L).
get_sum([X, Y | Ys], S) :-
    get_sum([X + Y | Ys], S).



% get mult of values from a list of values
get_mult([], 0).
get_mult([L], L).
get_mult([X, Y | Ys], S) :-
    get_mult([X * Y | Ys], S).



% get difference of values from two values
get_diff(X, Y, V) :-
    V = X - Y.



% get quotient of values from a list of values
get_quot(X, Y, V) :-
    V = X / Y.



% check plus
check_plus(T, +(S, L)) :-
    get_value_list(L, T, Value_list),
    get_sum(Value_list, V),
    S #= V.



% check mult
check_mult(T, *(P, L)) :-
    get_value_list(L, T, Value_list),
    get_mult(Value_list, V),
    P #= V.



% check minus
check_minus(T, -(D, J, K)) :-
    get_value(J, T, V1),
    get_value(K, T, V2),
    get_diff(V1, V2, V01),
    get_diff(V2, V1, V02),
    (D #= V01; D #= V02).
     
     
     
% check division
check_div(T, /(Q, J, K)) :-
     get_value(J, T, V1),
     get_value(K, T, V2),
     get_quot(V1, V2, V01),
     get_quot(V2, V1, V02),
     (Q #= V01; Q #= V02).
      
      

% check all four operations
check_all(T, C) :-
      (check_plus(T, C);
       check_mult(T, C);
       check_minus(T, C);
       check_div(T, C)).
      
      
      
kenken(N, C, T) :-
    % array size limits
    len_row(T, N),
    len_col(T, N),

    % finish domain limits
    within_domain(T, N),

    % check every constraint
    maplist(check_all(T), C),
      
    maplist(fd_all_different, T),
    transpose(T, T1),
    maplist(fd_all_different, T1),
    maplist(fd_labeling, T).


      
      
%============ plain_kenken ============
      
% From TA slide
% for fd: fd_all_different
all_unique([]).
all_unique([Hd | Tl]) :-
    member(Hd, Tl), !, fail.
all_unique([_ | Tl]) :-
    all_unique(Tl).


      
% From TA hint code
% for fd: fd_domain
% a list contain N elements
% http://www.gprolog.org/manual/html_node/gprolog033.html
% http://www.gprolog.org/manual/gprolog.html#hevea_default674
% Domain is all the enumerated answers of between(1, N, X)
plain_within_domain([], _).
plain_within_domain(N, Domain) :-
    findall(X, between(1, N, X), Domain).
          

      
% From TA hint code
% fill in a 2D array with lists of fixed length (N)
% http://www.gprolog.org/manual/gprolog.html#sec215
fill_2d([], _).
fill_2d([Head | Tail], N) :-
    plain_within_domain(N, Domain),
    permutation(Domain, Head),
    fill_2d(Tail, N).

      

% plain_check plus
plain_check_plus(T, +(S, L)) :-
    get_value_list(L, T, Value_list),
    get_sum(Value_list, V),
    S =:= V.


      
% plain_check mult
plain_check_mult(T, *(P, L)) :-
    get_value_list(L, T, Value_list),
    get_mult(Value_list, V),
    P =:= V.

      

% plain_check minus
plain_check_minus(T, -(D, J, K)) :-
    get_value(J, T, V1),
    get_value(K, T, V2),
    get_diff(V1, V2, V01),
    get_diff(V2, V1, V02),
    (D =:= V01; D =:= V02).
           
      
      
% plain_check division
plain_check_div(T, /(Q, J, K)) :-
    get_value(J, T, V1),
    get_value(K, T, V2),
    get_quot(V1, V2, V01),
    get_quot(V2, V1, V02),
    (Q =:= V01; Q =:= V02).
            
       
      
% check all four operations
plain_check_all(T, C) :-
    (plain_check_plus(T, C);
     plain_check_mult(T, C);
     plain_check_minus(T, C);
     plain_check_div(T, C)).
            
         
      
plain_kenken(N, C, T) :-
    % array size limits
    len_row(T, N),
    len_col(T, N),
    fill_2d(T, N),

    maplist(all_unique, T),
    transpose(T, T1),
    maplist(all_unique, T1),

    % check every constraint
    maplist(plain_check_all(T), C).


      
kenken_testcase(
    6,
    [
      +(11, [[1|1], [2|1]]),
      /(2, [1|2], [1|3]),
      *(20, [[1|4], [2|4]]),
      *(6, [[1|5], [1|6], [2|6], [3|6]]),
      -(3, [2|2], [2|3]),
      /(3, [2|5], [3|5]),
      *(240, [[3|1], [3|2], [4|1], [4|2]]),
      *(6, [[3|3], [3|4]]),
      *(6, [[4|3], [5|3]]),
      +(7, [[4|4], [5|4], [5|5]]),
      *(30, [[4|5], [4|6]]),
      *(6, [[5|1], [5|2]]),
      +(9, [[5|6], [6|6]]),
      +(8, [[6|1], [6|2], [6|3]]),
      /(2, [6|4], [6|5])
    ]
).
      
      

kenken_testcase_small(
    4,
    [
     +(6, [[1|1], [1|2], [2|1]]),
     *(96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
     -(1, [3|1], [3|2]),
     -(1, [4|1], [4|2]),
     +(8, [[3|3], [4|3], [4|4]]),
     *(2, [[3|4]])
    ]
).
