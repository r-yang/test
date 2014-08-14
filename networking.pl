:- use_module(library(random)).

/* top */
% go(NoOfChanges, NoOfPeople, NoOfStations)

go(C, P, S):- go(C, P, S, Result, Tbl),
	display_schedule(Result), 
	display_individuals(Result, P),
	display_met_list(Tbl).

go(C, P, S, Result, Tbl):- P > 0, C > 0, S > 0,
	Reminder is P mod S, % work out how many per station
	((Reminder > 0,
	RealP is P + (S-Reminder)); % use RealP, such that each station will have exactly same people	
	(Reminder == 0, RealP is P)),
	SizeOfStation is RealP/S,
	mk_met_relation_tbl(RealP, Tbl),
	gen_ids(RealP, Ids), % Ids = [1,2,3,4,....RealP]
	go(1, C, SizeOfStation, Ids, Result, Tbl). % Result = [Swp1, Swp2, ..], Swp=[st1, st2,..]

go(S1, S2, _SizeOfS, _Ids, [], _Tbl):- S1 > S2, !. % all schedules generated 
go(S1, S2, SizeOfS, Ids, [Solution|Rest], Tbl):- % a typical for loop 
	arrange_meeting(SizeOfS, Ids, Solution, Tbl),% generate one arragement for current around => Solution
	Next is S1+1, % Increase swap counter
	go(Next, S2, SizeOfS, Ids, Rest, Tbl). % carry on to the next swap

arrange_meeting(_, [], [], _):- ! . % all people are assigned
arrange_meeting(SizeOfS, Ids, [S|Sn], Tbl):- % S = [id1, id2, ...]
	assign_one_station(SizeOfS, Ids, RestIds, S, Tbl),
	arrange_meeting(SizeOfS, RestIds, Sn, Tbl).

assign_one_station(SizeOfS, Ids, RestIds, S, Tbl):-
	assign_one_station(SizeOfS, Ids, RestIds, S, [], Tbl). % init the satck

assign_one_station(0, Ids, Ids, Sol, Sol, _). % when station is full
assign_one_station(SizeOfS, Ids, RestIds, S, SoFar, Tbl):- Size is SizeOfS - 1, % counting down
	ndet_pick_random_then_rm(X, Ids, Rest),
	met_check(X, SoFar, Tbl),
	set_met_list_true(X, SoFar, Tbl),
	assign_one_station(Size, Rest, RestIds, S, [X|SoFar], Tbl).
	
/* the array to reperesent meet relation

meet( 	row(1,_,_,_,..... ),
	row(_,1,_,_,..... ),
	....
	row(_,_, .......,1)
)
**************************************/

mk_met_relation_tbl(NoOfPeople, Tbl):-
	functor(Tbl, meet, NoOfPeople),
	mk_met_relation_rows(Tbl, NoOfPeople).

mk_met_relation_rows(Tbl, N):-
	mk_met_relation_rows(Tbl, 1, N). % iterate for N times

mk_met_relation_rows(Tbl, X, N):- X =< N, !,  % iterate for N times
	functor(R, row, N), % create a rowX
	arg(X, R, 1), % set (i,i) = 1
	arg(X, Tbl, R), % assign rowX to Tbl as Xth argument 
	K is X+1,
	mk_met_relation_rows(Tbl, K, N). % carry on
mk_met_relation_rows(_, X, N):- X > N.   % all done

set_met_true(X,Y,Tbl):- % set X met Y true
	arg(X, Tbl, RowX),
	arg(Y, RowX, 1),
	arg(Y, Tbl, RowY),
	arg(X, RowY, 1).

set_met_list_true(_,[], _). % all done
set_met_list_true(X,[H|T],Tbl):- set_met_true(X,H,Tbl), set_met_list_true(X, T, Tbl). % recursion

met(X,Y,Tbl):- % check if X and Y have met 
	arg(X, Tbl, RowX),
	arg(Y, RowX, K),
	K == 1.

havent_met(X,Y,Tbl):- % check if X and Y have met 
	arg(X, Tbl, RowX),
	arg(Y, RowX, K),
	var(K).

met_check(_, [] ,_).
met_check(X,[H|T] ,Tbl):- havent_met(X, H, Tbl), met_check(X, T, Tbl).

gen_ids(N, List):- gen_ids(1, N, List).

gen_ids(X, Y, [X]):- X==Y, !.
gen_ids(X, Y, [X|R]):- X<Y, Next is X+1, gen_ids(Next, Y, R).

rm_ith_item(H, 1, [H|R], R).  % if remove the first item
rm_ith_item(X, I, [H|R], [H|NewR]):- I >1, Next is I-1,   % if not, remove the first item
	rm_ith_item(X, Next, R, NewR).
	
% pick_random_then_rm(List, NewList, RemovedID).

pick_random_then_rm(List, NewList, RemovedID):- length(List, N), random_between(1,N, Rand),
	rm_ith_item(RemovedID, Rand, List, NewList).

ndet_pick_random_then_rm(Picked, List, NewList):-
	pick_random_then_rm(List, NewList, Picked).
ndet_pick_random_then_rm(Picked, List, NewList):- length(List, L), L > 30, % give up small list
	ndet_pick_random_then_rm(Picked, List, NewList).

/**** code for display results ***********************/
/*
Ressult = [ Round1, Round2, ...]
Round_i = [Table1, Table 2, ...]
Table_i = [Id1, Id2, ..]
*/

display_schedule(S):- display_schedule(S, 1).

display_schedule([], _N):- nl.
display_schedule([H|T], N):- 
	display_round(N, H), 
	K is N+1, display_schedule(T,K).

display_round(N, Tables):- write('at round '), write(N), nl, display_tables(Tables, 1).

display_tables([], _):- nl.
display_tables([H|T], N):- write('table '), write(N), write(': '), write_list(H),
	K is N+1, display_tables(T, K).

display_individuals(S, Total):- display_individuals(1, Total, S).

display_individuals(Who, Final, _S):- Who > Final, !, nl.
display_individuals(Who, Final, S):- display_individual(Who, S),
	nl, Next is Who+1, display_individuals(Next, Final, S).

display_individual(Id, S):- write('Speed Networking Card for                  ID '), write(Id), nl, nl, write('Please follow the schedule below:'),
	nl,
	display_individual(Id, 1, S).

display_individual(_, _, []):- nl.
display_individual(Id, R, [H|T]):- nl, write('at metting '), write(R), 
	write(' go to table '), find_table(Id, H, N, 1), write(N), NextR is R+1, 
	display_individual(Id, NextR, T).

find_table(_ID, [], 0, _Counter). /* not not found result 0 */
find_table(ID, [H|_], Res, Counter):- member(ID, H), !, Res = Counter. %% found it
find_table(ID, [_|T], Res, Counter):- C is Counter+1, find_table(ID, T, Res, C).

write_list([]):- nl.
write_list([H|T]):- write(H), write(' '), write_list(T).

display_met_list(RelationMatrix):- functor(RelationMatrix, _, Size),
	loop_display_met(1,Size, RelationMatrix).

loop_display_met(N,Size, _):- N > Size, !, nl. % all done
loop_display_met(N,Size, M):- K is N+1, arg(N, M, RowN), 
	write('person '), write(N), write(' will meet '), display_met(1, Size, N, RowN), nl,
	loop_display_met(K, Size, M).

display_met(K, N, _, _):- K>N, !.
display_met(K, N, Me, Row):- K==Me, !, M is K+1, display_met(M, N, Me, Row).
display_met(K, N, Me, Row):- M is K+1,arg(K, Row, Flag), write_if_nonvar(Flag, K), 
	display_met(M, N, Me, Row).

write_if_nonvar(Flag, Val):- ground(Flag), !, write(Val), write(' ').
write_if_nonvar(_, _). % otherwise do nothing
