:- ['permutations.pro'].
:- ['crypto.pro']. 

% DISPLAY SOLUTION
displaySolution :-
	%write('Solution: '),
	solution(S),
	displayResult(S),
	nl.
displaySolution.
displayResult(ex(A,O,B)) :-
	number(A),number(B),
	write('( '),write(A),write(' '),write(O),write(' '),write(B),write(' )').
displayResult(ex(A,O,B)) :-
	number(A), B = ex(A1,O1,B1),
	write('( '),write(A),write(' '),write(O),write(' '),
	displayResult(ex(A1,O1,B1)),write(' )').
displayResult(ex(A,O,B)) :-
	number(B), A = ex(A1,O1,B1),
	write('( '),displayResult(ex(A1,O1,B1)),write(' '),write(O),write(' '),
	write(B),write(' )').
displayResult(ex(A,O,B)) :-
	A = ex(A1,O1,B1), B = ex(A2,O2,B2),
	write('( '),displayResult(ex(A1,O1,B1)),write(' '),write(O),write(' '),
	displayResult(ex(A2,O2,B2)),write(' )').

% HEURISTIC SOLVER

rule(1,situation1,action1).
rule(2,situation2,action2).
rule(3,situation3,action3).
rule(4,situation4,action4).
rule(5,situation5,action5).
rule(6,situation6,action6).
rule(7,situation7,action7).
rule(8,situation8,action8).

solveProblemHeuristically :-
	retract(solution(_)),
	rule(Number,Situation,Action),
	write('considering rule '),write(Number),write('...'),nl,
	Situation,
	write('application of rule '),write(Number),write(' produces '),
	Action.
solveProblemHeuristically :-
	rule(Number,Situation,Action),
	write('considering rule '),write(Number),write('...'),nl,
	Situation,
	write('application of rule '),write(Number),write(' produces '),
	Action.
solveProblemHeuristically.

% HEURISTICS

% H1 
situation1 :-
	problem(Numbers, Goal),
	Goal = goal(0),
	Numbers = numbers(N1,N2,N3,N4,N5),
	member(0,[N1,N2,N3,N4,N5]).
action1 :-
	problem(Numbers,_),
	Numbers = numbers(N1,N2,N3,N4,N5),
	assert(solution(ex(N1,*,ex(N2,*,ex(N3,*,ex(N4,*,N5)))))).
	
% H2
situation2 :-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	member(G,[N1,N2,N3,N4,N5]),
	member(0,[N1,N2,N3,N4,N5]),
	not(G=0).
action2 :-
	problem(_,goal(G)),
	other_numbers(special(G),others(A,B,C,D)),
	assert(solution(ex(G,+,ex(A,*,ex(B,*,ex(C,*,D)))))).
	
% H3 
situation3 :-
	problem(_,goal(0)),
	doubleton.
action3 :-
	doubleton(doubleton(A,B),rest(C,D,E)),
	assert(solution(ex(ex(A,-,B),*,ex(C,*,ex(D,*,E))))).

% H4 
%all numbers are the same
situation4 :-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	N1=G,N2=G,N3=G,N4=G,N5=G.
action4 :-
	problem(numbers(N1,N2,N3,N4,N5),_),
	assert(solution(ex(N1,+,ex(ex(N2,-,N3),+,ex(N4,-,N5))))).

%H5 

situation5 :- 
    problem(numbers(N1,N2,N3,N4,N5),goal(G)),
    combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
    A=B, C=D, V is E - 2, V = G.
action5 :-
    problem(numbers(N1,N2,N3,N4,N5),goal(G)),
    combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
    A=B, C=D, V is E - 2, V = G,
    assert(solution(ex(E,-,ex(ex(A,/,B),+,ex(C,/,D))))).
    
%H6 
situation6 :-
    problem(numbers(N1,N2,N3,N4,N5),goal(G)),
    combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
    A=1,B=1,C=D,
    V is E+2,
    G = V,
    setsequence(A,B,C,D,E).
  action6 :-
    sequence(A,B,C,D,E),
    assert(solution(ex(ex(E,+,ex(A,+,B)),+,ex(C,-,D)))).
    
    
%H7
situation7:-
    problem(numbers(N1,N2,N3,N4,N5),goal(G)),
    combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
	A+B =:= G,member(0,[C,D,E]).
action7:-
    problem(numbers(N1,N2,N3,N4,N5),goal(G)),
    combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
	A+B =:= G,member(0,[C,D,E]),
    assert(solution(ex(ex(A,+,B),+ ,ex(ex(C,*,D),*,E)))).

%H8 
situation8 :-
	problem(numbers(A,B,C,D,E),goal(G)),
	doubleton(doubleton(A,B),rest(C,D,E)),
	member(G,[C,D,E]).
action8 :-
	problem(_,goal(G)),
	doubleton(doubleton(A,B),rest(C,D,E)),
	goal_from_three(C,D,E,G,X,Y),
	assert(solution(ex(G,+,ex(ex(A,-,B),*,ex(X,*,Y))))).
    

%------------------------------------------------------    
% HEURISTIC SUPPORT
	
member(X,[X|R],R).
member(X,[Y|R],Result) :-
	member(X,R,Subresult),
	Result = [Y|Subresult].

neighbor(A,B) :- A is B - 1.
neighbor(A,B) :- A is B + 1.

neighbor(G,[N|R],N,R) :-
	neighbor(N,G).
neighbor(G,[X|R],Neighbor,Rest) :-
	neighbor(G,R,Neighbor,More),
	Rest = [X|More].

neighbors([N1,N2,N3,N4],neighbors(N1,N2),others(N3,N4)) :- neighbor(N1,N2).
neighbors([N1,N2,N3,N4],neighbors(N1,N3),others(N2,N4)) :- neighbor(N1,N3).
neighbors([N1,N2,N3,N4],neighbors(N1,N4),others(N2,N3)) :- neighbor(N1,N4).
neighbors([N1,N2,N3,N4],neighbors(N2,N3),others(N1,N4)) :- neighbor(N2,N3).
neighbors([N1,N2,N3,N4],neighbors(N2,N4),others(N1,N3)) :- neighbor(N2,N4).
neighbors([N1,N2,N3,N4],neighbors(N3,N4),others(N1,N2)) :- neighbor(N3,N4).

%%
goal_from_three(N1,N2,N3,G,X,Y) :-
	N1=G,
	X is N2,
	Y is N3.
goal_from_three(N1,N2,N3,G,X,Y) :-
	N2=G,
	X is N1,
	Y is N3.
goal_from_three(N1,N2,N3,G,X,Y) :-
	N3=G,
	X is N1,
	Y is N2.
%%
doubleton :-
	problem(numbers(N1,N2,N3,N4,N5),_),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),_),
	A=B.
	
doubleton(doubleton(A,B),rest(C,D,E)) :-
	problem(numbers(N1,N2,N3,N4,N5),_),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
	A=B.

can_make_goal_with_two_numbers(the_two(A,B),the_rest(C,D,E),the_goal(G)) :-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
	crypto(A,B,G,_).
	
can_make_a_number_with_three_numbers(the_rest(A,B),the_three(C,D,E),the_sgoal(G)) :-
	problem(numbers(N1,N2,N3,N4,N5),_),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
	crypto(C,D,E,G,_).
	
other_numbers(special(N),others(N2,N3,N4,N5)) :-
	problem(numbers(N,N2,N3,N4,N5),goal(_)).
other_numbers(special(N),others(N1,N3,N4,N5)) :-
	problem(numbers(N1,N,N3,N4,N5),goal(_)).
other_numbers(special(N),others(N1,N2,N4,N5)) :-
	problem(numbers(N1,N2,N,N4,N5),goal(_)).
other_numbers(special(N),others(N1,N2,N3,N5)) :-
	problem(numbers(N1,N2,N3,N,N5),goal(_)).
other_numbers(special(N),others(N1,N2,N3,N4)) :-
	problem(numbers(N1,N2,N3,N4,N),goal(_)).
    
setsequence(N1,N2,N3,N4,N5) :-
    retract(sequence(_)),
    asserta(sequence(N1,N2,N3,N4,N5)).

setsequence(N1,N2,N3,N4,N5) :-
    asserta(sequence(N1,N2,N3,N4,N5)).
	
adjacent(A,B) :-
	crypto(A,1,B,ex(_,OP,_)),member(OP,[+,-]).

% DEMO (SOLVE RANDOM PROBLEM)
%-----------------------------------------------
demo :-
	generateRandomCryptoProblem,
	displayProblem,
	solveProblemHeuristically,
	displaySolution.
demo(0).
demo(N):-
	demo,
	K is N - 1,
	demo(K).
	
% ESTABLISH A PROBLEM
establishProblem(numbers(N1,N2,N3,N4,N5),goal(G)) :-
	addCryptoProblemToKB(N1,N2,N3,N4,N5,G).

% CRYPTO SOLVER

solve(numbers(N1,N2,N3,N4,N5),goal(G)) :-
	establishProblem(numbers(N1,N2,N3,N4,N5),goal(G)),
	displayProblem,
	solveProblemHeuristically,
	displaySolution.
solve :-
	generateRandomCryptoProblem,
	displayProblem,
	solveProblemHeuristically,
	displaySolution.

% ITERATOR

repeat(0,_).
repeat(N,P) :-
	P,
	K is N - 1,
	repeat(K,P).
	
