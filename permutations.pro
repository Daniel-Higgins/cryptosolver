%% Permutation Rules

permutations(A,B) :-
	perm(s(A,B),p(V,W)),
	write(V),write(' '), write(W),nl,
	fail.

permutations(A,B,C) :-
	perm(s(A,B,C),p(V,W,X)),
	write(V),write(' '),write(W),write(' '),write(X),nl,
	fail.

permutations(A,B,C,D) :-
	perm(s(A,B,C,D),p(V,W,X,Y)),
	write(V),write(' '),write(W),write(' '),write(X),write(' '),write(Y),nl,
	fail.

permutations(A,B,C,D,E) :-
	perm(s(A,B,C,D,E),p(V,W,X,Y,Z)),
	write(V),write(' '),write(W),write(' '),write(X),write(' '),write(Y),write(' '),write(Z),nl,
	fail.

%% Combination facts

combos(set(N1,N2,N3),combo(N1,N2),extras(N3)).
combos(set(N1,N2,N3),combo(N2,N3),extras(N1)).
combos(set(N1,N2,N3),combo(N1,N3),extras(N2)).

combos(set(N1,N2,N3,N4),combo(N1,N2),extras(N3,N4)).
combos(set(N1,N2,N3,N4),combo(N1,N3),extras(N2,N4)).
combos(set(N1,N2,N3,N4),combo(N1,N4),extras(N2,N3)).
combos(set(N1,N2,N3,N4),combo(N2,N3),extras(N1,N4)).
combos(set(N1,N2,N3,N4),combo(N2,N4),extras(N1,N3)).
combos(set(N1,N2,N3,N4),combo(N3,N4),extras(N1,N2)).

combos(set(N1,N2,N3,N4,N5),combo(N1,N2),extras(N3,N4,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N1,N3),extras(N2,N4,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N1,N4),extras(N2,N3,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N1,N5),extras(N2,N3,N4)).
combos(set(N1,N2,N3,N4,N5),combo(N2,N3),extras(N1,N4,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N2,N4),extras(N1,N3,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N2,N5),extras(N1,N3,N4)).
combos(set(N1,N2,N3,N4,N5),combo(N3,N4),extras(N1,N2,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N3,N5),extras(N1,N2,N4)).
combos(set(N1,N2,N3,N4,N5),combo(N4,N5),extras(N1,N2,N3)).
