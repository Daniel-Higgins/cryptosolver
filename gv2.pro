%% gv2.pro

declare(Var,Val) :-
    retract(binding(Var,_)),
    assert(binding(Var,Val)).
declare(Var,Val) :-
    assert(binding(Var,Val)).

bind(Variable,Value) :-
    retract(binding(Variable,_)),
    assert(binding(Variable,Value)).

valueOf(Variable,Value) :-
    binding(Variable,Value).

undeclare(Var) :-
    retract(binding(Var,_)).

%%Binding display
displayBindings :-
    binding(Variable,Value),
    write(Variable),write(' -> '),write(Value),nl,
    fail.
displayBindings.
%%Arithmetic Functionality
inc(Variable) :-
    retract(binding(Variable,Value)),
    NewValue is Value + 1,
    assert(binding(Variable,NewValue)).

dec(Variable) :-
    retract(binding(Variable,Value)),
    NewValue is Value - 1,
    assert(binding(Variable,NewValue)).

add(Variable,Number,Sum) :-
    binding(Variable,First),
    binding(Number,Second),
    NewValue is First + Second,
    assert(binding(Sum,NewValue)).

sub(Variable,Number,Diff) :-
    binding(Variable,First),
    binding(Number,Second),
    NewValue is First - Second,
    assert(binding(Diff,NewValue)).

mul(Variable,Number,Prod) :-
        binding(Variable,First),
    binding(Number,Second),
    NewValue is First * Second,
    assert(binding(Prod,NewValue)).

div(Variable,Number,Quo) :-
 binding(Variable,First),
 binding(Number,Second),
 NewValue is First * Second,
 assert(binding(Quo,NewValue)).
pow(Variable,Number,Pow) :-
 binding(Variable,First),
 binding(Number,Second),
 NewValue is First ^ Second,
 assert(binding(Pow,NewValue)).
