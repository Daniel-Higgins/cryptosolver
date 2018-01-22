%% crypto.pro

%%load gv
:-['gv2.pro'].

% Establish the problem parameters

establishCryptoProblemParameters :-
declare(low,0),
declare(high,15).
:- establishCryptoProblemParameters.

% Generate a random number N within the previously specified range
generateRandomCryptoNumber(N) :-
    valueOf(low,Low),
    valueOf(high,High),
    HighPlus is High + 1,
    random(Low,HighPlus,N).

% Generate a random 5 numbered Crypto problem and add it to the KB
    generateRandomCryptoProblem :-
    generateRandomCryptoNumber(N1),
    generateRandomCryptoNumber(N2),
    generateRandomCryptoNumber(N3),
    generateRandomCryptoNumber(N4),
    generateRandomCryptoNumber(N5),
    generateRandomCryptoNumber(GoalNum),
    addCryptoProblemToKB(N1,N2,N3,N4,N5,GoalNum).

% Add the Crypto problem to KB
    addCryptoProblemToKB(N1,N2,N3,N4,N5,GoalNum) :-
    retract(problem(_,_)),
    assert(problem(numbers(N1,N2,N3,N4,N5),goal(GoalNum))).

% Add the problem if there is not a problem already in the KB
    addCryptoProblemToKB(N1,N2,N3,N4,N5,GoalNum) :-
    assert(problem(numbers(N1,N2,N3,N4,N5),goal(GoalNum))).

% Show Crypto problem present in the KB
    displayProblem :-
    problem(numbers(N1,N2,N3,N4,N5),goal(G)),
    write('Numbers = { '),write(N1),write(', '),
    write(N2),write(', '),write(N3),write(', '),
    write(N4),write(', '),write(N5),write(' } Goal = '),
    write(G),nl.

% Less demo
    %%demo :-
      %%  generateRandomCryptoProblem,
        %%displayProblem.

% Generate a number of crypto problems
genone :-
generateRandomCryptoProblem,
displayProblem.
generate(1) :-
genone.
generate(N) :-
genone,
NM1 is N - 1,
generate(NM1).
