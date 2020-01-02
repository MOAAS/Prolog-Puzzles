:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(lists)).
:-consult('display.pl').

/* Solver */
validPuzzle([], 0, 0).
validPuzzle([weight(Distance, Weight) | Puzzle], Torque, Total):-
    NewTorque #= Torque + Distance * Weight,
    validPuzzle(Puzzle, NewTorque, Subtotal),
    Total #= Subtotal + Weight.

validPuzzle([branch(Distance, Weights) | Puzzle], Torque, Total):-
    validPuzzle(Weights, 0, BranchWeight),
    NewTorque #= Torque + Distance * BranchWeight,
    validPuzzle(Puzzle, NewTorque, Subtotal),
    Total #= Subtotal + BranchWeight.

validPuzzle(Puzzle):- validPuzzle(Puzzle, 0, _).

getPuzzleVars([], [], []).
getPuzzleVars([weight(Distance, Weight) | Puzzle], [Weight | Weights], [Distance | Distances]):-
    getPuzzleVars(Puzzle, Weights, Distances).
getPuzzleVars([branch(Distance, SubBranch) | Puzzle], Weights, [Distance | Distances]):-
    getPuzzleVars(SubBranch, BranchWeights, BranchDistances),
    append(BranchWeights, SubWeights, Weights),
    append(BranchDistances, SubDistances, Distances),
    getPuzzleVars(Puzzle, SubWeights, SubDistances).

/*
getPuzzleSize([], 0).
getPuzzleSize([weight(_, _) | Puzzle], Size):-
    getPuzzleSize(Puzzle, SubSize),
    Size #= SubSize + 1.
getPuzzleSize([branch(_, Weights) | Puzzle], Size):-
    getPuzzleSize(Weights, BranchSize),
    getPuzzleSize(Puzzle, SubSize),
    Size #= SubSize + BranchSize.

unifyPuzzleVars([], Vars, Vars).
unifyPuzzleVars([weight(_, Var) | Puzzle], [Var | Vars], NotUnified):-
    unifyPuzzleVars(Puzzle, Vars, NotUnified).
unifyPuzzleVars([branch(_, Weights) | Puzzle], Vars, NotUnified):-
    unifyPuzzleVars(Weights, Vars, BranchNotUnified),
    unifyPuzzleVars(Puzzle, BranchNotUnified, NotUnified).
*/

solvePuzzle(Puzzle, Solution):-
    getPuzzleVars(Puzzle, Solution, _Distances),
    length(Solution, PuzzleSize),
    domain(Solution, 1, PuzzleSize),
    all_distinct(Solution),
    validPuzzle(Puzzle),
    labeling([], Solution).

solver:-
    puzzle8(Puzzle8),
    puzzle20(Puzzle20),
    solvePuzzle(Puzzle8, Solution8),
    solvePuzzle(Puzzle20, Solution20),
    write('Puzzle 8 solution: '), write(Solution8), nl,
    write('Puzzle 20 solution: '), write(Solution20), nl.


/* Maker */  

/*
[
    branch(_135253033, [
        branch(_135253409, [
            weight(_135257127,_135256779),
            weight(_135257233,_135256885),
            branch(_135254581, [
                weight(_135257339,_135256991)
            ])
        ])
    ]),
    branch(_135256329,[])
].

*/

addNBranches(0, Puzzle, Puzzle).
addNBranches(N, Puzzle, NewPuzzle):-
    N > 0,
    N1 is N - 1,
    addBranch(Puzzle, Puzzle2),
    addNBranches(N1, Puzzle2, NewPuzzle).

addBranch(Puzzle, NewPuzzle):-
    random(0, 2, R),
    addBranch(R, Puzzle, NewPuzzle).

addBranch(0, Puzzle, [branch(_Dist, []) | Puzzle]).
addBranch(1, [], [branch(_Dist, [])]).
addBranch(1, Puzzle, [branch(_Dist, NewBranch) | OtherBranches]):- 
    random_select(branch(_Dist, SubBranch), Puzzle, OtherBranches),     
    addBranch(SubBranch, NewBranch).

fillBranch([], [weight(_, _), weight(_, _)], 2).
fillBranch([branch(_Distance, SubBranch)], [weight(_, _), branch(_Distance, FilledSubBranch)], NumWeights):-
    fillBranch(SubBranch, FilledSubBranch, SubBranchNumWeights),
    NumWeights is SubBranchNumWeights + 1.
fillBranch(Branch, FilledBranch, NumWeights):- 
    length(Branch, BranchLength), BranchLength > 1,
    fillSubBranches(Branch, FilledBranch, NumWeights).

fillSubBranches([], [], 0).
fillSubBranches([branch(_Distance1, SubBranch) | OtherBranches], [branch(_Distance1, FilledSubBranch) | OtherFilledBranches], NumWeights):-
    fillSubBranches(OtherBranches, OtherFilledBranches, OtherNumWeights),
    fillBranch(SubBranch, FilledSubBranch, SubBranchNumWeights),
    NumWeights is OtherNumWeights + SubBranchNumWeights.

addWeight(Puzzle, NewPuzzle):-
    random(0, 2, R),
    addWeight(R, Puzzle, NewPuzzle).

addNWeights(0, Puzzle, Puzzle).
addNWeights(N, Puzzle, NewPuzzle):-
    N > 0,
    N1 is N - 1,
    addWeight(Puzzle, Puzzle2),
    addNWeights(N1, Puzzle2, NewPuzzle).

% Se calhar 0 -> poe weight aqui
addWeight(0, Puzzle, [weight(_Dist, _Weight) | Puzzle]).

% Se calhar 1 mas n ha branches -> poe weight aqui
addWeight(1, Puzzle, [weight(_Dist, _Weight) | Puzzle]):-
    getWeightsAndBranches(Puzzle, _Weights, []).

% Se calhar 1 -> escolhe um branch a sorte e poe weight nele
addWeight(1, Puzzle, [branch(_Dist, NewBranch) | Rest]):-
    getWeightsAndBranches(Puzzle, Weights, Branches),
  %  write('Puzzle: '), write(Puzzle), nl, 
   % write('Weights: '), write(Weights), nl, 
  %  write('Branches: '), write(Branches), nl, nl,
    length(Branches, NumSubBranches), NumSubBranches > 0, % reverificar
    random_select(branch(_D, SubBranch), Branches, OtherBranches),     
    addWeight(SubBranch, NewBranch),
    append(OtherBranches, Weights, Rest).

getWeightsAndBranches([], [], []).
getWeightsAndBranches([branch(_D, _W) | Puzzle], Weights, [branch(_D, _W) | Branches]):-
    getWeightsAndBranches(Puzzle, Weights, Branches).
getWeightsAndBranches([weight(_D, _W) | Puzzle], [weight(_D, _W) | Weights], Branches):-
    getWeightsAndBranches(Puzzle, Weights, Branches).

% primeiro criam-se todos os branches: entre size / 2 a size - 2 
% depois vao se preenchendo aqueles que ainda nao tem dois elementos com weights
% por fim add weight aleatoriamente 
makeEmptyPuzzle(Size, Puzzle):-
    NumBranches is div(Size, 3),
    addNBranches(NumBranches, [], PuzzleBranches),
    fillBranch(PuzzleBranches, FilledPuzzle, NumWeights),
    RemainingWeights is Size - NumWeights,
   % write('Puzzle before fill: '), write(PuzzleBranches), nl, nl,
   % write('FilledPuzzle: '), write(FilledPuzzle), nl,
   % write('NumWeightsUsed: '), write(NumWeights), nl,
   % write('Remaining weights: '), write(RemainingWeights), nl,
    addNWeights(RemainingWeights, FilledPuzzle, Puzzle).
  %  nl, write('Final puzzle: '), write(Puzzle), nl.

noZeros([]).
noZeros([Elem | List]):-
    Elem #\= 0,
    noZeros(List).

makePuzzle(PuzzleSize, Puzzle, Weights):- 
    % Make random puzzle
    makeEmptyPuzzle(PuzzleSize, Puzzle),
    getPuzzleVars(Puzzle, Weights, Distances),
    append(Weights, Distances, Vars),
    % Establish domain
    DistanceMax is min(5, max(3, div(PuzzleSize, 2))),
    DistanceMin is 0 - DistanceMax,
    domain(Weights, 1, PuzzleSize),
    domain(Distances, DistanceMin, DistanceMax),
    % Enforce restrictions
    all_distinct(Weights),
    noZeros(Distances),
    noOverlappingDistances(Puzzle),
    validPuzzle(Puzzle),
  % labeling( [variable(selRandom)], Vars),
    % Calculate distance and solution
    labeling( [value(selRandom)], Vars).
   % labeling( [bisect], Vars).

selRandom(Var, _Rest, BB0, BB1):- % seleciona valor de forma aleatória
    fd_set(Var, Set), fdset_to_list(Set, List),
    random_member(Value, List), % da library(random)
    ( first_bound(BB0, BB1), Var #= Value ;
    later_bound(BB0, BB1), Var #\= Value ).

/*
filledPuzzle([
    branch(_47956689, [
        weight(_47957443,_47957445),
        branch(_47956995, [
            weight(_47957515,_47957517),
            weight(_47957525,_47957527)
        ])
    ]),
    branch(_47956213, [
        weight(_47957313,_47957315),
        branch(_47956515, [
            weight(_47957385,_47957387),
            weight(_47957395,_47957397)
        ])
    ])
]).

test:-
    filledPuzzle(X),
    write(X), nl, nl,
    addNWeights(2, X, P),
    write(P), nl.
*/

    



/*
randomPuzzleMaker(0, [], [], []).
randomPuzzleMaker(Size, Puzzle, Weights, Distances):-
    Size #> 0,
    random(0, 3, R),    
    randomPuzzleMaker(R, Size, Puzzle, Weights, Distances).

randomPuzzleMaker(0, Size, [weight(Distance, Weight) | Puzzle], [Weight | Weights], [Distance | Distances]):-
    NextSize #= Size - 1,
    randomPuzzleMaker(NextSize, Puzzle, Weights, Distances).
randomPuzzleMaker(2, Size, Puzzle, Weights, Distances):-
    randomPuzzleMaker(Size, Puzzle, Weights, Distances).
randomPuzzleMaker(1, Size, [branch(Distance, Branch) | Puzzle], Weights, [Distance | Distances]):-
    Distance #\= 0,
    randomPuzzleMaker(Size, Branch, BranchWeights, BranchDistances),
    append(BranchWeights, SubWeights, Weights),
    append(BranchDistances, SubDistances, Distances),
    length(BranchWeights, BranchSize),
   % write('Branch: '), write(Branch), write(' | Size = '), write(BranchSize), nl,
    NextSize #= Size - BranchSize,
    randomPuzzleMaker(NextSize, Puzzle, SubWeights, SubDistances).
*/

noOverlappingDistances(Puzzle):- 
    noOverlappingDistances(Puzzle, BranchDistances),
    all_distinct(BranchDistances).

noOverlappingDistances([], []).
noOverlappingDistances([weight(Distance, _Weight) | Puzzle], [Distance | BranchDistances]):-
    noOverlappingDistances(Puzzle, BranchDistances).

noOverlappingDistances([branch(Distance, Weights) | Puzzle], [Distance | BranchDistances]):-
    noOverlappingDistances(Weights, SubBranchDistances),
    all_distinct(SubBranchDistances),
    noOverlappingDistances(Puzzle, BranchDistances).

/*

makePuzzle(PuzzleSize, Puzzle, Weights):-
    randomPuzzleMaker(PuzzleSize, Puzzle, Weights, Distances),
    append(Weights, Distances, Vars),
    domain(Weights, 1, PuzzleSize),
    domain(Distances, -500, 500),
    all_distinct(Weights),
    noOverlappingDistances(Puzzle),
  %  write(Puzzle), nl,
    validPuzzle(Puzzle),
  %  write(Puzzle), nl,
   labeling( [variable(selRandom)], Vars),
   % labeling( [], Vars),
    write(Puzzle).
*/

testprint:-
    puzzle6s(Puzzle),
    printPuzzle(Puzzle).

/* Puzzles */

puzzleDefault([
    weight(-3, _),
    weight(-1, _),
    branch(2, [
        weight(-2, _),
        weight(-1, _),
        weight(1, _)
    ])
]).

puzzle8([
    branch(-1, [
        branch(-1, [
            weight(-2, _),
            weight(-1, _),
            weight(1, _)
        ]),
        weight(2, _)
    ]),
    branch(1, [
        weight(-3, _),
        weight(-2, _),
        weight(-1, _),
        weight(2, _)
    ])
]).



puzzle6([
    weight(-3,_),
    weight(-2,_),
    branch(1,[
        branch(-1,[
            weight(-3,_),
            weight(1,_)
        ]),
        weight(1,_)
    ]),
    weight(2,_)
]).

puzzle6s([
    weight(-3,3),
    weight(-2,1),
    branch(1,[
        branch(-1,[
            weight(-3,5),
            weight(1,2)
        ]),
        weight(1,6)
    ]),
    weight(2,4)
]).

puzzle20([
    branch(-4, [
        weight(-3,_),
        weight(-2,_),
        weight(3,_)
    ]),
    branch(-2, [
        weight(-2,_),
        weight(-1,_),
        branch(1, [
            branch(-1, [
                weight(-1,_),
                weight(2,_),
                weight(3,_)
            ]),
            weight(1,_),
            weight(2,_)
        ])
    ]),
    weight(1,_),
    branch(3, [
        branch(-1, [
            branch(-1, [
                weight(-1,_),
                branch(1, [
                    weight(-2,_),
                    weight(1,6)
                ]),
                weight(2,_),
                weight(3,_)
            ]),
            weight(1,_),
            weight(2,_)
        ]),
        weight(1,_),
        weight(2,_)
    ])
]).

