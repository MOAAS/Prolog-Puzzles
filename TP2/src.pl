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
    length(Branches, NumSubBranches), NumSubBranches > 0, 
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
    addNWeights(RemainingWeights, FilledPuzzle, Puzzle).

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

selRandom(Var, _Rest, BB0, BB1):- % seleciona valor de forma aleatória
    fd_set(Var, Set), fdset_to_list(Set, List),
    random_member(Value, List), % da library(random)
    ( first_bound(BB0, BB1), Var #= Value ;
    later_bound(BB0, BB1), Var #\= Value ).
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

testprint:-
    makePuzzle(15,Puzzle,_),
    write(Puzzle),nl,
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

puzzle17([
    weight(-3,_),
    weight(-2,_),
    branch(-1,[
        branch(-1,[
            weight(-1,_),
            weight(1,_),
            weight(2,_)
        ]),
        branch(1,[
            branch(-1,[
                weight(-2,_),
                branch(1,[
                    weight(-1,_),
                    weight(2,_)
                ])
            ]),
            weight(2,_)
        ]),
        weight(2,_)
    ]),
    branch(1,[
        branch(-1,[
            weight(-1,_),
            weight(2,_)
        ]),
        weight(1,_),
        branch(2,[
            weight(-3,_),
            weight(1,_)
        ])
    ]),
    weight(2,_),
    weight(3,_)
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

puzzledomoas([
    weight(-1,8),
    branch(1,[
        branch(3,[
            weight(-4,1),
            weight(2,2),
            weight(-2,6),
            weight(4,3)
            ]),
        weight(-2,4),
        weight(-4,7)
        ]),
    weight(-3,5)
]).

puzzleb([
    branch(-3,[
        weight(-1,2),
        weight(-4,1),
        weight(1,6)
    ]),
    branch(1,[
        weight(3,3),
        weight(-2,7),
        weight(1,5)
    ]),
    weight(2,8),
    weight(-1,4)
]).

puzzlec([
    branch(3,[
        weight(4,1),
        weight(-4,5)
    ]),
     branch(-4,[
        weight(-1,1),
        weight(2,5)
    ])
]).
