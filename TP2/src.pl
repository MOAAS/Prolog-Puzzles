:- use_module(library(clpfd)).
:- use_module(library(random)).

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


getPuzzleVars([], []).
getPuzzleVars([weight(_Distance, Weight) | Puzzle], [Weight | Vars]):-
    getPuzzleVars(Puzzle, Vars).
getPuzzleVars([branch(_Distance, Weights) | Puzzle], Vars):-
    getPuzzleVars(Weights, BranchVars),
    append(BranchVars, SubVars, Vars),
    getPuzzleVars(Puzzle, SubVars).
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
    getPuzzleVars(Puzzle, Solution),
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

randomPuzzleMaker(Size, [], [], []):- Size #= 0.
randomPuzzleMaker(Size, Puzzle, Weights, Distances):-
    random(0, 2, R),
    randomPuzzleMaker(R, Size, Puzzle, Weights, Distances).

randomPuzzleMaker(0, Size, [weight(Distance, Weight) | Puzzle], [Weight | Weights], [Distance | Distances]):-
    NextSize #= Size - 1,
    randomPuzzleMaker(NextSize, Puzzle, Weights, Distances).
randomPuzzleMaker(1, Size, [branch(Distance, Branch) | Puzzle], Weights, [Distance | Distances]):-
    randomPuzzleMaker(Size, Branch, BranchWeights, BranchDistances),
    append(BranchWeights, SubWeights, Weights),
    append(BranchDistances, SubDistances, Distances),
    length(BranchWeights, BranchSize),
    NextSize #= Size - BranchSize,
    randomPuzzleMaker(NextSize, Puzzle, SubWeights, SubDistances).

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


makePuzzle(PuzzleSize, Puzzle, Weights):-
    randomPuzzleMaker(PuzzleSize, Puzzle, Weights, Distances),
    append(Weights, Distances, Vars),
    domain(Weights, 1, PuzzleSize),
    domain(Distances, -3, 3),
    all_distinct(Weights),
    validPuzzle(Puzzle),
    noOverlappingDistances(Puzzle),
    labeling( [variable(selRandom)], Vars),
    write(Puzzle).
    
selRandom(ListOfVars, Var, Rest):- random_select(Var, ListOfVars, Rest).

test:-
    puzzle8(X),
    noOverlappingDistances(X).

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

