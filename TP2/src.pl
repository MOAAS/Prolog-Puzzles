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

solvePuzzle(Puzzle, Solution):-
    getPuzzleSize(Puzzle, PuzzleSize),
    length(Solution, PuzzleSize),
    unifyPuzzleVars(Puzzle, Solution, []),
    domain(Solution, 1, PuzzleSize),
    all_distinct(Solution),
    validPuzzle(Puzzle),
    labeling([], Solution).

solver:-
    puzzle20(Puzzle),
    solvePuzzle(Puzzle, Solution),
    write(Solution).

/*
makePuzzle(Size, Puzzle, Solution):-
    length(Solution, Size),
    unifyPuzzleVars(Puzzle, Solution, []),
    domain(Solution, 1, Size),
    all_distinct(Solution),
    validPuzzle(Puzzle),
    labeling([], Solution).
    */

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

