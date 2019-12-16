:- use_module(library(clpfd)).

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
    Size is SubSize + 1.
getPuzzleSize([branch(_, Weights) | Puzzle], Size):-
    getPuzzleSize(Weights, BranchSize),
    getPuzzleSize(Puzzle, SubSize),
    Size is SubSize + BranchSize.

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

solvePuzzle:-
    puzzle8(Puzzle),
    getPuzzleSize(Puzzle, PuzzleSize),
    length(Vars, PuzzleSize),
    domain(Vars, 1, PuzzleSize),
    all_distinct(Vars),
    validPuzzle(Puzzle),
    labeling([], Vars),
    write(Vars).





