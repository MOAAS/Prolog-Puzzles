:- use_module(library(clpfd)).
:- use_module(library(samsort)).
:- use_module(library(random)).
:- use_module(library(lists)).
:-consult('display.pl').

/* Solver */

/* 
    validPuzzle(+Puzzle)
    validPuzzle(+Puzzle, -Torque, -TotalWeight)

Recursively enforces constraints on Puzzle, according to Weight rules. Unifies Torque and Weight with respective values.
Can be called with only the first argument if Torque must be 0 and Weight can be ignored.
*/
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

/* getPuzzleVars(+Puzzle, -Weights, -Distances)

Unifies Weights with a list containing all puzzle weights.
Unifies Distances with a list containing all puzzle distances.
*/
getPuzzleVars([], [], []).
getPuzzleVars([weight(Distance, Weight) | Puzzle], [Weight | Weights], [Distance | Distances]):-
    getPuzzleVars(Puzzle, Weights, Distances).
getPuzzleVars([branch(Distance, SubBranch) | Puzzle], Weights, [Distance | Distances]):-
    getPuzzleVars(SubBranch, BranchWeights, BranchDistances),
    append(BranchWeights, SubWeights, Weights),
    append(BranchDistances, SubDistances, Distances),
    getPuzzleVars(Puzzle, SubWeights, SubDistances).

/* solvePuzzle(+Puzzle, -Solution)
Solves a Weight puzzle. Solution will be a list containing the puzzle weight values (DFS order)
*/
solvePuzzle(Puzzle, Solution):-
    getPuzzleVars(Puzzle, Solution, _Distances),
    length(Solution, PuzzleSize),
    domain(Solution, 1, PuzzleSize),
    all_distinct(Solution),
    validPuzzle(Puzzle),
    labeling([], Solution).

% testestest
solver:-
    puzzle8(Puzzle8),
    puzzle20(Puzzle20),
    solvePuzzle(Puzzle8, Solution8),
    solvePuzzle(Puzzle20, Solution20),
    write('Puzzle 8 solution: '), write(Solution8), nl,
    write('Puzzle 20 solution: '), write(Solution20), nl.



/* Maker */  

/* addNBranches(+N, +Puzzle, -NewPuzzle)
Adds N branches to Puzzle, unifying result with NewPuzzle.
*/
addNBranches(0, Puzzle, Puzzle).
addNBranches(N, Puzzle, NewPuzzle):-
    N > 0,
    N1 is N - 1,
    addBranch(Puzzle, Puzzle2),
    addNBranches(N1, Puzzle2, NewPuzzle).

/* 
    addBranch(+Puzzle, -NewPuzzle)
    addBranch(+R, +Puzzle, -NewPuzzle)

Adds a branch to Puzzle, unifying with NewPuzzle.
If called with 2 arguments, has a 50% chance of adding a branch to the root, 50% of adding to a random subbranch (if no subbranches, adds to root)
If called with 3 arguments, R must be either 0 -> add to root, or 1 -> add to random subbranch
*/
addBranch(Puzzle, NewPuzzle):-
    random(0, 2, R),
    addBranch(R, Puzzle, NewPuzzle).
  
addBranch(0, Puzzle, [branch(_Dist, []) | Puzzle]). % 0 -> puts branch here
addBranch(1, [], [branch(_Dist, [])]). % 1 but no branches -> puts branch here
addBranch(1, Puzzle, [branch(_Dist, NewBranch) | OtherBranches]):- % 1 -> picks random branch, puts branch there
    random_select(branch(_Dist, SubBranch), Puzzle, OtherBranches),     
    addBranch(SubBranch, NewBranch).

/* fillBranch(+Branch, -FilledBranch, -NumWeights)

Fills Branch and all of its Subbranches, so that every branch has at least 2 elements. 
The added elements are weight(_, _). Unifies NumWeights with the total number of elements added.

Branch must not contain any element other than branch(_, _)
*/
fillBranch([], [weight(_, _), weight(_, _)], 2).
fillBranch([branch(_Distance, SubBranch)], [weight(_, _), branch(_Distance, FilledSubBranch)], NumWeights):-
    fillBranch(SubBranch, FilledSubBranch, SubBranchNumWeights),
    NumWeights is SubBranchNumWeights + 1.
fillBranch(Branch, FilledBranch, NumWeights):- 
    length(Branch, BranchLength), BranchLength > 1,
    fillSubBranches(Branch, FilledBranch, NumWeights).

/* fillSubBranches(+Branch, -FilledBranch, -NumWeights)

Same as fillBranch/3, but ignores number of elements of the root branch. (Meant for filling a list of subbranches)
*/
fillSubBranches([], [], 0).
fillSubBranches([branch(_Distance1, SubBranch) | OtherBranches], [branch(_Distance1, FilledSubBranch) | OtherFilledBranches], NumWeights):-
    fillSubBranches(OtherBranches, OtherFilledBranches, OtherNumWeights),
    fillBranch(SubBranch, FilledSubBranch, SubBranchNumWeights),
    NumWeights is OtherNumWeights + SubBranchNumWeights.

/* addNWeights(+N, +Puzzle, -NewPuzzle)
Adds N weights to Puzzle, unifying result with NewPuzzle.
*/
addWeight(Puzzle, NewPuzzle):-
    random(0, 2, R),
    addWeight(R, Puzzle, NewPuzzle).

addNWeights(0, Puzzle, Puzzle).
addNWeights(N, Puzzle, NewPuzzle):-
    N > 0,
    N1 is N - 1,
    addWeight(Puzzle, Puzzle2),
    addNWeights(N1, Puzzle2, NewPuzzle).

/* 
    addWeight(+Puzzle, -NewPuzzle)
    addWeight(+R, +Puzzle, -NewPuzzle)

Adds a weight to Puzzle, unifying with NewPuzzle.
If called with 2 arguments, has a 50% chance of adding a weight to the root, 50% of adding to a random subbranch (if no subbranches, adds to root)
If called with 3 arguments, R must be either 0 -> add to root, or 1 -> add to random subbranch
*/
addWeight(0, Puzzle, [weight(_Dist, _Weight) | Puzzle]). % 0 -> puts weight here
addWeight(1, Puzzle, [weight(_Dist, _Weight) | Puzzle]):- % 1 but no branches -> puts weight here
    getWeightsAndBranches(Puzzle, _Weights, []).
addWeight(1, Puzzle, [branch(_Dist, NewBranch) | Rest]):-  % 1 -> picks random branch, puts weight there
    getWeightsAndBranches(Puzzle, Weights, Branches),
    length(Branches, NumSubBranches), NumSubBranches > 0, 
    random_select(branch(_D, SubBranch), Branches, OtherBranches),     
    addWeight(SubBranch, NewBranch),
    append(OtherBranches, Weights, Rest).

/* getWeightsAndBranches(+Puzzle, -Weights, -Branches)

Separates Weights from Branches at root level.
*/
getWeightsAndBranches([], [], []).
getWeightsAndBranches([branch(_D, _W) | Puzzle], Weights, [branch(_D, _W) | Branches]):-
    getWeightsAndBranches(Puzzle, Weights, Branches).
getWeightsAndBranches([weight(_D, _W) | Puzzle], [weight(_D, _W) | Weights], Branches):-
    getWeightsAndBranches(Puzzle, Weights, Branches).

/* makeEmptyPuzzle(+PuzzleSize, -Puzzle)

Randomly makes a puzzle of exactly PuzzleSize weights, by combining the previous functions. Unifies result with Puzzle.
The puzzle's distances and weights will not be unified.
Steps:
- Create all of the branches (amount: between Size/3 and Size/3)
- Fill all the branches, so that each has at least two children
- With the remaining weights to place, randomly add them to the puzzle.
*/
makeEmptyPuzzle(Size, Puzzle):-
    NumBranches is div(Size, 3),
    addNBranches(NumBranches, [], PuzzleBranches),
    fillBranch(PuzzleBranches, FilledPuzzle, NumWeights),
    RemainingWeights is Size - NumWeights,
    addNWeights(RemainingWeights, FilledPuzzle, Puzzle).

/* noOverlappingDistances(+Puzzle)

Enforce restrictions, so that there are no Puzzle elements in the same place. 
(At the same level, two elements can't have the same distance)
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

/* noZeros(+List)

Enforce restrictions, so that no element from List can be zero.
*/
noZeros([]).
noZeros([Elem | List]):-
    Elem #\= 0,
    noZeros(List).

/* makePuzzle(+PuzzleSize, -Puzzle)

Makes and solves a random weight puzzle of size PuzzleSize. Puzzle is unified with result.
*/
makePuzzle(PuzzleSize, Puzzle):- 
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
    % Calculate distance and solution
   % labeling( [bisect], Vars).
    labeling( [value(selRandom)], Vars).

selRandom(Var, _Rest, BB0, BB1):- % seleciona valor de forma aleatória
    fd_set(Var, Set), fdset_to_list(Set, List),
    random_member(Value, List), % da library(random)
    ( first_bound(BB0, BB1), Var #= Value ;
    later_bound(BB0, BB1), Var #\= Value ).

%testestes
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
