:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(lists)).

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

/*Print*/
print:-
    puzzle6s(Puzzle),
    getpuzzleinfo(Puzzle,Min,Max,NumBranches),
    %write(Min),nl,write(Max),nl,write(NumBranches),nl,
    Height is 5 * NumBranches + 1,
    Width is Max - Min + 1,
    make_empty_matrix(Width,Height,EmptyMatrix),
    %print_matrix(EmptyMatrix),
    insertinmatrix(EmptyMatrix,Puzzle,Min,Matrix),
    print_matrix(Matrix).


getpuzzleinfo([], _, Min, Max, NumBranches,Min, Max, NumBranches).
getpuzzleinfo([weight(Distance, _) | Puzzle],  RelPos, Min, Max, NumBranches,FMin, FMax, FNumBranches):-
    Pos is Distance + RelPos,
    NewMin is min(Pos,Min),
    NewMax is max(Pos,Max),
    getpuzzleinfo(Puzzle, RelPos, NewMin, NewMax, NumBranches, FMin, FMax, FNumBranches).

getpuzzleinfo([branch(Distance, Weights) | Puzzle], RelPos, Min, Max, NumBranches,FMin, FMax, FNumBranches):-
    NewRelPos is RelPos + Distance,
    NewNumBranches is NumBranches + 1,
    getpuzzleinfo(Weights, NewRelPos, Min, Max, NewNumBranches,NewMin, NewMax, NewerNumBranches),
    getpuzzleinfo(Puzzle, RelPos, NewMin, NewMax, NewerNumBranches, FMin, FMax, FNumBranches).

getpuzzleinfo(Puzzle,Min,Max,NumBranches):-
    getpuzzleinfo(Puzzle,0,0,0,1,Min,Max,NumBranches).


insertinmatrix(Matrix,PosX,Origin,Dest,Branch,FMatrix):-
    getbranchinfo(Branch,Min,Max),
    %nl,write(Min),nl,write(Max),nl,
    X is PosX + Min,
    Size is Max - Min + 1, 
    Y1 is Dest + 1,
    checkiffits(Matrix,X,Y1,Size),
    drawstring(Matrix,Origin,Dest,PosX,NewMatrix1), %print_matrix(NewMatrix1),nl,
    drawhorizontal(NewMatrix1,X,Y1,Size,NewMatrix2),%print_matrix(NewMatrix2),nl,
    Y2 is Dest + 2, 
    drawbranchelements(NewMatrix2,Branch,PosX,Y2,FMatrix). %print_matrix(FMatrix),nl.

insertinmatrix(Matrix,PosX,Origin,Dest,Branch,FMatrix):-
    %write('ola'),
    NewDest is Dest+3,
    insertinmatrix(Matrix,PosX,Origin,NewDest,Branch,FMatrix).

insertinmatrix(EmptyMatrix,Puzzle,Min,FinalMatrix):-
    Zero is -Min,
    insertinmatrix(EmptyMatrix,Zero,0,0,Puzzle,FinalMatrix).

checkiffits(Matrix,X,Y,Size):-
    nl,
    X1 is X-1,Y1 is Y-1,
    XN is max(X1,0),YN is max(Y1,0),
    checkiffits(Matrix,XN,YN,Size,4).
    nl.

checkiffits(_,_,_,_,0).
checkiffits(Matrix,X,Y,Size,Loops):-
    checkrow(Matrix,X,Y,Size),
    NLoops is Loops - 1,
    Y1 is Y+1,
    checkiffits(Matrix,X,Y1,Size,NLoops).

checkrow(_,_,_,-1).
checkrow(Matrix,X,Y,Size):-
    get_elem_at(X-Y,Matrix,A),
    !,A == 0,
   % write(A),
    NX is X+1,NSize is Size-1,
    checkrow(Matrix,NX,Y,NSize).


drawstring(FinalMatrix,Current,Dest,_PosX,FinalMatrix):- Current > Dest.

drawstring(Matrix,Current,Dest,PosX,FinalMatrix):-
    get_elem_at(PosX-Current,Matrix,A),
    A =='-',
    C1 is Current - 1,
    replace_elem_at(PosX-C1,'[',Matrix,NewMatrix),
    C2 is Current + 3,
    replace_elem_at(PosX-C2,']',NewMatrix,NewMatrix2),
    New is C2+1,
    drawstring(NewMatrix2,New,Dest,PosX,FinalMatrix).
drawstring(Matrix,Current,Dest,PosX,FinalMatrix):- 
    replace_elem_at(PosX-Current,'|',Matrix,NewMatrix),
    New is Current +1,
    drawstring(NewMatrix,New,Dest,PosX,FinalMatrix).

drawhorizontal(FinalMatrix,_,_,0,FinalMatrix).
drawhorizontal(Matrix,X,Y,Size,FinalMatrix):-
    NSize is Size-1,
    replace_elem_at(X-Y,'-',Matrix,NewMatrix),
    Xn is X + 1,
    drawhorizontal(NewMatrix,Xn,Y,NSize,FinalMatrix).

drawbranchelements(Matrix,Branch,PosX,Y,FMatrix):-
    drawbranchelements1st(Matrix,Branch,PosX,Y,NewMatrix),
    Y1 is Y + 1,
    drawbranchelements2nd(NewMatrix,Branch,PosX,Y1,FMatrix).

drawbranchelements1st(FinalMatrix,[],_,_,FinalMatrix).
drawbranchelements1st(Matrix,[weight(Distance, _) | Branch],PosX,Y,FMatrix):-
    X is PosX +  Distance,
    replace_elem_at(X-Y,'|',Matrix,NewMatrix),
    drawbranchelements1st(NewMatrix,Branch,PosX,Y,FMatrix).
drawbranchelements1st(Matrix,[branch(Distance, _) | Branch],PosX,Y,FMatrix):-
    X is PosX +  Distance,
    replace_elem_at(X-Y,'|',Matrix,NewMatrix),
    drawbranchelements1st(NewMatrix,Branch,PosX,Y,FMatrix).

drawbranchelements2nd(FinalMatrix,[],_,_,FinalMatrix).
drawbranchelements2nd(Matrix,[weight(Distance, Weight) | Branch],PosX,Y,FMatrix):-
    integer(Weight),
    X is PosX +  Distance,
    replace_elem_at(X-Y,Weight,Matrix,NewMatrix),
    drawbranchelements2nd(NewMatrix,Branch,PosX,Y,FMatrix).
drawbranchelements2nd(Matrix,[weight(Distance, Weight) | Branch],PosX,Y,FMatrix):-
    X is PosX +  Distance,
    replace_elem_at(X-Y,'?',Matrix,NewMatrix),
    drawbranchelements2nd(NewMatrix,Branch,PosX,Y,FMatrix).
drawbranchelements2nd(Matrix,[branch(Distance, NewBranch) | Branch],PosX,Y,FMatrix):-
    X is PosX +  Distance,
    insertinmatrix(Matrix,X,Y,Y,NewBranch,NewMatrix),
    drawbranchelements2nd(NewMatrix,Branch,PosX,Y,FMatrix).
    %drawbranchelements2nd(Matrix,Branch,PosX,Y,FMatrix).




getbranchinfo([weight(Distance, _) | Puzzle],Min,Max,FMin,FMax):-
    getbranchinfoaux(Puzzle,Distance,Max,FMin,FMax).
getbranchinfo([branch(Distance, _) | Puzzle],Min,Max,FMin,FMax):-
    getbranchinfoaux(Puzzle,Distance,Max,FMin,FMax).

getbranchinfoaux([weight(Distance, _) | Puzzle],Min,_Max,FMin,FMax):-
    getbranchinfoaux(Puzzle,Min,Distance,FMin,FMax).
getbranchinfoaux([branch(Distance, _) | Puzzle],Min,_Max,FMin,FMax):-
    getbranchinfoaux(Puzzle,Min,Distance,FMin,FMax).
getbranchinfoaux([],Min,Max,Min,Max).

getbranchinfo(Puzzle,Min,Max):-
    getbranchinfo(Puzzle,0,0,Min,Max).

print_matrix([]).
print_matrix([H|T]) :- write(H), nl, print_matrix(T).

make_empty_matrix(_,0,[]).    
make_empty_matrix(Width, Height, [Row|Matrix]):-
    Height > 0,
    make_row(Width, Row),
    Height1 is Height - 1,
    make_empty_matrix(Width, Height1, Matrix).

make_row(0, []). 
make_row(Width, [0 | Row]):-
    Width > 0,
    Width1 is Width - 1,
    make_row(Width1,Row).  


/* Replaces an element from a list
Arguments:
- List
- Index to replace
- New element
- Returned new list
*/
replace([_|T], 0, X, [X|T]).
replace([H | T], I, X, [H | R]):-
    I > 0,
    I1 is I - 1,
    replace(T, I1, X, R).


replace_elem_at(X-Y, Elem, Matrix, NewMatrix):-
    nth0(Y, Matrix, Row),
    replace(Row, X, Elem, NewRow),
    replace(Matrix, Y, NewRow, NewMatrix).

get_elem_at(X-Y, Matrix, Elem):-
    nth0(Y, Matrix, Row),
    nth0(X, Row, Elem).
get_elem_at(_-_, _, 0).


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

