printPuzzle(Puzzle):-
    getPuzzlePrintMatrix(Puzzle,Matrix),
    %print_matrix(Matrix),
    printPuzzleMatrix(Matrix).

getPuzzlePrintMatrix(Puzzle,Matrix):-
    getpuzzleinfo(Puzzle,Min,Max,NumBranches),
    %$write(Min),nl,write(Max),nl,write(NumBranches),nl,
    Height is 7 * NumBranches + 1,
    Width is Max - Min + 1 ,
    make_empty_matrix(Width,Height,EmptyMatrix),
    %print_matrix(EmptyMatrix),
    insertinmatrix(EmptyMatrix,Puzzle,Min,Matrix).
    %print_matrix(Matrix).


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
    getpuzzleinfo(Puzzle,0,99,-99,1,Min,Max,NumBranches).


insertinmatrix(Matrix,PosX,Origin,Dest,Branch,FMatrix):-
    getbranchinfo(Branch,Min,Max),
    X is PosX + Min,
    %write(Max),write(' '),write(Min),nl, write(PosX), nl,
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
    X1 is X-1,Y1 is Y-1,
    XN is max(X1,0),YN is max(Y1,0),
    NSize is Size+2,
    checkiffits(Matrix,XN,YN,NSize,4).

checkiffits(_,_,_,_,0).
checkiffits(Matrix,X,Y,Size,Loops):-
    %write('AQUIIII'),
    %write(X),write(' '),write(Y),nl,
    %write(Size),nl,
    checkrow(Matrix,X,Y,Size),
    NLoops is Loops - 1,
    Y1 is Y+1,
    checkiffits(Matrix,X,Y1,Size,NLoops).

checkrow(_,_,_,-1).
checkrow(Matrix,X,Y,Size):-
    Size >= 0,
    %write(X),write(' '),write(Y),write(' '),write(Size),nl,
    get_elem_at(X-Y,Matrix,A),
    %write('OLA'),
    %nl,write(A),nl,
    %write('AQUIIIIaa'),
    !,A = 0,
    NX is X+1,NSize is Size-1,
    %write(NSize),
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
    replace_elem_at(X-Y,'l',Matrix,NewMatrix),
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

/*
getbranchinfo(« Puzzle],Min,Max,FMin,FMax):-
    getbranchinfoaux(Puzzle,99,-99,FMin,FMax),
    nl.
getbranchinfo([branch(Distance, _) | Puzzle],Min,Max,FMin,FMax):-
    getbranchinfoaux(Puzzle,99,-99,FMin,FMax),
    nl.
getbranchinfoaux([weight(Distance, _) | Puzzle],Min,_Max,FMin,FMax):-
    getbranchinfoaux(Puzzle,Min,Distance,FMin,FMax).
getbranchinfoaux([branch(Distance, _) | Puzzle],Min,_Max,FMin,FMax):-
    getbranchinfoaux(Puzzle,Min,Distance,FMin,FMax).
getbranchinfoaux([],Min,Max,Min,Max).

getbranchinfo(Puzzle,Min,Max):-
    getbranchinfo(Puzzle,0,0,Min,Max).*/

getbranchinfo(Puzzle,Min,Max):-
    getbranchinfo(Puzzle,99,-99,Min,Max).

getbranchinfo([],Min, Max, Min,Max).
getbranchinfo([weight(Distance, _) | Puzzle],Min, Max, FMin, FMax):-
    NewMin is min(Distance,Min),
    NewMax is max(Distance,Max),
    getbranchinfo(Puzzle, NewMin, NewMax, FMin, FMax).
getbranchinfo([branch(Distance, _) | Puzzle],Min, Max, FMin, FMax):-
    NewMin is min(Distance,Min),
    NewMax is max(Distance,Max),
    getbranchinfo(Puzzle, NewMin, NewMax, FMin, FMax).


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


printPuzzleMatrix([]).
printPuzzleMatrix([Row|Matrix]):-
    printPuzzleRow(Row),
    printPuzzleMatrix(Matrix).

printPuzzleRowLine(['-'|Matrix]):-
    write('----|'),
    printPuzzleRowLine(Matrix).
printPuzzleRowLine(Matrix):-
    write('  '),
    printPuzzleRow(Matrix).


printPuzzleRow([]):-nl.
printPuzzleRow([0|Matrix]):-
    write('     '),
    printPuzzleRow(Matrix).

printPuzzleRow(['-'|Matrix]):-
    write('  |'),
    printPuzzleRowLine(Matrix).
printPuzzleRow(['|'|Matrix]):-
    write('  |  '),
    printPuzzleRow(Matrix).
printPuzzleRow(['['|Matrix]):-
    write('  ^  '),
    printPuzzleRow(Matrix).
printPuzzleRow([']'|Matrix]):-
    write('  v  '),
    printPuzzleRow(Matrix).
printPuzzleRow(['l'|Matrix]):-
    write(' _|_ '),
    printPuzzleRow(Matrix).
printPuzzleRow(['?'|Matrix]):-
    write('| ? |'),
    printPuzzleRow(Matrix).
printPuzzleRow([Elem|Matrix]):-
    write('| '),displayNum(Elem),write('|'),
    printPuzzleRow(Matrix).


displayNum(V):- %if the number has 2 digits doesnt print the space so it can fit
    V > 9 , write(V).
displayNum(V):-
    write(V),write(' ').


/** Utility functions **/

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