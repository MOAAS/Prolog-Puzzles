/* printPuzzle(+Puzzle)
   Prints the puzzle
*/
printPuzzle(Puzzle):-
    getPuzzlePrintMatrix(Puzzle,Matrix),
    printPuzzleMatrix(Matrix), !.

/* getPuzzlePrintMatrix(+Puzzle,-Matrix)
   Gets the puzzle print matrix, the print matrix is used to make
   a structure to how to print the tree. Elements are put in the print matrix
   so then they can be printed line by line
*/
getPuzzlePrintMatrix(Puzzle,Matrix):-
    getpuzzleinfo(Puzzle,Min,Max,NumBranches),
    Height is 7 * NumBranches + 1,
    Width is Max - Min + 1 ,
    make_empty_matrix(Width,Height,EmptyMatrix),
    insertbranchinmatrix(EmptyMatrix,Puzzle,Min,Matrix).


/* getpuzzleinfo(+Puzzle,-Min,-Max,-NumBranches)
   Gets the puzzle info: Minimum and maximum coords and num of branches
*/
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

/* insertbranchinmatrix(+Matrix,+PosX,+Origin,+Dest,+Branch,-FMatrix)
   Inserts a branch into the matrix.Its the main function and is called
   for every branch.
*/
insertbranchinmatrix(Matrix,PosX,Origin,Dest,Branch,FMatrix):-
    getbranchinfo(Branch,Min,Max),
    X is PosX + Min,
    Size is Max - Min + 1, 
    Y1 is Dest + 1,
    checkiffits(Matrix,X,Y1,Size),
    drawsverticalstring(Matrix,Origin,Dest,PosX,NewMatrix1),
    drawhorizontal(NewMatrix1,X,Y1,Size,NewMatrix2),
    Y2 is Dest + 2, 
    drawbranchelements(NewMatrix2,Branch,PosX,Y2,FMatrix).

insertbranchinmatrix(Matrix,PosX,Origin,Dest,Branch,FMatrix):-
    NewDest is Dest + 3,
    insertbranchinmatrix(Matrix,PosX,Origin,NewDest,Branch,FMatrix).

insertbranchinmatrix(EmptyMatrix,Puzzle,Min,FinalMatrix):-
    Zero is -Min,
    insertbranchinmatrix(EmptyMatrix,Zero,0,0,Puzzle,FinalMatrix).

/* checkiffits(+Matrix,+X,+Y,+Size)
   Check if a branch with certain dimensions doesnt colide with 
   previus inserted elements.
*/
checkiffits(Matrix,X,Y,Size):-
    X1 is X-1,Y1 is Y-1,
    XN is max(X1,0),YN is max(Y1,0),
    NSize is Size+2,
    checkiffits(Matrix,XN,YN,NSize,4).

checkiffits(_,_,_,_,0).
checkiffits(Matrix,X,Y,Size,Loops):-
    checkrow(Matrix,X,Y,Size),
    NLoops is Loops - 1,
    Y1 is Y+1,
    checkiffits(Matrix,X,Y1,Size,NLoops).

checkrow(_,_,_,-1).
checkrow(Matrix,X,Y,Size):-
    Size >= 0,
    get_elem_at(X-Y,Matrix,A),
    !,A = 0,
    NX is X+1,NSize is Size-1,
    checkrow(Matrix,NX,Y,NSize).

/* drawsverticalstring(+Matrix,+Current,+Dest,+PosX,-FinalMatrix)
   Draws a vertical line in the matrix from Current to Dest. Withou coliding
   with previus inserted elements
*/
drawsverticalstring(FinalMatrix,Current,Dest,_PosX,FinalMatrix):- Current > Dest.

drawsverticalstring(Matrix,Current,Dest,PosX,FinalMatrix):-
    get_elem_at(PosX-Current,Matrix,A),
    A =='-',
    C1 is Current - 1,
    replace_elem_at(PosX-C1,'[',Matrix,NewMatrix),
    C2 is Current + 4,
    drawstringcolision(NewMatrix,C2,Dest,PosX,FinalMatrix).

drawsverticalstring(Matrix,Current,Dest,PosX,FinalMatrix):- 
    replace_elem_at(PosX-Current,'|',Matrix,NewMatrix),
    New is Current +1,
    drawsverticalstring(NewMatrix,New,Dest,PosX,FinalMatrix).

drawstringcolision(Matrix,Current,Dest,PosX,FinalMatrix):-
    get_elem_at(PosX-Current,Matrix,A),
    A \= 0,
    C1 is Current + 4,
    drawstringcolision(Matrix,C1,Dest,PosX,FinalMatrix).

drawstringcolision(Matrix,Current,Dest,PosX,FinalMatrix):-
    replace_elem_at(PosX-Current,']',Matrix,NewMatrix),
    C1 is Current+1,
    drawsverticalstring(NewMatrix,C1,Dest,PosX,FinalMatrix).


/* drawhorizontal(+Matrix,+X,+Y,+Size,-FinalMatrix)
   Draws a horizontal line in the matrix from X-Y position with size Size
*/
drawhorizontal(FinalMatrix,_,_,0,FinalMatrix).
drawhorizontal(Matrix,X,Y,Size,FinalMatrix):-
    NSize is Size-1,
    replace_elem_at(X-Y,'-',Matrix,NewMatrix),
    Xn is X + 1,
    drawhorizontal(NewMatrix,Xn,Y,NSize,FinalMatrix).


/* drawbranchelements(+Matrix,+Branch,+PosX,+Y,-FMatrix)
   Draws the elements from a branch. If the elemet is a branch
   it will call insertbranchinmatrix.
*/
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
drawbranchelements2nd(Matrix,[weight(Distance, _Weight) | Branch],PosX,Y,FMatrix):-
    X is PosX +  Distance,
    replace_elem_at(X-Y,'?',Matrix,NewMatrix),
    drawbranchelements2nd(NewMatrix,Branch,PosX,Y,FMatrix).
drawbranchelements2nd(Matrix,[branch(Distance, NewBranch) | Branch],PosX,Y,FMatrix):-
    X is PosX +  Distance,
    insertbranchinmatrix(Matrix,X,Y,Y+1,NewBranch,NewMatrix),
    drawbranchelements2nd(NewMatrix,Branch,PosX,Y,FMatrix).


/* getbranchinfo(+Puzzle,-Min,-Max)
   Gets branch information : min and maximum x values
*/
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

/* make_empty_matrix(+Width, +Height, -Matrix)
   Makes an empty matrix a certain Width and Height
*/
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

/* printPuzzleMatrix(+Matrix)
   Prints the print matrix
*/
printPuzzleMatrix([]):-nl.
printPuzzleMatrix([Row,Row2|_Matrix]):-
    checkiffinished(Row),
    checkiffinished(Row2),nl. %if there are 2 empty rows in a row, the printing is finished
printPuzzleMatrix([Row|Matrix]):-
    printPuzzleRow(Row),
    printPuzzleMatrix(Matrix).

/* checkiffinished(+Row)
   Checks if a row is empty
*/
checkiffinished([]).
checkiffinished([0|Row]):-
    checkiffinished(Row).

/* printPuzzleRowLine(+Row)
   Prints a horizontal line
*/
printPuzzleRowLine(['-'|Row]):-
    write('----+'),
    printPuzzleRowLine(Row).
printPuzzleRowLine(Row):-
    write('  '),
    printPuzzleRow(Row).

/* printPuzzleRow(+Row)
   Prints elemets from the row
*/
printPuzzleRow([]):-nl.
printPuzzleRow([0|Row]):-
    write('     '),
    printPuzzleRow(Row).

printPuzzleRow(['-'|Row]):-
    write('  +'),
    printPuzzleRowLine(Row).
printPuzzleRow(['|'|Row]):-
    write('  |  '),
    printPuzzleRow(Row).
printPuzzleRow(['['|Row]):-
    write('  ^  '),
    printPuzzleRow(Row).
printPuzzleRow([']'|Row]):-
    write('  v  '),
    printPuzzleRow(Row).
printPuzzleRow(['l'|Row]):-
    write(' _|_ '),
    printPuzzleRow(Row).
printPuzzleRow(['?'|Row]):-
    write('| ? |'),
    printPuzzleRow(Row).
printPuzzleRow([Elem|Row]):-
    write('| '),displayNum(Elem),write('|'),
    printPuzzleRow(Row).


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

print_matrix([]).
print_matrix([H|T]) :- write(H), nl, print_matrix(T).

displayNum(V):- %if the number has 2 digits doesnt print the space so it can fit
    V > 9 , write(V).
displayNum(V):-
    write(V),write(' ').