% 
board([
     [0,0,0,0,0,1,1,0,0,0,0,0],
    [0,0,0,0,0,2,2,1,2,1,0,0],
     [0,0,0,0,1,2,3,3,1,2,3,0],
    [0,0,0,0,0,3,2,3,2,1,3,0],
     [0,0,0,2,1,3,2,2,2,3,0,0],
    [0,0,0,3,3,1,1,2,2,2,0,0],
     [0,0,3,1,2,1,1,3,3,1,0,0],
    [0,0,0,3,1,3,3,2,2,2,2,0],
     [0,0,0,3,1,3,3,1,1,3,0,0],
    [0,0,0,0,1,2,1,1,2,3,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0]
]).

disp:-
    board(X),
    display_game(X, 1).

display_game(Board, Player):-
    display_board(Board),
    disp_player_turn(Player).      %prints player turn info
    
/*
Displays player turn
Arguments:
- player
*/
disp_player_turn(Player):- 
    write('Player '), write(Player), write(' turn ').

/*
Displays board
Arguments:
- board
*/
display_board([Row | Board]):-
    disp_board_first(Row),            %disp hex board
    disp_board_general(Board, 0 , 2),      %starts from 2nd row
    disp_board_last(Board).         %completes missing part of last hex row

/*
Displays board's first row
Arguments:
- row
*/
disp_board_first(Row):- %the first row is different from the rest 
    write('    '),
    displayRow1(Row),
    nl,

    write('    '),
    displayRow2(Row),
    nl,

    write('    '),
    displayRow3(Row),
    write('|'),
    nl,

    write('    '),
    displayRow4(Row,1,1),
    write('|'),
    nl.

/*
Displays board
Arguments:
- board to be displayed (list of lists)
- 1 if odd row, 0 otherwise
- row number
*/
disp_board_general([], _OddRow , _). % base case -> when there are no more rows,disp_game finishes here

disp_board_general([Row | Board], 1 , X):- %odd numbered row print. Each row needs to print 4 lines to make hexagon patterns
    write(' \\  '),    %first print line
    displayRow1(Row),  %odd numbered rows need to start ahead so they can fit in the pattern
    nl,
    
    write('   \\'),    %second print line
    displayRow2(Row),
    nl,

    write('    '),     %third print line
    displayRow3(Row),
    write('|'),
    nl,

    write('    '),     %fourth print line
    displayRow4(Row, X, 1),
    write('|'),
    nl,

    X1 is X+1,         %increment X

    disp_board_general(Board, 0, X1).%moving over to the next row, alternating between even and odd.


disp_board_general([Row | Board], 0, X):- %even numbered row print
    displayRow1(Row),%first print line
    write('   /'),
    nl,

    displayRow2(Row),%second print line
    write(' /'),
    nl,

    displayRow3(Row),%third print line
    write('|'),
    nl,

    displayRow4(Row, X, 1),%fourth print line
    write('|'),
    nl,
    X1 is X+1,         %increment X
    disp_board_general(Board, 1, X1).

/*
Displays the first print line of a board row
Arguments:
- Row to be displayed
*/
displayRow1([]).
displayRow1([_ | Row]):-
    write('   / \\  '),
    displayRow1(Row).

/*
Displays the second print line of a board row
Arguments:
- Row to be displayed
*/
displayRow2([]).
displayRow2([_ | Row]):-
    write(' /     \\'),
    displayRow2(Row).

/*
Displays the third print line of a board row
Arguments:
- Row to be displayed
*/
displayRow3([]). 

displayRow3([0 | Row]):-
    write('|       '),
    displayRow3(Row).

displayRow3([1 | Row]):-
    write('|  XXX  '),
    displayRow3(Row).

displayRow3([2 | Row]):-
    write('|  ---  '),
    displayRow3(Row).

displayRow3([3 | Row]):-
    write('|  OOO  '),
    displayRow3(Row).

/*
Displays the fourth print line of a board row
Arguments:
- Row to be displayed
*/
displayRow4([],_,_). 

displayRow4([ 0 | Row], X, Y):-  %fourth line only prints coordinates if there are pieces in that hex    
    write('|       '), %if there are no pieces, it doesnt show the coords
    Y1 is Y+1, %increment Y 
    displayRow4(Row, X , Y1).

displayRow4([ _A | Row],X,Y):- %else , prints the coords
    write('| '),
    displayCoord(X), %print X
    write(','),
    displayCoord(Y), %print Y
    write(' '), 
    Y1 is Y+1,        %increment Y 
    displayRow4(Row, X , Y1). 


/**
Dynamic single coordinate printing
Arguments:
- Number of coordinate to print
*/
displayCoord(V):- %if the number has 2 digits doesnt print the space so it can fit in the hex
    V > 9 , write(V).
displayCoord(V):-
    write(V),write(' ').


/*
Completes last row 
Arguments:
- Board
*/
disp_board_last([[_Row | SmallerRow] | _Board]):- %prints the missing characters to complete the last row hexagon pattern
    write('     \\  '),         %first print line
    displayRow1(SmallerRow),    %prints rowsize - 1 iterations 
    write('   /'),
    nl,

    write('       \\'),         %second print line
    displayRow2(SmallerRow),    %only 2 print lines because only need to complete the missing part of the last hexagon row
    write(' /'),        
    nl.







