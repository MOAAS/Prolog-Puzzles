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
    disp_game_first(Board, 1 , 1), %disp hex board
    disp_game_last_row(Board),     %completes missing part of last hex row
    disp_player_turn(Player).      %prints player turn info
    


disp_player_turn(Player):- %displays player turn 
    write('Player '), write(Player), write(' turn ').

disp_game([], _OddRow , _). % base case -> when there are no more rows,disp_game finishes here

disp_game_first([Row | Board], 1 , X):- %the first row is different from the rest 
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
    displayRow4(Row,X,1),
    write('|'),
    nl,
    Xnew is X+1,         %increment X
    disp_game(Board, 0 , Xnew). %after this (diferent) row, all the others will be made recursively the same way


disp_game([Row | Board], 1 , X):- %odd numbered row print. Each row needs to print 4 lines to make hexagon patterns
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

    write('    '),     %forth print line
    displayRow4(Row,X,1),
    write('|'),
    nl,

    Xnew is X+1,         %increment X

    disp_game(Board, 0 ,Xnew).%moving over to the next row, alternating between even and odd.


disp_game([Row | Board], 0,X):- %even numbered row print
    displayRow1(Row),%first print line
    write('   /'),
    nl,

    displayRow2(Row),%second print line
    write(' /'),
    nl,

    displayRow3(Row),%third print line
    write('|'),
    nl,

    displayRow4(Row,X,1),%forth print line
    write('|'),
    nl,
    Xnew is X+1,         %increment X
    disp_game(Board, 1 ,Xnew).

displayRow1([]).

displayRow1([_ | Row]):-
    write('   / \\  '),
    displayRow1(Row).

displayRow2([]).

displayRow2([_ | Row]):-
    write(' /     \\'),
    displayRow2(Row).

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


displayRow4([],_,_). 

displayRow4([ A | Row],X,Y):- 
    A = 0, %forth line only prints coordinates if there are pieces in that hex
    !,
    write('|       '), %if there are no pieces, it doesnt show the coords
    Ynew is Y+1,    %increment Y 
    displayRow4(Row, X , Ynew).

displayRow4([ _A | Row],X,Y):- %else , prints the coords
    write('| '),
    displayDynSize(X), %print X
    write(','),
    displayDynSize(Y), %print Y
    write(' '), 
    Ynew is Y+1,        %increment Y 
    displayRow4(Row, X , Ynew). 

displayDynSize(V):- %if the number has 2 digits doesnt print the space so it can fit in the hex
    V > 9 , ! , write(V).
displayDynSize(V):-
    write(V),write(' ').


disp_game_last_row([[_Row | SmallerRow] | _Board]):- %prints the missing characters to complete the last row hexagon pattern
    write('     \\  '),         %first print line
    displayRow1(SmallerRow),    %prints rowsize - 1 iterations 
    write('   /'),
    nl,

    write('       \\'),         %second print line
    displayRow2(SmallerRow),    %only 2 print lines because only need to complete the missing part of the last hexagon row
    write(' /'),        
    nl.







