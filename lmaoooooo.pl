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
    
display_game([], OddRow).
display_game([Row | Board], 1):-
    write(' \\  '),
    displayRow1(Row),
    write('   \\'),
    displayRow2(Row),
    write('    '),
    displayRow3(Row),
    display_game(Board, 0).
display_game([Row | Board], 0):-
    displayRow1(Row),
    displayRow2(Row),
    displayRow3(Row),
    display_game(Board, 1).

displayRow1([]):-
    nl.

displayRow1([X | Row]):-
    write('   / \\  '),
    displayRow1(Row).

displayRow2([]):-
    nl.

displayRow2([X | Row]):-
    write(' /     \\'),
    displayRow2(Row).

 displayRow3([]):-
    nl.   

displayRow3([0 | Row]):-
    write('|       '),
    displayRow3(Row).

displayRow3([1 | Row]):-
    write('|  RED  '),
    displayRow3(Row).

displayRow3([2 | Row]):-
    write('| BLUE  '),
    displayRow3(Row).

displayRow3([3 | Row]):-
    write('|YELLOW '),
    displayRow3(Row).


