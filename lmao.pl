board([
    [1,0,0,0,0,0,0,0,0,0,0,0],
    [1,0,2,0,0,0,0,0,0,0,0,0],
    [2,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,1,0,0,0,0,3,0,0],
    [0,0,0,0,0,0,0,0,0,0,0,2],
    [0,0,2,0,0,0,0,0,3,0,0,2],
    [0,0,0,0,1,0,0,0,0,0,0,3],
    [0,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0,0,0]
]).

disp:-
    board(X),
    display_game(X, 1).
    

display_game([], OddRow).
display_game([Row | Board], 1):-
    write('   '),
    displayRow(Row),
    display_game(Board, 0).
display_game([Row | Board], 0):-
    displayRow(Row),
    display_game(Board, 1).

displayRow([]):-
    nl.

displayRow([0 | Row]):-
    write('|    |'),
    displayRow(Row).

displayRow([1 | Row]):-
    write('|blue|'),
    displayRow(Row).

displayRow([2 | Row]):-
    write('|yell|'),
    displayRow(Row).

displayRow([3 | Row]):-
    write('|redd|'),
    displayRow(Row).
