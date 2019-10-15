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
    
last(X,[X]).
 last(X,[_|Z]) :- last(X,Z).


display_game([], OddRow).
display_game([Row | Board], 1):-
    write('\\      '),
    displayRow1(Row),
    write(' \\____ '),
    displayRow2(Row),
    display_game(Board, 0).
display_game([Row | Board], 0):-
    displayRow1(Row),
    displayRow2(Row),
    display_game(Board, 1).

displayRow1([]):-
    nl.
displayRow2([]):-
    nl.


displayRow1([0 | Row]):-
    write(' /     \\      '),
    displayRow1(Row).
displayRow2([0 | Row]):-
    write('/       \\_____'),
    displayRow2(Row).


displayRow1([1 | Row]):-
    write(' /     \\      '),
    displayRow1(Row).
displayRow2([1 | Row]):-
    write('/  RED  \\_____'),
    displayRow2(Row).

displayRow1([2 | Row]):-
    write(' /     \\      '),
    displayRow1(Row).
displayRow2([2 | Row]):-
    write('/ BLUE  \\_____'),
    displayRow2(Row).


displayRow1([3 | Row]):-
    write(' /     \\      '),
    displayRow1(Row).
displayRow2([3 | Row]):-
    write('/ YELLOW\\_____'),
    displayRow2(Row).


