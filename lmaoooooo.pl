:- use_module(library(lists)).

/** ---- Test functions ---- **/

test(X-Y):-
    board(Board),
    remove_pawn_at(X-Y, Board, NewBoard),
    display_board(NewBoard).

testplay:-
    game(Game),
    testloop(Game, 1).


testloop(Board-Pawns, Player):-
    display_game(Board-Pawns, Player),
    read(X-Y),
    get_pawn_at(X-Y, Board, TakenPawn),
    remove_pawn_at(X-Y, Board, NewBoard),
    add_pawn_to_player(TakenPawn, Player, Pawns, NewPawns),
    next_player(Player, NewPlayer),
    testloop(NewBoard-NewPawns, NewPlayer).


disp:-
    game(Game),
    display_game(Game, 1).

/** ---- Utility functions ---- **/

/* Replaces an element from a list
Arguments:
- List
- Index to replace
- New element
- Returned new list
*/
replace([_|T], 1, X, [X|T]).
replace([H | T], I, X, [H | R]):-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).
    

/** ---- Game representation ---- **/

% Represents current game Board-P1pawns-P2pawns
game(
[
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
]-[[],[]]).

/** ---- Gameplay ---- **/


/* Gets pawn on specified coordinates on a board
Arguments:
- X-Y: Coordinates
- Current board,
- Returned Pawn
*/
get_pawn_at(X-Y, Board, Pawn):-
    nth1(Y, Board, Row),
    nth1(X, Row, Pawn).

/* Removes pawn on specified coordinates
Arguments:
- X-Y: Coordinates
- Current board,
- Returned board, with removed pawn
*/
remove_pawn_at(X-Y, Board, NewBoard):-
    nth1(Y, Board, Row),
    replace(Row, X, 0, NewRow),
    replace(Board, Y, NewRow, NewBoard).

% Gets next player (1->2, 2->1)
next_player(1, 2).
next_player(2, 1).

/* Adds a pawn to a player's harvest
Arguments:
- Pawn to be added,
- 1 or 2: Player who'll receive the pawn
- Pawn harvests
- Returned pawn harvests (with the added pawn)
*/
add_pawn_to_player(TakenPawn, 1, [P1pawns, P2pawns], [NewP1pawns, P2pawns]):-
    append(P1pawns, [TakenPawn], NewP1pawns).

add_pawn_to_player(TakenPawn, 2, [P1pawns, P2pawns], [P1pawns, NewP2pawns]):-
    append(P2pawns, [TakenPawn], NewP2pawns).

% Toodooo

%move(Move, Board, NewBoard). 

%valid_move(X-Y, Board).

%valid_moves(Board, Player, ListOfMoves).

/** ---- Game display ---- **/

/* Displays state of the game
Arguments:
- Current game state
- Player whose turn it is to play
*/
display_game(Board-[P1pawns, P2pawns], Player):-
    display_board(Board),
    disp_p1_pawns(P1pawns),
    disp_p2_pawns(P2pawns),
    disp_player_turn(Player).   

% Displays player 1's pawns
disp_p1_pawns(Pawns):-
    write('P1 pawns: '),
    disp_pawns(Pawns),
    nl.

% Displays player 2's pawns
disp_p2_pawns(Pawns):-
    write('P2 pawns: '),
    disp_pawns(Pawns),
    nl.

/*
Displays a list of pawns
Arguments:
- pawn list
*/
disp_pawns([]).
disp_pawns([Pawn | Pawns]):- 
    disp_pawn(Pawn),
    write(' '),
    disp_pawns(Pawns).

% Displays a single pawn
disp_pawn(0):- write(' ').
disp_pawn(1):- write('O').
disp_pawn(2):- write('X').
disp_pawn(3):- write('T').



    
/*
Displays player turn
Arguments:
- player
*/
disp_player_turn(Player):- 
    write('Player '), write(Player), write(' turn '), nl.

/*
Displays board
Arguments:
- board
*/
display_board([Row | Board]):-
    disp_board_first(Row),            %disp hex board
    disp_board_general(Board, 0, 2),      %starts from 2nd row
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

disp_board_general([Row | Board], 1 , Y):- %odd numbered row print. Each row needs to print 4 lines to make hexagon patterns
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
    displayRow4(Row, 1, Y),
    write('|'),
    nl,

    Y1 is Y+1,         %increment Y

    disp_board_general(Board, 0, Y1).%moving over to the next row, alternating between even and odd.


disp_board_general([Row | Board], 0, Y):- %even numbered row print
    displayRow1(Row),%first print line
    write('   /'),
    nl,

    displayRow2(Row),%second print line
    write(' /'),
    nl,

    displayRow3(Row),%third print line
    write('|'),
    nl,

    displayRow4(Row, 1, Y),%fourth print line
    write('|'),
    nl,
    Y1 is Y+1,         %increment X
    disp_board_general(Board, 1, Y1).

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
Displays the third print line of a board row (contains the cell)
Arguments:
- Row to be displayed
*/
displayRow3([]). 
displayRow3([Cell | Row]):-
    write('|   '),
    disp_pawn(Cell),
    write('   '),
    displayRow3(Row).

/*
Displays the fourth print line of a board row (contains the coords)
Arguments:
- Row to be displayed
*/
displayRow4([],_,_). 

displayRow4([ 0 | Row], X, Y):-  %fourth line only prints coordinates if there are pieces in that hex    
    write('|       '), %if there are no pieces, it doesnt show the coords
    X1 is X+1, %increment X 
    displayRow4(Row, X1, Y).

displayRow4([ _A | Row],X,Y):- %else , prints the coords
    write('| '),
    displayCoord(X), %print X
    write(','),
    displayCoord(Y), %print Y
    write(' '), 
    X1 is X+1,        %increment Y 
    displayRow4(Row, X1, Y). 


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
- Full Board
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
