:- use_module(library(lists)).

/** ---- Test functions ---- **/

test(List):-
    game(Board-_),
    are_pawns_safe(List, Board).

testplay:-
    game(Game),
    testloop(Game, 1).


testplay2(X-Y,New):-
    board(Board),
    move(X-Y,Board,New),
    display_board(New).
    
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

/* Counts how many elements of certain type are in list
Arguments:
- List
- Element to search for
- Returned number
*/
count_element([], _, 0).
count_element([X | L], X, Res):-
    count_element(L, X, Res1),
    Res is Res1 + 1.
count_element([Y | L], X, Res):-
    X \= Y,
    count_element(L, X, Res).

/* Checks if a list has duplicates
Arguments:
- List
*/
has_dup([X | L]):- member(X, L).
has_dup([_ | L]):- has_dup(L).

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



% Represents current game Board-[P1pawns-P2pawns]

board(Board):- game(Board-_).

game(
[
     [0,0,0,0,0,1,1,0,0,0,0,0],
    [0,0,0,0,0,2,2,1,2,1,0,0],
     [0,0,0,0,1,2,3,3,1,2,3,0],
    [0,0,0,0,0,3,2,3,2,1,3,0],
     [0,0,0,2,1,3,2,2,2,3,0,0],
    [0,0,3,3,3,1,1,2,2,2,0,0],
     [0,0,3,0,2,1,1,3,3,1,0,0],
    [0,0,0,3,1,3,3,2,2,2,2,0],
     [0,0,0,3,1,3,3,1,1,3,0,0],
    [0,0,0,0,1,2,1,1,2,3,0,0],
     [0,0,0,0,0,0,0,0,1,0,0,0]
]-[[],[]]).    

/** ---- Gameplay ---- **/

/* Gets board height
Arguments:
- Board
- Returned height
*/
get_board_height(Board, Height):- length(Board, Height).

/* Gets board width
Arguments:
- Board
- Returned width
*/
get_board_width(Board, Width):- nth1(1, Board, Row), length(Row, Width).

/* Gets pawn on specified coordinates on a board
Arguments:
- X-Y: Coordinates
- Current board,
- Returned Pawn
*/
get_pawn_at(X-Y, Board, Pawn):-
    get_board_height(Board, Height),
    get_board_width(Board, Width),
    Y > 0, X > 0, Y =< Height, X =< Width, !,
    nth1(Y, Board, Row),
    nth1(X, Row, Pawn).
get_pawn_at(_-_, _, 0).

/* Gets pawn on specified coordinate list on a board
Arguments:
- List of coordinates
- Current board,
- Returned Pawn list
*/
get_pawns_at([], _, []).
get_pawns_at([X-Y | CoordList], Board, [Pawn | Pawns]):-
    get_pawn_at(X-Y, Board, Pawn),
    get_pawns_at(CoordList, Board, Pawns).

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
- 1 or 2: Player who'll receive the2 pawn
- Pawn harvests
- Returned pawn harvests (with the added pawn)
*/
add_pawn_to_player(TakenPawn, 1, [P1pawns, P2pawns], [NewP1pawns, P2pawns]):-
    append(P1pawns, [TakenPawn], NewP1pawns).

add_pawn_to_player(TakenPawn, 2, [P1pawns, P2pawns], [P1pawns, NewP2pawns]):-
    append(P2pawns, [TakenPawn], NewP2pawns).

/* Checks if pawns at specified coord list are safe
Arguments:
- Coordinate list
- Current board
*/
are_pawns_safe([], _Board).
are_pawns_safe([X-Y | Coords], Board):-
    get_pawn_at(X-Y, Board, 0), !, % If no pawn in location, no reason to worry
    are_pawns_safe(Coords, Board).
are_pawns_safe([X-Y | Coords], Board):-
    get_pawn_at(X-Y, Board, Pawn), % If there's a pawn...
    get_pawns_around(X-Y, Board, Pawns),
    safe_pawn_list(Pawn, Pawns),
    are_pawns_safe(Coords, Board).

/* Gets the (up to) 6 pawns around a location 
Arguments:
- X-Y: coordinates
- Current board
- Returned pawn list
*/
get_pawns_around(X-Y, Board, Pawns):-
    get_coords_around(X-Y, CoordList),
    get_pawns_at(CoordList, Board, AllPawns),
    delete(AllPawns, 0, Pawns). % deletes empty cells

/* Gets the 6 coordinates around a position
Arguments:
- X-Y: position
- Current board
- Returned list [Left,Right,BotLeft,TopLeft,BotRight,TopRight]
*/
get_coords_around(X-Y, CoordList):-
    Y mod 2 =:= 0, % if Y is even
    Xdec is X - 1,
    Xinc is X + 1,
    Yinc is Y + 1,
    Ydec is Y - 1,
    CoordList = [Xdec-Y,Xinc-Y,Xdec-Ydec,Xdec-Yinc,X-Ydec,X-Yinc].

get_coords_around(X-Y, CoordList):-
    Y mod 2 =:= 1, % if Y is odd
    Xdec is X - 1,
    Xinc is X + 1,
    Yinc is Y + 1,
    Ydec is Y - 1,
    CoordList = [Xdec-Y,Xinc-Y,X-Ydec,X-Yinc,Xinc-Ydec,Xinc-Yinc].
 
/* Checks if a pawn list is enough to protect a pawn (2 equals or 3 total)
Arguments:
- Pawn list to check
*/
safe_pawn_list(_Pawn, Pawns):- length(Pawns, N), N >= 3, !.
safe_pawn_list(Pawn, [Pawn, Pawn]).

/*
game2(
[
     [0,0,0,3,0,0,0],
    [0,0,0,3,3,3,3],
     [0,0,3,3,2,3,3]
]-[[1,1,1,1,1,2,2,2,2,3,3,3,3,3],[]]).

ttest(X):- 
    game2(Board-_Pawns),
    game_over(Board, X).
*/

/* Checks if the game has ended, either by one person winning or having no valid moves
Arguments:
- Game
- Winning player or draw (0) in case there are no valid moves
*/
game_over(_Board-[P1pawns, _P2pawns],1):-
    check_winner(P1pawns).

game_over(_Board-[_P1pawns, P2pawns],2):-
    check_winner(P2pawns).

game_over(Board-_Pawns, 0):-
    valid_moves(Board,_,[]).

/* Checks if given pawns are enough to win
Arguments:
- Pawns to check
*/
check_winner(Pawns):-
    count_element(Pawns, 1, Res1), Res1 > 4,
    count_element(Pawns, 2, Res2), Res2 > 4,
    count_element(Pawns, 3, Res3), Res3 > 4.

/* Makes a move if valid
Arguments:
- X-Y: move (coordinates)
- Current board
- Returned board, with move done
*/
move(X-Y, Board, NewBoard):-
    valid_move(X-Y, Board),
    remove_pawn_at(X-Y,Board, NewBoard).

/* Checks if a move is valid
Arguments:
- X-Y: move (coordinates)
- Current board
*/
valid_move(X-Y, Board):-
    \+get_pawn_at(X-Y, Board, 0),
    remove_pawn_at(X-Y, Board, NewBoard),
    get_coords_around(X-Y, CoordList),
    are_pawns_safe(CoordList, NewBoard).
    
/* Gets a list of valid moves
Arguments:
- Current board
- Returned list of moves
*/
valid_moves(Board, _Player, ListOfMoves):-
    get_valid_moves(Board, 1-1, ListOfMoves).

/* Gets a list of valid moves, starting from a specified position
Arguments:
- Current board:
- Current position to check
- Returned list of moves
*/
get_valid_moves(_, 0-0, []):- !.
get_valid_moves(Board, X-Y, [X-Y | ListOfMoves]):-
    valid_move(X-Y, Board), !, % If valid move
    write(X-Y), nl,
    next_cell(Board, X-Y, NextX-NextY),
    get_valid_moves(Board, NextX-NextY, ListOfMoves).

get_valid_moves(Board, X-Y, ListOfMoves):-
    next_cell(Board, X-Y, NextX-NextY), % If not valid move
    get_valid_moves(Board, NextX-NextY, ListOfMoves).


/* Moves right or down the board, retrieving the next cell
Arguments:
- Board
- X-Y: Coordinates
- X-Y: Returned next coordinates
*/
next_cell(_, X-_, 0-0):- X =< 0, !.
next_cell(_, _-Y, 0-0):- Y =< 0, !.
next_cell(Board, X-Y, NextX-Y):-
    get_board_width(Board, Width),
    get_board_height(Board, Height),
    X < Width, Y =< Height, !, % If (X < width && y <= height) 
    NextX is X + 1.            %     return (x+1, y)

next_cell(Board, _-Y, 1-NextY):-
    get_board_height(Board, Height),
    Y < Height, !, % Else if (Y < height) 
    NextY is Y + 1. %acabaar
next_cell(_, _-_, 0-0).


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
