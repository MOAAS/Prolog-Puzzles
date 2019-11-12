:- use_module(library(lists)).
:- use_module(library(random)).

/** ---- Test functions ---- **/

standardGame(
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

testGame1(
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
]-[[1,1,1,1,3,3,3,2,2,2,2,3,3,3,1,1],[1,1,1,1,3,2,2,2,2,2,3,3,3,1,1]]).    

testGameFlood(
[
     [0,0,0,0,0,1,1,0,0,0,0,0],
    [0,0,0,0,0,2,2,1,2,1,0,0],
     [0,0,0,0,1,2,3,3,1,2,3,0],
    [0,0,0,0,0,3,2,3,2,1,3,0],
     [0,0,0,2,1,3,2,2,2,0,0,0],
    [0,0,3,0,3,1,1,3,3,3,0,0],
     [0,3,3,0,2,1,3,3,3,3,0,0],
    [0,0,0,3,0,0,0,0,0,0,3,0],
     [0,0,0,3,3,3,3,3,3,3,3,0]
]-[[],[]]).    

testmustprint74:-
    testGame1(Game),
    choose_move(Game, 1, 1, X),
    write(X).

testmustprint73:-
    testGame1(Game),
    choose_move(Game, 2, 1, X),
    write(X).

testFloodMustPrintNo:-
    testGameFlood(Board-_Pawns),
    valid_move(10-7, Board).

testFloodMustPrintYes:-
    testGameFlood(Board-_Pawns),
    valid_move(9-7, Board).

disp:-
    testGame1(Game),
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


/* teste ultra fixe mt bom */
ttest:-
    make_board(12,11,FreshBoard),
    get_random_center(12,11,X-Y),
    insert_pieces(FreshBoard,FinalBoard,X-Y,25-25-25),
    display_board(FinalBoard).


/* Makes a board filled with 0's
Arguments:
- Width
- Height
- Returned board
*/
make_board(_,0,[]).    
make_board(Width, Height, [Row|Board]):-
    make_row(Width, Row),
    Height1 is Height - 1,
    make_board(Width, Height1, Board).

/* Makes a row filled with 0's
Arguments:
- Width
- Returned row
*/
make_row(0, []). 
make_row(Width, [0 | Row]):-
    Width1 is Width - 1,
    make_row(Width1,Row).  


/* Inserts pieces in the board in order to make a random island of pieces starting at a center
Arguments:
- Empty Board 
- Returned Board
- XC-YC coordinates of the starting point(center) of the island
- R-G-B number of pieces for each color(Reds,Greens,Blues)
*/
insert_pieces(Board,FinalBoard,XC-YC,R-G-B):-
    place_random_pawn(Board,NewBoard,XC-YC,R-G-B,NR-NG-NB),                   %inserts the center piece
    get_coords_around(XC-YC,AroundCenter),                                    %gets coords around it
    insert_pieces_loop(NewBoard,FinalBoard,AroundCenter,[XC-YC],NR-NG-NB,1).  %goes through the loop to insert the other pieces  


/* Inserts pieces in the board in order to make a random island of pieces starting at a center
   Each iteration places random pieces in the border of the previous iteration. When there are no pieces to make
   a complete border,it places them close to previoes pieces but in a more chaotic manner.
Arguments: 
- Board 
- Returned Board
- PosList List of positions to put pieces in the current iteration
- OldPosList List of positions of the last iteration, used in the last iteration.
- R-G-B number of pieces for each color(Reds,Greens,Blues)
- Currrent Iteration
*/
insert_pieces_loop(Board,Board,_,_,0-0-0,_).                                          %stops when there are no more pieces

insert_pieces_loop(Board,FinalBoard,PosList,OldPosList,R-G-B,Iteration):-
    Total is R+G+B,
    Total > (Iteration*6-1),                                                            %if there are enough pieces letf to complete the border
    place_random_pawns(Board,NewBoard,PosList,R-G-B,RF-GF-BF),                          %places pieces on border positions
    get_coords_around(PosList,ClosePos),                                                %gets coords around them that will make the next border
    delete(ClosePos, PosList, ClosePosClean),                                           %deletes some duplicates
    Iteration1 is Iteration + 1, 
    insert_pieces_loop(NewBoard,FinalBoard,ClosePosClean,PosList,RF-GF-BF,Iteration1).   

insert_pieces_loop(Board,FinalBoard,_,PosList,R-G-B,Iteration):-                      %last iteration - there are not enough pieces to complete a border,
    length(PosList,Lng),                                                              %so they are placed randomly close to previous coords in this loop
    random(0,Lng,Rnd),                                                                %until there are no more pieces
    nth0(Rnd,PosList,Elem),                                                             %choses a random coord from the recently put pieces
    get_coords_around(Elem,Aux),                                                        %gets coords around it
    append([Elem],Aux,Aux2),                                                            %adds itself into a list
    place_random_pawns(Board,NewBoard,Aux2,R-G-B,RF-GF-BF),                             %places random pieces from the coord list
    get_coords_around(Aux2,ClosePos),                                                   %gets coords that can be chosen for the next loop
    append(PosList,ClosePos,NewPosList),                                                %adds them to the pool of coords to be chosen
    insert_pieces_loop(NewBoard,FinalBoard,_,NewPosList,RF-GF-BF,Iteration).

/* Places a random pawn on a certain position depending on their availability
Arguments:
- Board 
- Returned Board
- X-Y position of the pawn
- R-G-B number of pieces for each color(Reds,Greens,Blues) before putting the pawn
- Returned R-G-B
*/
place_random_pawn(Board,NewBoard,X-Y,R-G-B,NR-NG-NB):-
     random(1,4,Rnd),                                               %random number 1-3
     place_random_aux(Board,NewBoard,X-Y,R-G-B,NR-NG-NB,Rnd).


place_random_aux(Board,NewBoard,X-Y,0-G-B,NR-NG-NB,1):-            %if there are no pieces left of a certain type
    place_random_aux(Board,NewBoard,X-Y,0-G-B,NR-NG-NB,2).         %it tries to place the next type
place_random_aux(Board,NewBoard,X-Y,R-0-B,NR-NG-NB,2):-
    place_random_aux(Board,NewBoard,X-Y,R-0-B,NR-NG-NB,3).
place_random_aux(Board,NewBoard,X-Y,R-G-0,NR-NG-NB,3):-
    place_random_aux(Board,NewBoard,X-Y,R-G-0,NR-NG-NB,1).

place_random_aux(Board,NewBoard,X-Y,R-G-B,NR-G-B,1):-              %if there are pieces left,it places it on the board
    replace_pawn_at(X-Y,1,Board,NewBoard),                         %and updates the number of pieces of that color
    NR is R - 1.
place_random_aux(Board,NewBoard,X-Y,R-G-B,R-NG-B,2):-
    replace_pawn_at(X-Y,2,Board,NewBoard),
    NG is G - 1.
place_random_aux(Board,NewBoard,X-Y,R-G-B,R-G-NB,3):-
    replace_pawn_at(X-Y,3,Board,NewBoard),
    NB is B - 1.

/* 
Arguments: places random pawns on a list of position if the position is valid
- Board 
- Returned Board
- PosList list of positions to put the random pawns on
- R-G-B number of pieces for each color(Reds,Greens,Blues) before putting pawns
- Returned R-G-B
*/
place_random_pawns(Board,Board,[],R-G-B,R-G-B).                        %if the PosList has no more Coords,stops

place_random_pawns(Board,Board,_,0-0-0,0-0-0).                         %if there are no more pieces,stops

place_random_pawns(Board,FinalBoard,[X-Y|PosList],R-G-B,Rf-Gf-Bf):-
    inbounds(X-Y,Board),                                                    %checks if the Pos is inbounds
    get_pawn_at(X-Y,Board,0),                                               %checks if the coord is still empty
    place_random_pawn(Board,NewBoard,X-Y,R-G-B,Rn-Gn-Bn),                   %places a random pawn in that position
    place_random_pawns(NewBoard,FinalBoard,PosList,Rn-Gn-Bn,Rf-Gf-Bf).
place_random_pawns(Board,FinalBoard,[X-Y|PosList],R-G-B,Rf-Gf-Bf):-     %if the position is out-of-bound or not empty, skips to the next coord
    place_random_pawns(Board,FinalBoard,PosList,R-G-B,Rf-Gf-Bf).


/* Gets a random position for the center fromm 2/5 to 3/5 of each dimension
Arguments:
- Width 
- Heigth
-Returned Position
*/
get_random_center(Width,Height,X-Y):-
    WidthLow is Width*2/5,
    WidthHigh is Width*3/5,
    random(WidthLow,WidthHigh,X1), 
    X is round(X1),                             %X esta entre (2/5*width) e (3/5* width)
    HeightLow is Height*2/5,
    HeightHigh is Height*3/5,
    random(HeightLow,HeightHigh,Y1),            %Y esta entre (2/5*height) e (3/5* height)
    Y is round(Y1).

/** ---- Game loop ---- **/

play:-
    standardGame(Game), 
    read_difficulty(Difficulty),
    read_mode(Mode),
    gameloop(Game, Mode, Difficulty, 1, Winner),
    write_winner(Winner).

write_winner(0):- write('There are no more valid moves. It\'s a tie!').
write_winner(1):- write('Player 1 wins!').
write_winner(2):- write('Player 2 wins!').

/* Runs the game, on the defined Mode and Difficulty
Arguments:
- Current game
- Game mode (0,1,2,3)
- Difficulty / Level (0,1)
- Player turn
- Returned winner
*/
gameloop(Game, _Mode, _Difficulty, _Player, Winner):- game_over(Game, Winner), display_game(Game, 0), !.

gameloop(Game, Mode, Difficulty, Player, Winner):-
    display_game(Game, Player),
    get_move(Game, Mode, Difficulty, Player, Move),
    move(Move, Player, Game, NewGame),    
    next_player(Player, NewPlayer),
    gameloop(NewGame, Mode, Difficulty, NewPlayer, Winner).

/* Gets a move for player, depending on difficulty and move
Arguments:
- Game
- Mode
- Difficulty (Level)
- Player
- Returned move
*/
% Mode 0: Human-Human -> Gets Input
get_move(Game, 0, _Difficulty, _Player, Move):- read_move(Game, Move). 

% Mode 1: Human-PC -> Gets Input for P1, CPU chooses for P2
get_move(Game, 1, _Difficulty, 1, Move):- read_move(Game, Move).
get_move(Game, 1, Difficulty, 2, Move):- choose_move(Game, 2, Difficulty, Move).

% Mode 2: PC-Human -> Gets Input for P2, CPU chooses for P1
get_move(Game, 2, Difficulty, 1, Move):- choose_move(Game, 1, Difficulty, Move).
get_move(Game, 2, _Difficulty, 2, Move):- read_move(Game, Move).

% Mode 3: PC-PC -> CPU chooses
get_move(Game, 3, Difficulty, Player, Move):- choose_move(Game, Player, Difficulty, Move).


/** ---- User input ---- **/    

/* Reads Difficulty from keyboard, repeating until user types number from 0 to 1
Arguments:
- Returned Difficulty
*/
read_difficulty(Difficulty):-
    repeat,
        write('Choose CPU difficulty:'), nl,
        write('- 0: Super Easy'), nl,
        write('- 1: Very Easy'), nl,
        catch(read(Difficulty), _Error, bad_difficulty_format),
        validate_difficulty_format(Difficulty),
    !.

% Validate the difficulty input format
validate_difficulty_format(Difficulty):- integer(Difficulty), Difficulty >= 0, Difficulty =< 1, !.
validate_difficulty_format(_):- bad_difficulty_format.

% Printing error message for read difficulty
bad_difficulty_format:- write('Couldn\'t read difficulty.'), nl, fail.


/* Reads mode from keyboard, repeating until user types number from 0 to 3
Arguments:
- Returned Mode
*/
read_mode(Mode):-
    repeat,
        write('Choose game mode:'), nl,
        write('- 0: Human VS Human'), nl,
        write('- 1: Human VS CPU'), nl,
        write('- 2: CPU VS Human'), nl,
        write('- 3: CPU VS CPU'), nl,
        catch(read(Mode), _Error, bad_mode_format),
        validate_mode_format(Mode),
    !.

% Validate the mode input format
validate_mode_format(Mode):- integer(Mode), Mode >= 0, Mode =< 3, !.
validate_mode_format(_):- bad_mode_format.

% Printing error message for read mode
bad_mode_format:- write('Couldn\'t read mode.'), nl, fail.

/* Reads a move from the keyboard, repeating until the user enters valid coordinates
Arguments:
- Game
- Returned coordinates
*/
read_move(Board-_Pawns, X-Y):-
    repeat,
        write('Type the coordinates in the format X-Y.'), nl,
        catch(read(X-Y), _Error, bad_input_format),
        validate_input_format(X-Y),
        validate_input_move(X-Y, Board),
    !.


% Validate the move input format
validate_input_format(X-Y):- integer(X), integer(Y), !.
validate_input_format(_):- bad_input_format.

% Printing error message for read move
bad_input_format:- write('Couldn\'t read move.'), nl, fail.

% Validate move in board
validate_input_move(X-Y, Board):- valid_move(X-Y, Board), !.
validate_input_move(_, _):- write('Invalid move, pieces around would not be safe'), nl, fail.

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

replace_pawn_at(X-Y, NewPawn, Board, NewBoard):-
    nth1(Y, Board, Row),
    replace(Row, X, NewPawn, NewRow),
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
get_coords_around([],_).
get_coords_around([X-Y|PosList],[A,B,C,D,E,F|ClosePosList]):-
    get_coords_around(X-Y,[A,B,C,D,E,F]),
    get_coords_around(PosList,ClosePosList).

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
    valid_moves(Board, Moves),
    Moves = [].

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
- Player to move
- Current board
- Returned board, with move done
*/
move(X-Y, Player, Board-Pawns, NewBoard-NewPawns):-
    valid_move(X-Y, Board),
    get_pawn_at(X-Y, Board, Pawn),
    add_pawn_to_player(Pawn, Player, Pawns, NewPawns),
    remove_pawn_at(X-Y, Board, NewBoard).


/* Checks if a move is valid
Arguments:
- X-Y: move (coordinates)
- Current board
*/
valid_move(X-Y, Board):-
    \+get_pawn_at(X-Y, Board, 0),
    remove_pawn_at(X-Y, Board, NewBoard),
    get_coords_around(X-Y, CoordList),
    are_pawns_safe(CoordList, NewBoard),
    check_single_section(NewBoard).
    
/* Gets a list of valid moves
Arguments:
- Current board
- Returned list of moves
*/
valid_moves(Board, ListOfMoves):-
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
    next_cell(Board, X-Y, NextX-NextY),
    get_valid_moves(Board, NextX-NextY, ListOfMoves).

get_valid_moves(Board, X-Y, ListOfMoves):-
    next_cell(Board, X-Y, NextX-NextY), % If not valid move
    get_valid_moves(Board, NextX-NextY, ListOfMoves).

/* Checks if a position is inbounds a certain board
Arguments:
- X-Y position
- Board
*/
inbounds(X-Y,Board):-
    get_board_width(Board, Width),
    get_board_height(Board, Height),
    X < Width, Y =< Height.


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

/* Gets value of board for player
Arguments:
- Board
- Player
- Returned value
*/
value(_Board-[P1pawns,_P2pawns], 1, Value):-
    pawn_value(P1pawns, Value).

value(_Board-[_P1pawns,P2pawns], 2, Value):-
    pawn_value(P2pawns, Value).

/* Gets value for pawn list
Arguments:
- Pawn list
- Returned value
*/
pawn_value(Pawns, Value):-
    count_element(Pawns, 1, Num1s),
    count_element(Pawns, 2, Num2s),
    count_element(Pawns, 3, Num3s),
    Value is min(Num1s, 5) + min(Num2s, 5) + min(Num3s, 5).

/* Chooses a move for PC
Arguments:
- Game
- Player to move
- Difficulty (0 / 1)
- Returned move
*/
choose_move(Board-_, _, 0, Move):- % For difficulty 0, picks random move
    valid_moves(Board, Moves), 
    random_member(Move, Moves).

choose_move(Board-Pawns, Player, 1, Move):- % For difficulty 1, picks best move in the current turn
    valid_moves(Board, Moves),
    best_move(Moves, Board-Pawns, Player, Move-_).

/* Chooses best move from list of moves
Arguments:
- Move list
- Game
- Player to move
- BestMove-BestValue: Best move and its value
*/    
best_move([Move], Game, Player, Move-Value):- move_value(Move, Player, Game, Value).
best_move([Move | Moves], Game, Player, BestMove-BestValue):- 
    best_move(Moves, Game, Player, BestMoveRest-BestValueRest), % Gets best move-value of other moves
    move_value(Move, Player, Game, Value), % Gets value of current move
    compare_moves(Move-Value, BestMoveRest-BestValueRest, BestMove-BestValue). % Returns the best of the two moves

/* Compares two moves, given their values
Arguments:
- Move1-Value1: Move 1 and its value
- Move2-Value2: Move 2 and its value
- BestMove-BestValue: Move-Value pair with the best value
*/
compare_moves(Move1-Value1, _Move2-Value2, Move1-Value1):- Value1 >= Value2.
compare_moves(_Move1-Value1, Move2-Value2, Move2-Value2):- Value1 < Value2.

/* Gets the value of a move
Arguments:
- Move
- Game
- Player
- Returned value
*/
move_value(Move, Player, Game, Value):-
    move(Move, Player, Game, NewGame),
    value(NewGame, Player, Value).

/* Returns yes if Board contains no seperate sections (using flood fill)
Arguments:
- Board
*/
check_single_section(Board):-
    get_first_pawn(Board,1-1,Xf-Yf), % Gets a position where a pawn is located
    flood_fill(Board,Xf-Yf,NewBoard), !, % Flood fills
    \+board_contains(NewBoard, 1), % Board must not contain 1, 2 or 3's
    \+board_contains(NewBoard, 2),
    \+board_contains(NewBoard, 3).

/* Returns yes if Board contains specified element
Arguments:
- Board
- Element
*/ 
board_contains([Row | _Board], X):- member(X, Row).
board_contains([_Row | Board],X):-
    board_contains(Board, X).
    
/* Gets the position of the first pawn in the board, starting from a specified pos
Arguments:
- Board
- X-Y: Starting coordinates
- X-Y: Returned coordinates
*/    
get_first_pawn(Board,X-Y,X-Y):-
    \+get_pawn_at(X-Y, Board,0).

get_first_pawn(Board,X-Y,Xf-Yf):-
    next_cell(Board,X-Y,X1-Y1),
    get_first_pawn(Board,X1-Y1,Xf-Yf).

/* Applies flood fill on the board, painting it with -1's
Arguments:
- Board
- X-Y: start coordinates
- Returned board
*/
flood_fill(Board,X-Y,NewBoard):-
    \+get_pawn_at(X-Y, Board,0),
    \+get_pawn_at(X-Y, Board,-1), !,
    replace_pawn_at(X-Y,-1,Board,NewBoard0),

    get_coords_around(X-Y,[A,B,C,D,E,F]),
    flood_fill(NewBoard0, A, NewBoard1),
    flood_fill(NewBoard1, B, NewBoard2),
    flood_fill(NewBoard2, C, NewBoard3),
    flood_fill(NewBoard3, D, NewBoard4),
    flood_fill(NewBoard4, E, NewBoard5),
    flood_fill(NewBoard5, F, NewBoard).

flood_fill(Board,_,Board).

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
    disp_player_turn(Player), !.

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
disp_pawn(-1):- write('P').



    
/*
Displays player turn
Arguments:
- player
*/
disp_player_turn(0):- !.
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
