:-consult('trees.pl').
:-use_module(library(lists)).

%Gets the biggest amount of spacing necessary (for root)
getLeftMostPosition(Tree, LeftMostValue):-
    Tree = [FirstElem|_],
    FirstElem = (Pos|_),
    depthSearch(Tree, 0, Pos, LeftMostValue).

%In case we called depthSearch on a Leaf
depthSearch(NotAList, Tmp, LeftMostTmp, LeftMostValue):-
    \+is_list(NotAList),
    ((Tmp < LeftMostTmp, LeftMostValue = Tmp);
    (Tmp >= LeftMostTmp, LeftMostValue = LeftMostTmp)).
depthSearch([], _, _, _).
%DFS search
depthSearch([FirstElem|Rest], Tmp, LeftMostTmp, LeftMostValue):-
    FirstElem = (Pos|Children),
    NewTmp is Tmp + Pos,
    depthSearch(Children, NewTmp, LeftMostTmp, NewLeftMostTmp),
    depthSearch(Rest, 0, NewLeftMostTmp, LeftMostValue),
    LeftMostValue = NewLeftMostTmp.

%Writes a Char a Total number of times
writeRepeat(_, 0).
writeRepeat(Char, Total):-
    print(Char),
    NewT is Total - 1,
    writeRepeat(Char, NewT).

%Prints The Root of the Tree
printRoot(Total):-
    writeRepeat('     ', Total),
    print('|'), nl.

%Prints a Row of the Tree
printRow([Elem|Rest], TotalSpacing):-
    Elem = (FirstPos|_),
    WS is TotalSpacing + FirstPos,
    writeRepeat('     ', WS),
    print('+'),
    printRowRest(Rest, FirstPos).

printRowRest([], _):- nl.
printRowRest([Elem|Rest], LastPos):-
    Elem = (CurrPos|_),
    Spaces is CurrPos - LastPos - 1,
    writeRepeat('----|', Spaces),
    print('----+'),
    printRowRest(Rest, CurrPos).

%Prints the bottom side of a Row
printRowBottom([Elem|Rest], TotalSpacing):-
    Elem = (CurrPos|Node),
    ((isWeight(Node),
      WS is TotalSpacing + CurrPos - 1,
      writeRepeat('     ', WS),
      print(' _|_ '),
      printRowBottomRest(Rest, CurrPos)
     );
     (WS is TotalSpacing + CurrPos,
     writeRepeat('     ', WS),
     print('|'),
     printRowBottomRest(Rest, CurrPos))).

printRowBottomRest([], _):- nl.
printRowBottomRest([Elem|Rest], LastPos):-
    Elem = (CurrPos|Node),
    ((isWeight(Node),
      Spaces is CurrPos - LastPos - 1,
      writeRepeat('     ', Spaces),
      print('   _|_ '),
      printRowBottomRest(Rest, CurrPos));
     (Spaces is CurrPos - LastPos - 1, 
      writeRepeat('     ', Spaces),
      print('    |'),
      printRowBottomRest(Rest, CurrPos))).

isWeight(Node):- var(Node).
isWeight(Node):- number(Node).

%Main Function to print tree
printTree:-
    complexTree(Tree),
    Margin = 0,
    getLeftMostPosition(Tree, LeftMostValue),
    MaxSpacing is abs(LeftMostValue),
    Total is MaxSpacing + Margin,
    printRoot(Total),
    printRow(Tree, Total),
    printRowBottom(Tree, Total).

% consult('display.pl'), printTree.