:-use_module(library(lists)).
:-use_module(library(clpfd)).
:-consult('trees.pl').

start(Tree, Weight):-
    treeWeightsList(Tree, Weights),
    length(Weights, Length),
    domain(Weights,1,Length),
    all_distinct(Weights),
    solveTree(Tree, Weight),!,
    labeling([], Weights),
    write('Weights: '),
    write(Weights),
    nl.

solveTree(Tree, Weight):-
    getTorques(Tree, PosTorque, NegTorque),
    PosTorque #= NegTorque,
    treeWeight(Tree, Weight).

treeWeight([], 0).
treeWeight([(_|Subtree)|Rest], Weight):-
    is_list(Subtree),
    treeWeight(Subtree, SubtreeWeight),
    NewWeight #= Weight - SubtreeWeight,
    treeWeight(Rest, NewWeight).
treeWeight([(_|Node)|Rest], Weight):-
    (\+is_list(Node)),
    NewWeight #= Weight - Node,
    treeWeight(Rest, NewWeight).


treeWeightsList([], []).
treeWeightsList([(_|Subtree)|Rest], TotalWeights):-
    is_list(Subtree),
    treeWeightsList(Subtree, SubtreeWeights),
    append(SubtreeWeights, Weights, TotalWeights),
    treeWeightsList(Rest, Weights).
treeWeightsList([(_|Node)|Rest], [Node|RestWeights]):-
    (\+is_list(Node)),
    treeWeightsList(Rest, RestWeights).

getTorques([], 0, 0).
getTorques([(Pos|Node)|Rest], PosTorque, NegTorque):-
    Pos>0,
    getNodeTorque(Pos, Node, NodeTorque),
    NewPosTorque #= PosTorque - NodeTorque,
    getTorques(Rest, NewPosTorque, NegTorque).
getTorques([(Pos|Node)|Rest], PosTorque, NegTorque):-
    Pos<0,
    getNodeTorque(Pos, Node, NodeTorque),
    NewNegTorque #= NegTorque - NodeTorque,
    getTorques(Rest, PosTorque, NewNegTorque).

getNodeTorque(Pos, Value, Torque):-
    (\+is_list(Value)),
    Torque #= abs(Pos) * Value.
getNodeTorque(Pos, Subtree, Torque):-
    is_list(Subtree),
    solveTree(Subtree, SubtreeWeight),
    Torque #= abs(Pos) * SubtreeWeight.