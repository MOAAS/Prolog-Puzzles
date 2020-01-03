/* Do site */

puzzleDefault([
    weight(-3, _),
    weight(-1, _),
    branch(2, [
        weight(-2, _),
        weight(-1, _),
        weight(1, _)
    ])
]).

puzzle6([
    weight(-3,_),
    weight(-2,_),
    branch(1,[
        branch(-1,[
            weight(-3,_),
            weight(1,_)
        ]),
        weight(1,_)
    ]),
    weight(2,_)
]).

puzzle8([
    branch(-1, [
        branch(-1, [
            weight(-2, _),
            weight(-1, _),
            weight(1, _)
        ]),
        weight(2, _)
    ]),
    branch(1, [
        weight(-3, _),
        weight(-2, _),
        weight(-1, _),
        weight(2, _)
    ])
]).

puzzle17([
    weight(-3,_),
    weight(-2,_),
    branch(-1,[
        branch(-1,[
            weight(-1,_),
            weight(1,_),
            weight(2,_)
        ]),
        branch(1,[
            branch(-1,[
                weight(-2,_),
                branch(1,[
                    weight(-1,_),
                    weight(2,_)
                ])
            ]),
            weight(2,_)
        ]),
        weight(2,_)
    ]),
    branch(1,[
        branch(-2,[
            weight(-1,_),
            weight(2,_)
        ]),
        weight(1,_),
        branch(2,[
            weight(-3,_),
            weight(1,_)
        ])
    ]),
    weight(2,_),
    weight(3,_)
]).

puzzle20([
    branch(-4, [
        weight(-3,_),
        weight(-2,_),
        weight(3,_)
    ]),
    branch(-2, [
        weight(-2,_),
        weight(-1,_),
        branch(1, [
            branch(-1, [
                weight(-1,_),
                weight(2,_),
                weight(3,_)
            ]),
            weight(1,_),
            weight(2,_)
        ])
    ]),
    weight(1,_),
    branch(3, [
        branch(-1, [
            branch(-1, [
                weight(-1,_),
                branch(1, [
                    weight(-2,_),
                    weight(1,6)
                ]),
                weight(2,_),
                weight(3,_)
            ]),
            weight(1,_),
            weight(2,_)
        ]),
        weight(1,_),
        weight(2,_)
    ])
]).

/* Nao do site */
puzzle6s([
    weight(-3,3),
    weight(-2,1),
    branch(1,[
        branch(-1,[
            weight(-3,5),
            weight(1,2)
        ]),
        weight(1,6)
    ]),
    weight(2,4)
]).

puzzledomoas([
    weight(-1,8),
    branch(1,[
        branch(3,[
            weight(-4,1),
            weight(2,2),
            weight(-2,6),
            weight(4,3)
            ]),
        weight(-2,4),
        weight(-4,7)
        ]),
    weight(-3,5)
]).

puzzleb([
    branch(-3,[
        weight(-1,2),
        weight(-4,1),
        weight(1,6)
    ]),
    branch(1,[
        weight(3,3),
        weight(-2,7),
        weight(1,5)
    ]),
    weight(2,8),
    weight(-1,4)
]).

puzzlec([
    branch(3,[
        weight(4,1),
        weight(-4,5)
    ]),
     branch(-4,[
        weight(-1,1),
        weight(2,5)
    ])
]).

puzzled([
    branch(-1,[
        weight(-3,1),
        weight(3,5),
        branch(-2,[
            weight(-3,1),
            weight(3,5)
        ])
    ]),
     branch(-3,[
        weight(-3,1),
        weight(3,5)
    ]),
    branch(6,[
        weight(-1,1),
        weight(1,5)
    ])
]).
