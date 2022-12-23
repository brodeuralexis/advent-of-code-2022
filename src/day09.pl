:- module(day09, [day09/2]).

:- use_module(library(assoc)).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(pio)).

motions([]) -->
    [].
motions([Motion]) -->
    motion(Motion).
motions([Motion | Motions]) -->
    motion(Motion), eol, motions(Motions).

motion(Direction-Distance) -->
    direction(Direction), " ", integer(Distance).

direction(up) --> "U".
direction(down) --> "D".
direction(left) --> "L".
direction(right) --> "R".

motions_directions([], []).
motions_directions([Direction-Distance | Motions], Directions) :-
    length(Directions0, Distance),
    maplist(=(Direction), Directions0),
    append(Directions0, Directions1, Directions),
    motions_directions(Motions, Directions1).

head_direction_next(X-Y0, up, X-Y) :-
    Y #= Y0 + 1.
head_direction_next(X-Y0, down, X-Y) :-
    Y #= Y0 - 1.
head_direction_next(X0-Y, left, X-Y) :-
    X #= X0 - 1.
head_direction_next(X0-Y, right, X-Y) :-
    X #= X0 + 1.

tail_head_next(X-Y, HX-HY, X-Y) :-
    1 #>= abs(X - HX),
    1 #>= abs(Y - HY),
    !.
tail_head_next(X0-Y0, HX-HY, X-Y) :-
    (   X0 #> HX,
        X #= X0 - 1
    ;   X0 #< HX,
        X #= X0 + 1
    ;   X #= HX
    ),
    (   Y0 #> HY,
        Y #= Y0 - 1
    ;   Y0 #< HY,
        Y #= Y0 + 1
    ;   Y #= HY
    ).

knots_direction_next([Head0 | Tails0], Direction, [Head | Tails]) :-
    head_direction_next(Head0, Direction, Head),
    tails_head_next(Tails0, Head, Tails).

tails_head_next([], _, []).
tails_head_next([Tail0 | Tails0], Head, [Tail | Tails]) :-
    tail_head_next(Tail0, Head, Tail),
    tails_head_next(Tails0, Tail, Tails).

knots_directions_visited(_, [], []).
knots_directions_visited(Knots, [Direction | Directions], [TX-TY | Positions]) :-
    knots_direction_next(Knots, Direction, Next),
    last(Next, TX-TY),
    knots_directions_visited(Next, Directions, Positions).

day09(Part1, Part2) :-
    phrase_from_file(motions(Motions), './src/day09.txt'),
    motions_directions(Motions, Directions),
    part1(Directions, Part1),
    part2(Directions, Part2).

part1(Directions, Result) :-
    knots_directions_visited([0-0, 0-0], Directions, Visited),
    sort(Visited, Unique),
    length(Unique, Result).

part2(Directions, Result) :-
    length(Knots, 10),
    maplist(=(0-0), Knots),
    knots_directions_visited(Knots, Directions, Visited),
    sort(Visited, Unique),
    length(Unique, Result).
