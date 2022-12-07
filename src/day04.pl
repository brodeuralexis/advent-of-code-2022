:- module(day04, [day04/2]).

:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

assignment(range(Start, End)) -->
    integer(Start), "-", integer(End).

assignment_pair(First-Second) -->
    assignment(First), ",", assignment(Second).

assignment_pairs([]) -->
    [].
assignment_pairs([Pair]) -->
    assignment_pair(Pair).
assignment_pairs([Pair | Pairs]) -->
    assignment_pair(Pair), "\n", assignment_pairs(Pairs).

fullyOverlaps(First-Second) :-
    outer_inner(First, Second);
    outer_inner(Second, First).

outer_inner(range(OuterMin, OuterMax), range(InnerMin, InnerMax)) :-
    InnerMin #>= OuterMin,
    InnerMax #=< OuterMax.

overlaps(range(Min1, Max1)-range(Min2, Max2)) :-
    Max1 #>= Min2,
    Max2 #>= Min1.

day04(Part1, Part2) :-
    phrase_from_file(assignment_pairs(Pairs), "./src/day04.txt"),
    part1(Pairs, Part1),
    part2(Pairs, Part2).

part1(Pairs, Result) :-
    include(fullyOverlaps, Pairs, OverlappingPairs),
    length(OverlappingPairs, Result).

part2(Pairs, Result) :-
    include(overlaps, Pairs, OverlappingPairs),
    length(OverlappingPairs, Result).
