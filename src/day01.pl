:- module(day01, [day01/2]).

:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

calories([]) --> eol.
calories([C|Cs]) --> number(C), eol, calories(Cs).

inventories([]) --> eos.
inventories([I|Is]) --> calories(I), inventories(Is).

day01(Part1, Part2) :-
    phrase_from_file(inventories(Is), "./src/day01.txt"),
    part1(Is, Part1),
    part2(Is, Part2).

part1(Is, Result) :-
    maplist(sumlist, Is, Ss),
    max_list(Ss, Result).

part2(Is, Result) :-
    maplist(sumlist, Is, Ss0),
    msort(Ss0, Ss1),
    reverse(Ss1, [S1, S2, S3 | _]),
    Result #= S1 + S2 + S3.
