:- module(day03, [day03/2]).

:- use_module(library(aggregate)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists)).

items([]) -->
    [].
items([X | Xs]) -->
    [X], { code_type(X, alpha) }, items(Xs).

rucksack(Content) -->
    items(Content).

rucksacks([]) -->
    [].
rucksacks([Rucksack]) -->
    rucksack(Rucksack).
rucksacks([Rucksack | Rucksacks]) -->
    rucksack(Rucksack), "\n", rucksacks(Rucksacks).

:- foreach(between(0'a, 0'z, Code), assertz(
                                       item_priority(Code, Priority) :-
                                           Priority #= Code - 97 + 1
                                   )).
:- foreach(between(0'A, 0'Z, Code), assertz(
                                      item_priority(Code, Priority) :-
                                          Priority #= Code - 65 + 27
                                  )).

chunk([], [], _).
chunk(List, [Chunk | Chunks], N) :-
    append(Chunk, Rest, List),
    length(Chunk, N),
    chunk(Rest, Chunks, N).

rucksack_compartment(Rucksack, First-Second) :-
    append(First, Second, Rucksack),
    length(First, N),
    length(Second, N).

compartment_error(First-Second, Repeated) :-
    member(Repeated, First),
    member(Repeated, Second).

group_badge(Rucksacks, Badge) :-
    maplist(member(Badge), Rucksacks).

day03(Part1, Part2) :-
    phrase_from_file(rucksacks(Rucksacks), "./src/day03.txt"),
    part1(Rucksacks, Part1),
    part2(Rucksacks, Part2).

part1(Rucksacks, Result) :-
    maplist(rucksack_compartment, Rucksacks, Compartments),
    maplist(compartment_error, Compartments, Errors),
    maplist(item_priority, Errors, Priorities),
    sumlist(Priorities, Result).

part2(Rucksacks, Result) :-
    chunk(Rucksacks, Groups, 3),
    maplist(group_badge, Groups, Badges),
    maplist(item_priority, Badges, Priorities),
    sumlist(Priorities, Result).
