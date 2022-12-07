:- module(day06, [day06/2]).

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

stream_startOfPacket(Stream, StartOfPacket) :-
    stream_startLength_startIndex(Stream, 4, StartOfPacket).

stream_startOfMessage(Stream, StartOfMessage) :-
    stream_startLength_startIndex(Stream, 14, StartOfMessage).

stream_startLength_startIndex(Stream, Length, StartIndex) :-
    append(BeforeMarker, WithMarker, Stream),
    length(BeforeMarker, MarkerIndex),
    StartIndex #= MarkerIndex + Length,
    append(Marker, _, WithMarker),
    length(Marker, Length),
    is_set(Marker).

day06(Part1, Part2) :-
    phrase_from_file(string(Stream), "./src/day06.txt"),
    part1(Stream, Part1),
    part2(Stream, Part2).

part1(Stream, Part1) :-
    stream_startOfPacket(Stream, Part1).

part2(Stream, Part2) :-
    stream_startOfMessage(Stream, Part2).
