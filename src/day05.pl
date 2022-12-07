:- module(day05, [day05/2]).

:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

container(X) -->
    "[", [X], { code_type(X, alpha) }, "]".
container(0) -->
    "   ".

row([]) -->
    [].
row([X]) -->
    container(X).
row([X | Xs]) -->
    container(X), " ", row(Xs).

rows([]) -->
    [].
rows([R]) -->
    row(R).
rows([R | Rs]) -->
    row(R), "\n", rows(Rs).

state(State) -->
    rows(Rs),
    {   transpose(Rs, Cs),
        maplist(exclude(=(0)), Cs, State)
    }.

move(move(Quantity, From, To)) -->
    "move ", integer(Quantity), " from ", integer(From), " to ", integer(To).

moves([]) -->
    [].
moves([Move]) -->
    move(Move).
moves([Move | Moves]) -->
    move(Move), "\n", moves(Moves).

head([H | _], H).

state_moves(State, Moves) -->
    state(State),
    string_without("\n", _), "\n",
    string_without("\n", _), "\n",
    moves(Moves).

state_moves_finalState_crateMover(State, [], State, _).
state_moves_finalState_crateMover(State, [Move | Moves], FinalState, CrateMover) :-
    state_move_nextState_crateMover(State, Move, NextState, CrateMover),
    state_moves_finalState_crateMover(NextState, Moves, FinalState, CrateMover).

state_move_nextState_crateMover(State, Move, NextState, CrateMover) :-
    findall(Index, nth1(Index, State, _), Indices),
    maplist(move_state_crateMover_index_stack_nextStack(Move, State, CrateMover), Indices, State, NextState).
move_state_crateMover_index_stack_nextStack(move(Quantity, From, To), _, _, From, Stack, NextStack) :-
    dif(From, To),
    append(Removed, NextStack, Stack),
    length(Removed, Quantity).
move_state_crateMover_index_stack_nextStack(move(Quantity, From, To), State, CrateMover, To, Stack, NextStack) :-
    dif(To, From),
    nth1(From, State, FromStack),
    append(ToMove, _, FromStack),
    length(ToMove, Quantity),
    crateMover_toMove_moved(CrateMover, ToMove, Moved),
    append(Moved, Stack, NextStack).
move_state_crateMover_index_stack_nextStack(move(_, From, To), _, _, Index, Stack, Stack) :-
    dif(Index, From),
    dif(Index, To).

crateMover_toMove_moved(9000, ToMove, Moved) :-
    reverse(ToMove, Moved).
crateMover_toMove_moved(9001, ToMove, ToMove).

day05(Part1, Part2) :-
    phrase_from_file(state_moves(State, Moves), "./src/day05.txt"),
    part1(State, Moves, Part1),
    part2(State, Moves, Part2).

part1(State, Moves, Result) :-
    state_moves_finalState_crateMover(State, Moves, FinalState, 9000),
    maplist(head, FinalState, Chars),
    string_chars(Result, Chars).

part2(State, Moves, Result) :-
    state_moves_finalState_crateMover(State, Moves, FinalState, 9001),
    maplist(head, FinalState, Chars),
    string_chars(Result, Chars).
