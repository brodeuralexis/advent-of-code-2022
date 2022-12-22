:- module(day08, [day08/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(thread)).

trees(Trees) -->
    treeRows(Trees, 1).

treeRows([], _) -->
    [].
treeRows(Trees, Y) -->
    treeRow(Trees, Y).
treeRows(Trees, Y) -->
    treeRow(Trees0, Y), eol, treeRows(Trees1, Y1),
    { Y1 #= Y + 1,
      append(Trees0, Trees1, Trees)
    }.

treeRow(Trees, Y) -->
    digits(Codes),
    {   maplist(plus(-48), Codes, Trees0),
        length(Codes, Length),
        findall(Index, between(1, Length, Index), Indices),
        maplist(treeRows_(Y), Indices, Trees0, Trees)
    }.
treeRows_(Y, X, Tree0, X-Y-Tree0).

trees_visiblePosition(Trees, X-Y) :-
    member(X-Y-Tree, Trees),
    % Cut, because if one is true, the rest do not matter.
    % If the tree is visible from the top, then it does not matter if it is or
    % isn't visible from the left, right, or bottom.
    (   forall((member(X-Y0-A, Trees), Y0 #< Y), Tree #> A), !
    ;   forall((member(X0-Y-B, Trees), X0 #< X), Tree #> B), !
    ;   forall((member(X1-Y-C, Trees), X1 #> X), Tree #> C), !
    ;   forall((member(X-Y1-D, Trees), Y1 #> Y), Tree #> D), !
    ).

trees_visibleTrees(Trees, VisibleTrees) :-
    findall(X-Y-Tree, (member(X-Y-Tree, Trees), trees_visiblePosition(Trees, X-Y)), VisibleTrees).

trees_tree_score(Trees, X-Y-Tree, Score) :-
    member(X-Y-Tree, Trees),
    findall(A0, (member(X-Y0-A0, Trees), Y0 #< Y), T0),
    reverse(T0, T),
    tree_scene_score(Tree, T, S1),
    findall(B0, (member(X0-Y-B0, Trees), X0 #< X), L0),
    reverse(L0, L),
    tree_scene_score(Tree, L, S2),
    findall(D0, (member(X2-Y-D0, Trees), X #< X2), R),
    tree_scene_score(Tree, R, S3),
    findall(E0, (member(X-Y2-E0, Trees), Y #< Y2), D),
    tree_scene_score(Tree, D, S4),
    Score #= S1 * S2 * S3 * S4.

trees_width_height(Trees, Width, Height) :-
    findall(X, member(X-_-_, Trees), Xs),
    findall(Y, member(_-Y-_, Trees), Ys),
    max_list(Xs, Width),
    max_list(Ys, Height).

trees_scores(Trees, Scores) :-
    maplist(trees_tree_score(Trees), Trees, Scores).

tree_scene_score(Tree, Scene, Score) :-
    tree_scene_score_(Tree, -1, Scene, Score).

tree_scene_score_(_, _, [], 0).
tree_scene_score_(Tree, _, [Next | _], 1) :-
    Next #>= Tree.
tree_scene_score_(Tree, Last, [Next | Scene], Score) :-
    (   Next #>= Last
    ->  Score #= Score0 + 1,
        tree_scene_score_(Tree, Next, Scene, Score0)
    ;   tree_scene_score_(Tree, Last, Scene, Score)
    ).

day08(Part1, Part2) :-
    phrase_from_file(trees(Trees), './src/day08.txt'),

    part1(Trees, Part1),
    part2(Trees, Part2).

part1(Trees, Result) :-
    trees_visibleTrees(Trees, VisibleTrees),
    length(VisibleTrees, Result).

part2(Trees, Result) :-
    trees_scores(Trees, Scores),
    max_list(Scores, Result).
