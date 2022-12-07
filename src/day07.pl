:- module(day07, [day07/2]).

:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(clpfd)).

commands(FileTree) -->
    { empty_assoc(FileTree0) },
    commands(FileTree0, [], FileTree, _).

commands(FileTree, _, FileTree) -->
    [].
commands(FileTree0, Path0, FileTree, Path) -->
    command(FileTree0, Path0, FileTree, Path).
commands(FileTree0, Path0, FileTree, Path) -->
    command(FileTree0, Path0, FileTree1, Path1),
    "\n",
    commands(FileTree1, Path1, FileTree, Path).

command(FileTree0, Path, FileTree, Path) -->
    lsCommand(FileTree0, Path, FileTree).
command(FileTree, Path0, FileTree, Path) -->
    cdCommand(Path0, Path).

cdCommand(Path0, Path) -->
    "$ cd ", string_without("\n", Segment0),
    {   string_chars(Segment, Segment0),
        path_cd_nextPath(Path0, Segment, Path)
    }.

lsCommand(FileTree0, Path, FileTree) -->
    "$ ls\n",
    lsEntries(Items),
    { foldl(lsCommand(Path), Items, FileTree0, FileTree)
    }.
lsCommand(Path, Name0-file(Size), FileTree0, FileTree) :-
    string_chars(Name, Name0),
    fileTree_touch(Name, Path, FileTree0, Size, FileTree).
lsCommand(_, _-dir(_), FileTree, FileTree).

lsEntries([]) -->
    [].
lsEntries([Entry]) -->
    lsEntry(Entry).
lsEntries([Entry | Entries]) -->
    lsEntry(Entry), "\n", lsEntries(Entries).

lsEntry(Name-file(Size)) -->
    integer(Size), " ", string_without("\n", Name).
lsEntry(Name-dir([])) -->
    "dir ", string_without("\n", Name).

path_cd_nextPath(Path, Segment0, NextPath) :-
    string_codes(Segment0, Segment),
    path_cd_nextPath_(Path, Segment, NextPath).
path_cd_nextPath_(_, "/", []).
path_cd_nextPath_(Path, "..", NextPath) :-
    append(NextPath, [_], Path).
path_cd_nextPath_(Path, Segment0, NextPath) :-
    string_codes(Segment, Segment0),
    append(Path, [Segment], NextPath).

% fileTree_touch(Name, Path, FileTree0, Size, FileTree)
fileTree_touch(Name, [], FileTree0, Size, FileTree) :-
    put_assoc(Name, FileTree0, file(Size), FileTree).
fileTree_touch(Name, [Segment | Segments], FileTree0, Size, FileTree) :-
    (   get_assoc(Segment, FileTree0, dir(SubTree0), FileTree, dir(SubTree))
    ->  fileTree_touch(Name, Segments, SubTree0, Size, SubTree)
    ;   empty_assoc(SubTree0),
        fileTree_touch(Name, Segments, SubTree0, Size, SubTree),
        put_assoc(Segment, FileTree0, dir(SubTree), FileTree)
    ).

item_size(file(Size), Size).
item_size(dir(SubTree), Size) :-
    fileTree_size(SubTree, Size).

fileTree_size(FileTree, Size) :-
    assoc_to_values(FileTree, Items),
    maplist(item_size, Items, Sizes),
    sumlist(Sizes, Size).

fileTree_directories_sizes(FileTree, [[] | Directories], [TotalSize | Sizes]) :-
    fileTree_size(FileTree, TotalSize),
    assoc_to_list(FileTree, Items),
    fileTree_directories_sizes_(Items, Directories, Sizes, []).

fileTree_directories_sizes_([], [], [], _).
fileTree_directories_sizes_([Name-dir(SubTree) | Items], [Directory | Directories], [Size | Sizes], Path) :-
    append(Path, [Name], Directory),
    fileTree_size(SubTree, Size),
    assoc_to_list(SubTree, SubItems),
    fileTree_directories_sizes_(SubItems, Directories0, Sizes0, Directory),
    fileTree_directories_sizes_(Items, Directories1, Sizes1, Path),
    append(Directories0, Directories1, Directories),
    append(Sizes0, Sizes1, Sizes).
fileTree_directories_sizes_([_-file(_) | Items], Directories, Sizes, Path) :-
    fileTree_directories_sizes_(Items, Directories, Sizes, Path).

day07(Part1, Part2) :-
    phrase_from_file(commands(FileTree), "./src/day07.txt"),
    part1(FileTree, Part1),
    part2(FileTree, Part2).

part1(FileTree, Result) :-
    fileTree_directories_sizes(FileTree, _, Sizes0),
    exclude(#<(100000), Sizes0, Sizes),
    sumlist(Sizes, Result).

part2(FileTree, Result) :-
    fileTree_directories_sizes(FileTree, _, [RootSize | Sizes0]),
    NeededSpace #= 30000000 - (70000000 - RootSize),
    exclude(#>=(NeededSpace), [RootSize | Sizes0], Sizes),
    msort(Sizes, [Result | _]).
