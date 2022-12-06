:- module(day02, [day02/2]).

strategy([]) --> [].
strategy([Them-Us]) -->
    them(Them), " ", us(Us).
strategy([Them-Us | Rest]) -->
    them(Them), " ", us(Us), "\n", strategy(Rest).

them(rock) --> "A".
them(paper) --> "B".
them(scissor) --> "C".

us(rock) --> "X".
us(paper) --> "Y".
us(scissor) --> "Z".

prediction_points(Them-Us, Points) :-
    them_us_points(Them, Us, Points).

them_us_points(rock, rock, 4). % rock + draw = 1 + 3
them_us_points(rock, paper, 8). % paper + win = 2 + 6
them_us_points(rock, scissor, 3). % scissor + loss = 3 + 0
them_us_points(paper, rock, 1). % rock + loss = 1 + 0
them_us_points(paper, paper, 5). % paper + draw = 2 + 3
them_us_points(paper, scissor, 9). % scissor + win = 3 + 6
them_us_points(scissor, rock, 7). % rock + win = 1 + 6
them_us_points(scissor, paper, 2). % paper + loss = 2 + 0
them_us_points(scissor, scissor, 6). % scissor + draw = 3 + 3

round_prediction(rock-rock, rock-scissor). % lose
round_prediction(rock-paper, rock-rock). % draw
round_prediction(rock-scissor, rock-paper). % win
round_prediction(paper-rock, paper-rock).
round_prediction(paper-paper, paper-paper).
round_prediction(paper-scissor, paper-scissor).
round_prediction(scissor-rock, scissor-paper).
round_prediction(scissor-paper, scissor-scissor).
round_prediction(scissor-scissor, scissor-rock).

day02(Part1, Part2) :-
    phrase_from_file(strategy(Strategy), "./src/day02.txt"),
    part1(Strategy, Part1),
    part2(Strategy, Part2).

part1(Strategy, Part1) :-
    maplist(prediction_points, Strategy, Points),
    sumlist(Points, Part1).

part2(Strategy, Part2) :-
    maplist(round_prediction, Strategy, Predictions),
    maplist(prediction_points, Predictions, Points),
    sumlist(Points, Part2).
