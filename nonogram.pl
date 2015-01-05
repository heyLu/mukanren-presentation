%% nonogram solver, for 'Praktikum Wissensrepräsentation'
%%
%% Eugen Rein, Jakob Matthes, Lucas Stadler


%% introduction

% # An example
%
% A nonogram is given by it's row and column hints, both of which are
% lists of lists of integers.
%
% For example, consider this simple nonogram:
%
%       3 1
%     2 1 1 3 2
%   2 _ _ _ _ _
% 2 1 _ _ _ _ _
%   4 _ _ _ _ _
%   1 _ _ _ _ _
%   3 _ _ _ _ _
%
% The row hints would be `[[2], [2, 1], [4], [1], [3]]` and the column
% hints would be `[[2], [3, 1], [1, 1], [3], [2]]`.
%
% To solve the above nonogram, run `nonogramSolve(RH, CH, F).`, where
% `RH` and `CH` are the row and column hints.
%
% `nonogramSolve` is a convenience predicate that uses `nonogramGenMixed`
% to solve the nonogram, but also times it and prints the result.

% # Solving strategy
%
% Our solution strategy is based on the idea that the hints always 
% "force" some field values.
% Consider the following example:
%
%    4 _ _ _ _ _
%
% Every valid assignment must assign 'black' to the center three fields,
% this we now know their values:
%
%    4 _ ◼ ◼ ◼ _
%
% A slightly more interesting example:
%
%  3 2 _ _ _ _ ◼ _ ◼ ◻ _ _
%
% We must transform '◼ _ ◼' into '◼ ◼ ◼', because single '◼' blocks are
% not allowed by the hint. In turn, there is only one position for the
% '◼ ◼' block, i.e. at the end of the line and we know that all other
% fields must be '◻':
%
%  3 2 ◻ ◻ ◻ ◻ ◼ ◼ ◼ ◻ ◼ ◼
%
% To compute which fields are "forced" we compute all valid assignments
% of a hint, given a line. This line (e.g. a row or column) may already
% contain some "forced" values, which restricts the remaining
% possibilities.
% Among these possibilities we check which fields have the same value in
% all possibilities, these are the forced values.
%
% The possibilities are calculated with a predicate of the same name and
% the common fields are computed using `commons`:
%
%   ?- L = [X1, X2, X3, X4, X5], possibilities([4], L, PS), commons(PS, C), printRow(C).
%   ¿ ◼ ◼ ◼ ¿
%   C = [_G592, black, black, black, _G604]
%
% `nonogramGenForceLimes` forces fields until there are no more changes.
%
% In addition to the 'forcing' described above, we also use a 'guessing'
% strategy, which just fills in the unknown fields based on the hints,
% trying all possible assignments.
% This guessing strategy is used after the forcing strategy, to fill in
% any remaining fields.

% # Reading the code
%
% Start by solving some examples using `nonogramSolve`, then read it's
% definition, which refers you to `nonogramGenMixed`.
%
% `nonogramGenMixed` calls `nonogramGenForceLimes` and `nonogramGenGuess`,
% which are implementations of the forcing and guessing strategies.
%
% The `possibility` predicate is the central piece of the code, it calculates
% the possible assignments for a line, based on the hint for it. Both
% strategies use this predicate.
%
% The forcing strategy generates all possible assignments with it and
% calculates the common fields, as described above.
%
% The guessing strategy uses the `possibility` predicate to try all
% possible assignments and finishes if it has found values for all free
% variables.


%% field construction


mkRow([], []).
mkRow([_ | T], [_ | RowRest]) :- mkRow(T, RowRest).

% given row and column hints, generate the field with unassigned variables
field([], _, []).
field([_ | T], ColHints, [Row | FieldRest]) :- mkRow(ColHints, Row), field(T, ColHints, FieldRest).


%% field access


% get the nth row from a field
row(0, [R | _], R).
row(N, [_ | T], R) :- N1 is N - 1, row(N1, T, R).

% get the nth element from a list
nth(0, [X | _], X).
nth(N, [_ | T], X) :- N1 is N - 1, nth(N1, T, X).

% get the nth column from a field
col(_, [], []).
col(N, [H | T], [X | R]) :- nth(N, H, X), col(N, T, R).

% get the rows from S to E from the field
%
%   % get all rows from a field
%   ?- rows(0, NumRows, F, R)
rows(S, E, _, []) :- S >= E.
rows(S, E, F, [R1 | RR]) :- S < E, row(S, F, R1), S1 is S + 1, rows(S1, E, F, RR).

% get the columns from S to E from the field
cols(S, E, _, []) :- S >= E.
cols(S, E, F, [C1 | CC]) :- S < E, col(S, F, C1), S1 is S + 1, cols(S1, E, F, CC).


%% pretty printing


printRow([]) :- nl, true.
printRow([X | T]) :- var(X), write('¿'), write(' '), printRow(T).
printRow([black | T]) :- write('◼'), write(' '), printRow(T).
printRow([white | T]) :- write('◻'), write(' '), printRow(T).

printNonogram([]).
printNonogram([H | T]) :- printRow(H), printNonogram(T).


%% helpers


white(white).
black(black).


%% simple solving


% assign the color C to the elements between S and E
%   (P counts the "current" position, initialize with 0)
%
%   ?- fill(0, 2, 5, black, [X1,X2,X3,X4,X5,X6,X7]).
%   X3 = X4, X4 = X5, X5 = black
fill(P, S, E, C, [_ | T]) :- P < S, P1 is P + 1, fill(P1, S, E, C, T).
fill(P, S, E, C, [H | T]) :- P >= S, P < E, H = C, P1 is P + 1, fill(P1, S, E, C, T).
fill(P, S, E, _, _) :- P >= S, P >= E.

% assign black to fields that are guaranteed to be black
%
%   ?- safeBlacks(7, [5], [X1,X2,X3,X4,X5,X6,X7]).
%   X3 = X4, X4 = X5, X5 = black
safeBlacks(Len, [N], L) :- Border is Len - N, Border < N, End is Len - Border, fill(0, Border, End, black, L).
safeBlacks(Len, [H, _ | _], L) :- safeBlacks(Len, [H], L).
safeBlacks(_, _, _).


%% utilities


% joins pairs from two lists (cf. zip in e.g. haskell)
zip([], [], []).
zip([H1 | T1], [H2 | T2], [[H1, H2] | RR]) :- zip(T1, T2, RR).


%% helpers


% groups hints with columns and rows
groupHints(RowHints, ColHints, F, RowsWithHints, ColsWithHints) :-
    length(RowHints, NumRows), length(ColHints, NumCols),
    rows(0, NumRows, F, Rows), zip(RowHints, Rows, RowsWithHints),
    cols(0, NumCols, F, Cols), zip(ColHints, Cols, ColsWithHints)
.

% helper functions to be usable on a list of hint/line pairs
applySafeBlacks([Hints, L]) :- length(L, Len), safeBlacks(Len, Hints, L).
%applyConstrain([Hints, L]) :- write(Hints), write(' '), write(L), nl, possibility(Hints, L).
applyConstrain([Hints, L]) :- possibility(Hints, L).


%% solvers


% find a solution by constructing the field, finding "safe" black fields and
%  guessing the remaining fields based on the constraints
nonogramGenHeuristic(RowHints, ColHints, F) :-
    field(RowHints, ColHints, F),
    groupHints(RowHints, ColHints, F, RowsWithHints, ColsWithHints),

    maplist(applySafeBlacks, RowsWithHints),
    maplist(applySafeBlacks, ColsWithHints),

    maplist(applyConstrain, RowsWithHints),
    maplist(applyConstrain, ColsWithHints)
.


% assign the color C to the first Nth elements of L, R contains the remaining elements
assign(_, 0, L, L).
assign(C, N, [C | T], R) :- N1 is N - 1, assign(C, N1, T, R).

% require that all elements of the list are of the given color
allC(_, []).
allC(C, [C | T]) :- allC(C, T).

% minimum length of a constraint
minLength([], 0).
minLength([N], N).
minLength([N1, N2], L) :- L is N1 + 1 + N2.
minLength([N1, N2 | RN], L) :- \+ RN = [], minLength(RN, RL), L is N1 + 1 + N2 + 1 + RL.

% given a constraint and a "line", checks if it's a possible (valid) binding
possibility([], _).
possibility([N], L) :- length(L, Len), Len >= N, assign(black, N, L, R), allC(white, R).
possibility([N], [H | T]) :- length([H | T], Len), Len > N, white(H), possibility([N], T).
possibility([N | RN], L) :- \+ RN = [], length(L, Len), minLength([N | RN], ML), Len >= ML, assign(black, N, L, [RH | RT]), white(RH), possibility(RN, RT).
possibility([N | RN], [H | T]) :- \+ RN = [], length([H | T], Len), minLength([N | RN], ML), Len > ML, white(H), possibility([N | RN], T).

% given a constraint and a line, calculates a list of all possibilities
%
%   ?- length(L, 5), possibilities([1,1], L, PS), printNonogram(PS).
possibilities(CN, L, PS) :- findall(L, possibility(CN, L), PS).

% check if all elements in the list are the same and assign that value to the second argument if so
%
%   ?- common([1,1,1], C).
%   ?- common([1,2,1], C).
common([A], A).
common([A, A], A).
common([A, B], _) :- \+ A = B.
common([A, A | R], C) :- \+ R = [], common([A | R], C).
common([A, B | R], _) :- \+ R = [], \+ A = B.

commonsInner([], []).
commonsInner([Col | Cols], [CC | CR]) :- common(Col, CC), commonsInner(Cols, CR).

% given a list of possible bindings, calculate the positions that are the same in all of them
%
%   ?- length(L, 5), possibilities([1,1,1], L, PS), commons(PS, C), printRow(C).
%   ?- L = [X1,X2,X3,X4,black,X6,black,white,X9,X10], possibilities([3,2], L, PS), commons(PS, C), printRow(C).
commons([P | PS], C) :- length(P, Len), cols(0, Len, [P | PS], Cols), commonsInner(Cols, C).

% convenience helper for use with maplist
applyForce([Hints, L]) :- possibilities(Hints, L, PS), commons(PS, L).


nonogramGenForce(RowHints, ColHints, F) :-
    field(RowHints, ColHints, F),
    groupHints(RowHints, ColHints, F, RowsWithHints, ColsWithHints),

    maplist(applyForce, RowsWithHints),
    maplist(applyForce, ColsWithHints),

    %maplist(applyConstrain, RowsWithHints),
    %maplist(applyConstrain, ColsWithHints),

    true
.

% forces to run nonogramGenForce until all variables in F are bound
nonogramGenForceFull(_,_,_, true).
nonogramGenForceFull(RowHints, ColHints, F, false) :- nonogramGenForce(RowHints, ColHints, F), \+ ground(F), nonogramGenForceFull(RowHints, ColHints, F, false).
nonogramGenForceFull(RowHints, ColHints, F, false) :- nonogramGenForce(RowHints, ColHints, F), ground(F), nonogramGenForceFull(RowHints, ColHints, F, true).

% forces nonogramGenForce to run until more iterations cause no more changes
nonogramGenForceLimes(RowHints, ColHints, F1, F2) :- nonogramGenForce(RowHints, ColHints, F1), F2 == F1, nonogramGenForce(RowHints, ColHints, F2), nonogramGenForce(RowHints, ColHints, F2).
nonogramGenForceLimes(RowHints, ColHints, F1, F2) :- nonogramGenForce(RowHints, ColHints, F1), F2 = F1, nonogramGenForceLimes(RowHints, ColHints, F1, F2).

% guesses content of field based on hints
nonogramGenGuess(RowHints, ColHints, F) :-
    field(RowHints, ColHints, F),
    groupHints(RowHints, ColHints, F, RowsWithHints, ColsWithHints),
    
    maplist(applyConstrain, RowsWithHints),
    maplist(applyConstrain, ColsWithHints).

% first derive, then guess
nonogramGenMixed(RowHints, ColHints, F) :-
    nonogramGenForceLimes(RowHints, ColHints, F, _),
    nonogramGenGuess(RowHints, ColHints, F).

% convenience function to time the solution and print it
nonogramSolve(RH, CH, F) :- time(nonogramGenMixed(RH, CH, F)), printNonogram(F).


%% examples


% nonogramSolve([[2], [2, 1], [4], [1], [3]], [[2], [3, 1], [1, 1], [3], [2]], F).

% from http://de.wikipedia.org/wiki/Nonogramm#Beispiel
%   nonogramSolve([[0],[4],[6],[2,2],[2,2],[6],[4],[2],[2],[2],[0]],[[0],[9],[9],[2,2],[2,2],[4],[4],[0]],F).

% http://en.japonskie.ru/crossword/chashechka2
%   nonogramSolve([[8],[10],[1,8],[8],[4]],[[2],[1,1],[4],[4],[5],[5],[5],[5],[4],[3]], F).

% from http://en.japonskie.ru/crossword/vopros1
%   nonogramSolve([[7],[2,4],[2,3],[4,4],[5,4],[5,4],[5,4],[3,4],[4],[3],[3],[2],[1],[1],[],[3],[5],[5],[5],[3]],[[4],[6],[7],[2,5,3],[1,3,5],[1,3,5],[1,2,5],[2,3,3],[11],[10],[8],[5]],F).

% from http://jowang.name/dotpix/#/play/19
%   nonogramSolve([[7],[1,1],[1,1,1,1],[1,1],[1,3,1],[1,1],[7]], [[7],[1,1],[1,1,1,1],[1,1,1],[1,1,1,1],[1,1],[7]], F).

% from http://en.japonskie.ru/crossword/infogram
%   nonogramSolve([[5],[2,2],[1,1,1,1],[1,1],[2,3],[1,2,2,2],[4],[1,1],[3,3]], [[1],[1],[5,1],[2,2,1],[1,1,3],[1,1],[1,1,4],[2,2,1],[4,1],[2],[1]], F).

% from http://www.janko.at/Raetsel/Nonogramme/221.a.htm
%   nonogramSolve([[2],[4,1],[1,1],[2,1,2],[9],[7,1],[9],[6,2],[4,2],[5]], [[1],[1,4],[2,6],[2,7],[1,6],[8],[1,4,1],[4,2],[2,3],[4]], F).

% http://en.japonskie.ru/crossword/black_cat
%   nonogramSolve([[4],[1,1,1],[5,4],[1,1,1,1],[12],[11],[8],[4,1,1],[1,1,1,2],[1,1,1]], [[4],[1,2,2],[4,1],[1,6],[7],[3],[3],[3],[3,6],[1,1,3],[1,1,5],[1,4,2]], F).

% list of simple nonograms sorted by increasing size: http://en.japonskie.ru/index.php?kind=1&resh=2&slev=1&lev=1&k_name=&notr=0&list=0&sort=1&sort_asc=1
