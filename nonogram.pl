% field(RowHints, ColHints, Field) :- ???

%field([], [], F) :- F.
%field([R | T], ColHints, F) :- append(F, row(ColHints, Row)

mkRow([], []).
mkRow([_ | T], Row) :- Row = [_ | RowRest], mkRow(T, RowRest).

field([], _, []).
field([_ | T], ColHints, F) :- mkRow(ColHints, Row), F = [Row | FieldRest], field(T, ColHints, FieldRest).

%row([], R) :- R.
%row([H | T], R) :- append(_

%f1 :- [[X11, X12, X13, X14, X15],
%       [X21, X22, X23, X24, X25],
%       [X31, X32, X33, X34, X35],
%       [X41, X42, X43, X44, X45],
%       [X51, X52, X53, X54, X55]].

cell(white). cell(black).

white(white).
black(black).

cells([]).
cells([H | T]) :- cell(H), cells(T).

numBlacks([], N) :- N = 0.
numBlacks([white | T], N) :- numBlacks(T, N).
numBlacks([black | T], N) :- numBlacks(T, N1), N is N1 + 1.

blacks(0, L, R) :- L = R.
blacks(N, [H | T], R) :- black(H), N1 is N - 1, blacks(N1, T, R).

%constrain(N, Row) :- N = numBlacks(Row).
constrain([], _).
constrain([0], []).
constrain([0], [H | T]) :- white(H), constrain([0], T).
%constrain([N], [H | T]) :- \+ N = 0, N1 is N - 1, black(H), constrain([N1], T).
constrain([N], L) :- \+ N = 0, blacks(N, L, R), constrain([0], R).
constrain([N], [H | T]) :- \+ N = 0, white(H), constrain([N], T).
constrain([0, Y], [H | T]) :- white(H), constrain([Y], T).
%constrain([X, Y], [H | T]) :- \+ X = 0, X1 is X - 1, black(H), constrain([X1, Y], T).
constrain([X, Y], L) :- \+ X = 0, blacks(X, L, R), constrain([0, Y], R).
constrain([X, Y], [H | T]) :- \+ X = 0, white(H), constrain([X, Y], T).

row(0, [R | _], R).
row(N, [_ | T], R) :- N1 is N - 1, row(N1, T, R).

nth(0, [X | _], X).
nth(N, [_ | T], X) :- N1 is N - 1, nth(N1, T, X).

col(_, [], []).
col(N, [H | T], C) :- nth(N, H, X), C = [X | R], col(N, T, R).

% 5x5, 3 -> fill(2, 3, black, _)
% 5x5, 4 -> fill(1, 4, black, _)
fill(P, S, E, C, [_ | T]) :- P < S, P1 is P + 1, fill(P1, S, E, C, T).
fill(P, S, E, C, [H | T]) :- P >= S, P < E, H = C, P1 is P + 1, fill(P1, S, E, C, T).
fill(P, S, E, _, _) :- P >= S, P >= E.

safeBlacks(Len, [N], L) :- Border is Len - N, Border < N, End is Len - Border, fill(0, Border, End, black, L).
safeBlacks(Len, [H, _ | _], L) :- safeBlacks(Len, [H], L).
safeBlacks(_, _, _).

nonogram1(X11, X12, X13, X14, X15,
         X21, X22, X23, X24, X25,
         X31, X32, X33, X34, X35,
         X41, X42, X43, X44, X45,
         X51, X52, X53, X54, X55) :-
    cells([X11, X12, X13, X14, X15,
           X21, X22, X23, X24, X25,
           X31, X32, X33, X34, X35,
           X41, X42, X43, X44, X45,
           X51, X52, X53, X54, X55]),
    F = [[X11, X12, X13, X14, X15],
         [X21, X22, X23, X24, X25],
         [X31, X32, X33, X34, X35],
         [X41, X42, X43, X44, X45],
         [X51, X52, X53, X54, X55]],

    row(0, F, R1), constrain([2], R1),
    row(1, F, R2), constrain([2,1], R2),
    row(2, F, R3), constrain([4], R3),
    row(3, F, R4), constrain([1], R4),
    row(4, F, R5), constrain([3], R5),

    col(0, F, C1), constrain([2], C1),
    col(1, F, C2), constrain([3,1], C2),
    col(2, F, C3), constrain([1,1], C3),
    col(3, F, C4), constrain([3], C4),
    col(4, F, C5), constrain([2], C5)
.

nonogram1a(X11, X12, X13, X14, X15,
         X21, X22, X23, X24, X25,
         X31, X32, X33, X34, X35,
         X41, X42, X43, X44, X45,
         X51, X52, X53, X54, X55) :-
    F = [[X11, X12, X13, X14, X15],
         [X21, X22, X23, X24, X25],
         [X31, X32, X33, X34, X35],
         [X41, X42, X43, X44, X45],
         [X51, X52, X53, X54, X55]],

    row(0, F, R1), constrain([2], R1),
    row(1, F, R2), constrain([2,1], R2),
    row(2, F, R3), constrain([4], R3),
    row(3, F, R4), constrain([1], R4),
    row(4, F, R5), constrain([3], R5),

    col(0, F, C1), constrain([2], C1),
    col(1, F, C2), constrain([3,1], C2),
    col(2, F, C3), constrain([1,1], C3),
    col(3, F, C4), constrain([3], C4),
    col(4, F, C5), constrain([2], C5)

    %% cells([X11, X12, X13, X14, X15,
    %%        X21, X22, X23, X24, X25,
    %%        X31, X32, X33, X34, X35,
    %%        X41, X42, X43, X44, X45,
    %%        X51, X52, X53, X54, X55])
.

% this is slower than nonogram1!
nonogram2(X11, X12, X13, X14, X15,
         X21, X22, X23, X24, X25,
         X31, X32, X33, X34, X35,
         X41, X42, X43, X44, X45,
         X51, X52, X53, X54, X55) :-
    cells([X11, X12, X13, X14, X15,
           X21, X22, X23, X24, X25,
           X31, X32, X33, X34, X35,
           X41, X42, X43, X44, X45,
           X51, X52, X53, X54, X55]),
    F = [[X11, X12, X13, X14, X15],
         [X21, X22, X23, X24, X25],
         [X31, X32, X33, X34, X35],
         [X41, X42, X43, X44, X45],
         [X51, X52, X53, X54, X55]],

    col(1, F, C2), constrain([3,1], C2),
    row(2, F, R3), constrain([4], R3),
    row(1, F, R2), constrain([2,1], R2),
    row(4, F, R5), constrain([3], R5),
    col(3, F, C4), constrain([3], C4),
    col(2, F, C3), constrain([1,1], C3),
    row(0, F, R1), constrain([2], R1),
    col(0, F, C1), constrain([2], C1),
    col(4, F, C5), constrain([2], C5),
    row(3, F, R4), constrain([1], R4)
.

nonogram3(X11, X12, X13, X14, X15,
         X21, X22, X23, X24, X25,
         X31, X32, X33, X34, X35,
         X41, X42, X43, X44, X45,
         X51, X52, X53, X54, X55) :-
    F = [[X11, X12, X13, X14, X15],
         [X21, X22, X23, X24, X25],
         [X31, X32, X33, X34, X35],
         [X41, X42, X43, X44, X45],
         [X51, X52, X53, X54, X55]],
    Len = 5,

    row(0, F, R1), safeBlacks(Len, [2], R1),
    row(1, F, R2), safeBlacks(Len, [2,1], R2),
    row(2, F, R3), safeBlacks(Len, [4], R3),
    row(3, F, R4), safeBlacks(Len, [1], R4),
    row(4, F, R5), safeBlacks(Len, [3], R5),

    col(0, F, C1), safeBlacks(Len, [2], C1),
    col(1, F, C2), safeBlacks(Len, [3,1], C2),
    col(2, F, C3), safeBlacks(Len, [1,1], C3),
    col(3, F, C4), safeBlacks(Len, [3], C4),
    col(4, F, C5), safeBlacks(Len, [2], C5),

    %% cells([X11, X12, X13, X14, X15,
    %%        X21, X22, X23, X24, X25,
    %%        X31, X32, X33, X34, X35,
    %%        X41, X42, X43, X44, X45,
    %%        X51, X52, X53, X54, X55]),

    constrain([2], R1), constrain([2,1], R2), constrain([4], R3), constrain([1], R4), constrain([3], R5),
    constrain([2], C1), constrain([3,1], C2), constrain([1,1], C3), constrain([3], C4), constrain([2], C5)
.

%nonogram(X11, X12, X13, X14, X15, X21, X22, X23, X24, X25, X31, X32, X33, X34, X35, X41, X42, X43, X44, X45, X51, X52, X53, X54, X55).

rows(S, E, _, []) :- S >= E.
rows(S, E, F, R) :- S < E, row(S, F, R1), R = [R1 | RR], S1 is S + 1, rows(S1, E, F, RR).

cols(S, E, _, []) :- S >= E.
cols(S, E, F, C) :- S < E, col(S, F, C1), C = [C1 | CC], S1 is S + 1, cols(S1, E, F, CC).

zip([], [], []).
zip([H1 | T1], [H2 | T2], R) :- R = [[H1, H2] | RR], zip(T1, T2, RR).

applySafeBlacks([Hints, L]) :- length(L, Len), safeBlacks(Len, Hints, L).

applyConstrain([Hints, L]) :- constrain(Hints, L).

nonogramGen(RowHints, ColHints, F) :-
    field(RowHints, ColHints, F),
    
    length(RowHints, NumRows), length(ColHints, NumCols),
    rows(0, NumRows, F, Rows), zip(RowHints, Rows, RowsWithHints),
    cols(0, NumCols, F, Cols), zip(ColHints, Cols, ColsWithHints),

    maplist(applySafeBlacks, RowsWithHints),
    maplist(applySafeBlacks, ColsWithHints),

    maplist(applyConstrain, RowsWithHints),
    maplist(applyConstrain, ColsWithHints)
.

printRow([]) :- nl, true.
printRow([black | T]) :- write('◼'), write(' '), printRow(T).
printRow([white | T]) :- write('◻'), write(' '), printRow(T).
printRow([_ | T]) :- write('¿'), write(' '), printRow(T).

printNonogram([]).
printNonogram([H | T]) :- printRow(H), printNonogram(T).

nonogramSolve(R, C, F) :- time(nonogramGen(R, C, F)), printNonogram(F).

nonogram1Gen(F) :-
    nonogramGen([[2], [2, 1], [4], [1], [3]],
                [[2], [3, 1], [1, 1], [3], [2]],
                F).

nonogram2Gen(F) :-
    nonogramGen([[6],[1,1],[1,1,1,1],[1,1,1,1],[1,1,1,1],[1,1,1,1],[1,1,1,1], [1,1,1,1],[1,1],[6]],
                [[6],[1,1],[1,1,1,1],[1,4,1],[1,1],[1,1],[1,4,1],[1,1,1,1],[1,1],[6]],
                F).

nonogram3Gen(F) :-
    nonogramGen([[7],[1,1],[1,1,1,1],[1,1],[1,3,1],[1,1],[7]],
                [[7],[1,1],[1,1,1,1],[1,1,1],[1,1,1,1],[1,1],[7]],
                F).

% list of simple nonograms sorted by increasing size: http://en.japonskie.ru/index.php?kind=1&resh=2&slev=1&lev=1&k_name=&notr=0&list=0&sort=1&sort_asc=1

% correct. (from http://de.wikipedia.org/wiki/Nonogramm#Beispiel)
%   nonogramGen([[0],[4],[6],[2,2],[2,2],[6],[4],[2],[2],[2],[0]],[[0],[9],[9],[2,2],[2,2],[4],[4],[0]],F), write(F).

% doesn't finish. (from http://en.japonskie.ru/crossword/vopros1)
%   nonogramGen([[7],[2,4],[2,3],[4,4],[5,4],[5,4],[5,4],[3,4],[4],[3],[3],[2],[1],[1],[],[3],[5],[5],[5],[3]],[[4],[6],[7],[2,5,3],[1,3,5],[1,3,5],[1,2,5],[2,3,3],[11],[10],[8],[5]],F), write(F).

% correct. (from http://en.japonskie.ru/crossword/chashechka2)
%   nonogramSolve([[8],[10],[1,8],[8],[4]],[[2],[1,1],[4],[4],[5],[5],[5],[5],[4],[3]], F).

% incorrect. (from http://en.japonskie.ru/crossword/infogram)
%   nonogramSolve([[5],[2,2],[1,1,1,1],[1,1],[2,3],[1,2,2,2],[4],[1,1],[3,3]], [[1],[1],[5,1],[2,2,1],[1,1,3],[1,1],[1,1,4],[2,2,1],[4,1],[2],[1]], F).

% incorrect. (from http://www.janko.at/Raetsel/Nonogramme/221.a.htm)
%   nonogramSolve([[2],[4,1],[1,1],[2,1,2],[9],[7,1],[9],[6,2],[4,2],[5]], [[1],[1,4],[2,6],[2,7],[1,6],[8],[1,4,1],[4,2],[2,3],[4]], F).

assign(_, 0, L, R) :- L = R.
assign(C, N, [H | T], R) :- H = C, N1 is N - 1, assign(C, N1, T, R).

% require that all elements of the list are of the given color
allC(_, []).
allC(C, [H | T]) :- H = C, allC(C, T).

% minimum length of a constraint
minLength([], L) :- L = 0.
minLength([N], L) :- L = N.
minLength([N1, N2], L) :- L is N1 + 1 + N2.
minLength([N1, N2 | RN], L) :- \+ RN = [], minLength(RN, RL), L is N1 + 1 + N2 + 1 + RL.

% given a constraint and a "line", checks if it's a possible (valid) binding
possibility([], _).
possibility([N], L) :- length(L, Len), Len >= N, assign(black, N, L, R), allC(white, R).
possibility([N], [H | T]) :- length([H | T], Len), Len > N, white(H), possibility([N], T).
possibility([N | RN], L) :- \+ RN = [], length(L, Len), minLength([N | RN], ML), Len >= ML, assign(black, N, L, [RH | RT]), white(RH), possibility(RN, RT).
possibility([N | RN], [H | T]) :- \+ RN = [], length([H | T], Len), minLength([N | RN], ML), Len > ML, white(H), possibility([N | RN], T).

% given a constraint and a line, calculates a list of all possibilities
possibilities(CN, L, PS) :- findall(L, possibility(CN, L), PS).

%common([], _).
common([A], C) :- A = C.
common([A, B], C) :- A = B, A = C.
common([A, B], _) :- \+ A = B.
common([A, B | R], C) :- \+ R = [], A = B, common([B | R], C).
common([A, B | R], _) :- \+ R = [], \+ A = B.

commonsInner([], []).
commonsInner([Col | Cols], C) :- common(Col, CC), C = [CC | CR], commonsInner(Cols, CR).

% given a list of possible bindings, calculate the positions that are the same in all of them
commons([P | PS], C) :- length(P, Len), cols(0, Len, [P | PS], Cols), commonsInner(Cols, C).

applyForce([Hints, L]) :- possibilities(Hints, L, PS), commons(PS, L).

% needs to be run multiple times, e.g. until all variables are bound.
%  (currently by hand, don't know yet how to do that.)
nonogramGenForce(RowHints, ColHints, F) :-
    field(RowHints, ColHints, F),

    length(RowHints, NumRows), length(ColHints, NumCols),
    rows(0, NumRows, F, Rows), zip(RowHints, Rows, RowsWithHints),
    cols(0, NumCols, F, Cols), zip(ColHints, Cols, ColsWithHints),

    maplist(applyForce, RowsWithHints),
    maplist(applyForce, ColsWithHints),

    %maplist(applyConstrain, RowsWithHints),
    %maplist(applyConstrain, ColsWithHints),

    true
.

% solution for http://www.janko.at/Raetsel/Nonogramme/221.a.htm
%   needs 4 runs
% RH = [[2],[4,1],[1,1],[2,1,2],[9],[7,1],[9],[6,2],[4,2],[5]], CH = [[1],[1,4],[2,6],[2,7],[1,6],[8],[1,4,1],[4,2],[2,3],[4]], nonogramGenForce(RH, CH, F), write(F), nl, nonogramGenForce(RH, CH, F), write(F), nl, nonogramGenForce(RH, CH, F), write(F), nl, nonogramGenForce(RH, CH, F), write(F), nl, printNonogram(F).

% solution for nonogram3Gen
%   needs 1 run
% RH = [[7],[1,1],[1,1,1,1],[1,1],[1,3,1],[1,1],[7]], CH = [[7],[1,1],[1,1,1,1],[1,1,1],[1,1,1,1],[1,1],[7]], nonogramGenForce(RH, CH, F), write(F), nl, printNonogram(F).

% solution for http://en.japonskie.ru/crossword/infogram
%   needs 6 runs
% RH = [[5],[2,2],[1,1,1,1],[1,1],[2,3],[1,2,2,2],[4],[1,1],[3,3]], CH = [[1],[1],[5,1],[2,2,1],[1,1,3],[1,1],[1,1,4],[2,2,1],[4,1],[2],[1]], nonogramGenForce(RH, CH, F), write(F), nl, nonogramGenForce(RH, CH, F), write(F), nl, nonogramGenForce(RH, CH, F), write(F), nl, nonogramGenForce(RH, CH, F), write(F), nl, nonogramGenForce(RH, CH, F), write(F), nl, nonogramGenForce(RH, CH, F), write(F), nl, printNonogram(F).

% solution for http://en.japonskie.ru/crossword/vopros1
% RH = [[7],[2,4],[2,3],[4,4],[5,4],[5,4],[5,4],[3,4],[4],[3],[3],[2],[1],[1],[],[3],[5],[5],[5],[3]], CH = [[4],[6],[7],[2,5,3],[1,3,5],[1,3,5],[1,2,5],[2,3,3],[11],[10],[8],[5]], nonogramGenForce(RH, CH, F), write(F), nl, nonogramGenForce(RH, CH, F), write(F), nl, nonogramGenForce(RH, CH, F), write(F), nl, nonogramGenForce(RH, CH, F), write(F), nl, nonogramGenForce(RH, CH, F), write(F), nl, printNonogram(F).
