pridejz(X, Xs, [X|Xs]).

% pridejk(X, Xs, XsX):-XsX vznikne pridanim X na konec Xs
pridejk(X, [], [X]).
pridejk(X, [S|Sx], [S|SxX]):-pridejk(X, Sx, SxX).

% otoc(Xs, Sx):-Sx je Xs pozpatku
otoc([], []).
otoc([X|Xs], R):-otoc(Xs, Z), pridejk(X, Z, R).

otoc2(Xs, Sx):-otoc2(Xs, [], Sx).
otoc2([], A, A).
otoc2([X|Xs], A, Sx):-otoc2(Xs, [X|A], Sx).


%range(Start, End, Range):-Range je seznam cisel od Start do End -1
range(Start, End, []):-Start >= End.
range(Start, End, [Start|Zbytek]):-Start < End, 
                                   S1 is Start + 1,
                                   range(S1, End, Zbytek).

%vypustVse(X, S, SbezX):-S bez X je S bez vsech vyskytu X
vypustVse(_, [], []).
vypustVse(X, [X|S], Z):-vypustVse(X, S, Z).
vypustVse(X, [Y|S], [Y|Z]):-X \= Y, vypustVse(X, S, Z).

%podseznam(S, PS):-PS je podseznam S
podseznam([], []).
podseznam([X|Xs], [X|Z]):-podseznam(Xs, Z).
podseznam([_|Xs], Z):-podseznam(Xs, Z).

%soucet(S, X):-X je soucet prvku seznamu S
soucet([], 0).
soucet([S|Ss], X):-soucet(Ss, X1), X is X1 + S.

%soucetPodmnoziny(S, N, PS):-PS je podseznam S se souctem N
soucetPodmnoziny(S, N, PS):-podseznam(S, PS), soucet(PS, N).
