soucet(0, X, X).
soucet(s(X), Y, s(Z1)):-soucet(X, Y, Z1).

%prvek(X, S):-X je prvek seznamu S
prvek(X, [X|_]).
prvek(X, [_|Xs]):-prvek(X, Xs).

%prunik(Xs, Ys, Zs):-Zs je seznam, ktery obsahuje prunik prvku seznamu Xs a Ys
prunik([], _, []).
prunik([X|Xs], Y, [X|Zs]):-member(X, Y), prunik(Xs, Y, Zs).
prunik([X|Xs], Y, Zs):- \+ member(X, Y), prunik(Xs, Y, Zs).

prunik2([], _, []).
prunik2([X|Xs], Y, [X|Zs]):-member(X, Y),!,prunik2(Xs, Y, Zs).
prunik2([_|Xs], Y, Zs):- prunik2(Xs, Y, Zs).
