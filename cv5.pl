% [1,2,3,3,4,2,3,4,4], 15

re([], []).
re([one(A)|Rs], S):-append(A, Z, S),re(Rs, Z).

re([opt(A)|Rs], S):-re([one(A)|Rs], S).
re([opt(_)|Rs], S):-re(Rs, S).

re([star(A)|Rs], S):-append(A, Z, S),re([star(A)|Rs],Z). 
re([star(_)|Rs], S):-re(Rs,S).

% podseznam(X, S):-X je podseznam S
podseznam([], []).
podseznam([X|Xs],[X|Ss]):-podseznam(Xs, Ss).
podseznam(X, [_|Ss]):-podseznam(X, Ss).

% rozklad(X, S):-soucet cisel v seznamu S je presne X
rozklad(0, []).
rozklad(X, [S|Ss]):-between(1, X, S),
                    X1 is X - S,
                    rozklad(X1, Ss).

% jako rozklad/2, ale kazda moznost jen jednou
rozkladk(X, S):-rozkladk(X, 1, S).
rozkladk(0, _, []).
rozkladk(X, M, [S|Ss]):-between(M, X, S),
                        X1 is X - S,
                        rozkladk(X1, S, Ss).


% spojeni rozdilovych seznamu, zakomentovana verze je
% citelnejsi, odkomentovana bezneji zapsana
%
% spojRS(X-KX, Y-KY, Z-KZ):-KX=Y,KZ=KY,Z=X.
spojRS(X-KX, KX-KZ, X-KZ).

% prevod rozdiloveho seznamu na normalni seznam
% naNormalni(A-Z, A):-Z=[].
naNormalni(A-[], A).

% vybere ze seznamu S dve hodnoty X1 a X2, jejichz soucet je presne K
vyberDve(S, K, X1, X2):-select(X1, S, S1), 
                        member(X2, S1),
                        K =:= X1 + X2.


