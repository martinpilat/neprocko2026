test1(g([1,2,3,4,5,6], [1-2,2-3,2-4,2-5,3-4,3-6,4-5,5-6])).

hrana(g(_, E), H1, H2):-member(H1-H2, E);member(H2-H1,E).

%dfs(G, S, C, Cesta):-Cesta je cesta v G z S do C (DFS prohledavani)
dfs(Graf, Start, Cil, Cesta):-dfs(Graf, Start, Cil, [], Cesta).
dfs(_, S, S, _, [S]).
dfs(G, S, C, N, [S|Zc]):-hrana(G, S, S1), 
                         % heuristika(S1, DO), % pro iterative deepening A*, heuristika vraci spodni odhad
                                               % poctu kroku do konce
                         % DO >= len(Zc),
                         \+ member(S1, N),
                         dfs(G, S1, C, [S1|N], Zc).

id(Graf, Start, Cil, MaxL, Cesta):-between(1, MaxL, L),
                                   length(Cesta, L),
                                   dfs(Graf, Start, Cil, Cesta),!.

dfsP(Graf, Start, Cil, Hrana, Cesta):-dfsP(Graf, Start, Cil, [], Hrana, Cesta).
dfsP(_, S, S, _, _, [S]).
dfsP(G, S, C, N, Hrana, [S|Zc]):-call(Hrana, S, S1), 
                         % heuristika(S1, DO), % pro iterative deepening A*, heuristika vraci spodni odhad
                                               % poctu kroku do konce
                         % DO >= len(Zc),
                         \+ member(S1, N),
                         dfsP(G, S1, C, [S1|N], Hrana, Zc).

id(Graf, Start, Cil, MaxL, Hrana, Cesta):-between(1, MaxL, L),
                                   length(Cesta, L),
                                   dfsP(Graf, Start, Cil, Hrana, Cesta),!.
