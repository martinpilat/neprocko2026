% prefix(P, S):- P je prefix seznamu S
prefix(P, S):-append(P, _, S).

% sufix(P, S):- P je sufix seznamu S
sufix(P, S):-append(_, P, S).

% P a P1 jsou dohromady seznam S (ukazka, ze append funguje i takto)
rozdel(P, P1, S):-append(P, P1, S).

% spoj(X, Y, XY):-XY je spojeni seznamu X a Y (append)
spoj([], Y, Y).
spoj([X|Xs], Y, [X|Ys]):-spoj(Xs, Y, Ys).

% pridejSeznam(X, S, XS):-XS je seznam S s X pridanym na zacatek (pro porovnani zakladniho pripadu s pridejBVS)
pridejSeznam(X, [], [X]).
pridejSeznam(X, Y, [X|Y]).

% pridejBVS(X, T, XT):-XT je BVS T s pridanym X
pridejBVS(X, nil, t(nil, X, nil)).
pridejBVS(X, t(LP, H, PP), t(NLP, H, PP) ):-X < H, pridejBVS(X, LP, NLP).
pridejBVS(X, t(LP, H, PP), t(LP, H, NPP) ):-X >= H,pridejBVS(X, PP, NPP).

%soucet(S, X):-X je soucet prvku seznamu S (pro porovnani s nasledujicim)
soucet([], 0).
soucet([S|Ss], X):-soucet(Ss, X1), X is X1 + S.

%seznamNaStrom(S, T):-T je binarni vyhledavaci strom, ktery obsahuje stejne prvky jako seznam S
seznamNaStrom([], nil).
seznamNaStrom([X|Xs], T):-seznamNaStrom(Xs, T1), pridejBVS(X, T1, T).

%sNS(S, T):-to same jako seznamNaStrom ale s akumulatorem
sNS(S, T):-sNS(S, nil, T).
sNS([], A, A).
sNS([X|Xs], A, T):-pridejBVS(X, A, NA), sNS(Xs, NA, T).

%soucet2(S, Sum):-Sum je soucet cisel ze seznamu S (pro porovnani s predchazejicim)
soucet2(S, Sum):-soucet2(S, 0, Sum).
soucet2([], A, A).
soucet2([S|Ss], A, Sum):-NA is A + S, soucet2(Ss, NA, Sum).

%stromNaSeznamIn(T, S):-S obsahuje prvky stromu S v In/Pre/Post order poradi
stromNaSeznamIn(nil, []).
stromNaSeznamIn(t(LP, H, PP), S):-stromNaSeznamIn(LP, LS),
                                  stromNaSeznamIn(PP, PS),
                                  append(LS, [H|PS], S).
stromNaSeznamPre(nil, []).
stromNaSeznamPre(t(LP, H, PP), S):-stromNaSeznamPre(LP, LS),
                                  stromNaSeznamPre(PP, PS),
                                  append([H|LS], PS, S).
stromNaSeznamPost(nil, []).
stromNaSeznamPost(t(LP, H, PP), S):-stromNaSeznamPost(LP, LS),
                                    stromNaSeznamPost(PP, PS),
                                    append(LS, PS, S1),
                                    append(S1, [H], S).

%prvekStromu(X, T):-X je prvek stromu T (poradi radek ovlivuje poradi vystupu)
prvekStromu(X, t(LP, _, _)):-prvekStromu(X, LP).
prvekStromu(X, t(_, _, PP)):-prvekStromu(X, PP).
prvekStromu(X, t(_, X, _)).
