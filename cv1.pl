muz(adam).
muz(honza).
muz(david).

zena(jitka).
zena(tereza).
zena(klaudie).

rodic(adam, jitka).
rodic(jitka, tereza).
rodic(karel, tereza).
rodic(karel, honza).
rodic(jitka, honza).

otec(X, Y):-rodic(X, Y), muz(X).
matka(X, Y):-rodic(X, Y), zena(X).

%sestra(?X, ?Y):-X je sestra Y
sestra(Sestra, Koho):-zena(Sestra),
                      rodic(Rodic,Sestra),
                      rodic(Rodic,Koho),
                      Sestra \= Koho.
