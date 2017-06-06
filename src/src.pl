% Prolog vieja

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ej. 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
herramienta(rayo, 10).
herramienta(volatilizador, 40).
herramienta(encendedor, 5).

jerarquica(X, Y) :-
    (herramienta(X, _); X = jerarquica(_, _)),
    (herramienta(Y, _); Y = jerarquica(_, _)).

%% no se si esta bien decir esto, porque una herramienta no es una composicion:
composicion(X, P, 1):- herramienta(X, P).
composicion(binaria(X,Y),P,5):- herramienta(X, PX),
                                herramienta(Y, PY),
                                X\=encendedor, P is 2*PX + PY.
composicion(jerarquica(X, Y), P, C) :-
    composicion(X, PX, CX),
    composicion(Y, PY, CY),
    P is PY * PX,
    C is 2*(CY+CX).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ej. 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% permute(+, ?): permute(X,Y) <=> Y es permutacion de X.
permute([], []).
permute(L,[X|R]) :- omit(X,L,M), permute(M,R).

% omit(?, +, ?): omit(X,Y,Z) <=> Z es Y sin la primera aparicion de X.
omit(H,[H|T],T).
omit(X,[H|L],[H|R]) :- omit(X,L,R), X \= H.

% componentes(+Z, ?CZ): componentes(X, M) <=> M es la lista de componentes de X.
componentes(Z, [Z]) :- herramienta(Z, _).
componentes(binaria(X,Y), [X,Y]) :- herramienta(X, _), herramienta(Y, _).
componentes(jerarquica(X, Y), CZ) :- CZ \= [],
    append(CX, CY, CZ),
    CX \= [], CY \= [],
    componentes(X, CX),
    componentes(Y, CY).

% configuracion(?M, ?Conf, ?P, ?C)
%% esta linea creo que no va VVVVVVVVVVV por favor confirmar. Si la sacamos,
%% dejamos de tener repetidos.
% configuracion([X], X, P, 1) :- herramienta(X, P).
configuracion(M, Conf, P, C) :- permute(M, M1),
    componentes(Conf, M1),
    composicion(Conf, P, C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ej. 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% masPoderosa(+M1, +M2)
masPoderosa(M1,M2) :- configuracion(M1, _, P, _),
    forall(configuracion(M2, _, PPrima, _), P > PPrima).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ej. 4 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mejor(+M1, +M2)
% Ejemplo en el que da true: mejor([rayo, rayo], [volatilizador]).
mejor(M1, M2) :- forall(configuracion(M2, _, C2, P2),
    (configuracion(M1, _, C1, P1), P1 >= P2, C1 < C2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ej. 5 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% setMember(+X, +XS): setMember(X, XS) solo si la lista X es una permutacion de
%                     una de las listas de XS (XS es una lista de listas).
setMember(X, [C|_]) :- permute(X, C).
setMember(X, [_|L]) :- setMember(X, L).

% agregarAAlguno(+A, +XS, ?YS): agregarAAlguno(A,XS,YS) si y solo si YS contiene
%                               todas las listas de XS, pero ademas una tiene a
%                               A agregado al principio.
agregarAAlguno(A, [C], [[A|C]]).
agregarAAlguno(A, [C|L], [C|M]) :- L\= [], not(setMember(C,L)),
    agregarAAlguno(A, L, M).
agregarAAlguno(A, [C|L], [C|M]) :- L\= [], setMember(C,L),
    agregarAAlguno(A, L, M).
agregarAAlguno(A, [C|L], [[A|C]|L]) :- L\= [], not(setMember(C,L)).

% partes(+X, +N, ?XS): partes(X,N,XS) si y solo si XS tiene largo N y
%                      permute(concat(XS), X) (concat como en haskell).
partes(L, 1, [L]).
partes([X|L], S, Ps) :- S > 1, partes(L, S, P1), agregarAAlguno(X, P1, Ps).
partes([X|L], S, [[X]|Ps1]) :- S1 is S - 1, partes(L, S1, Ps1).

% len(?X, ?N): largo de una lista.
len([], 0).
len([_|L], N) :- len(L, N1), N is N1 + 1.

% potencialesOk(+Ps, ?Hs, ?Confs):
%     potencialesOk([P1, ..., Pn], [Hs1, ..., Hsn], [Conf1, ..., Confn]) si y
%     solo si para todo i, Hsi son las herramientas de la configuracion Confi
%     y ademas el potencial de la configuracion es mayor o igual que Pi.
potencialesOk([],[],[]).
potencialesOk([Pot|P],[Herr|H],[Conf|C]) :-
    configuracion(Herr, Conf, Pot1, _),
    Pot1 >= Pot,
    potencialesOk(P, H, C).

% usarRepitiendo(+M1, +Ps, ?resto, ?Cs): igual que usar pero potencialmente con
%                                        repetidos.
usarRepitiendo(M1, Ps, Resto, Cs) :-
    len(Ps, Len1), S is Len1 + 1,
    partes(M1, S, Particion),
    permute(Particion, [RestoSet|ParticionShufleada]),
    permute(RestoSet, Resto),
    potencialesOk(Ps, ParticionShufleada, Cs).

% usar(+M1, +Ps, ?Cs, ?M2)
usar(M1, Ps, Cs, Resto) :-
    setof((ConfRepe, RestoRepe),
          usarRepitiendo(M1, Ps, RestoRepe, ConfRepe),
          Confs),
    member((Cs, Resto), Confs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ej. 6 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% listaHerramientas(+N, ?Hs): listas de herramientas de un cierto largo.
listaHerramientas(0, []).
listaHerramientas(N, [X|L]) :- N > 0, herramienta(X, _),
    N1 is N - 1, listaHerramientas(N1, L).

% comprarRepitiendo(+P, +C, ?M): comprarRepitiendo es como comprar pero
%                                potencialmente repite resultados; es solo una
%                                definicion auxiliar para que comprar quede mas
%                                claro.
comprarRepitiendo(P, C, M) :-
    between(0, C, C1),
    listaHerramientas(C1, M),
    configuracion(M, _, Pot, _),
    Pot >= P.

% comprar(+P, +C, ?M)
comprar(P, C, M) :- setof(M1, comprarRepitiendo(P, C, M1), Ms), member(M, Ms).
