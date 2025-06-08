% rules.pl – Regras de inferência para atualizar a KB com base nos perceptos

:- use_module(library(lists)).

% Precisamos de adjacências (4-vizinhos em uma grade 12×12)
adjacente(X,Y,X1,Y) :-
    X1 is X+1, X1 =< 12.
adjacente(X,Y,X1,Y) :-
    X1 is X-1, X1 >= 1.
adjacente(X,Y,X,Y1) :-
    Y1 is Y+1, Y1 =< 12.
adjacente(X,Y,X,Y1) :-
    Y1 is Y-1, Y1 >= 1.

% handle_brisa/2:
%   Quando sinto brisa em (X,Y), todos os vizinhos não visitados podem ter poço
handle_brisa(X,Y) :-
    findall((Xn,Yn),
        (   adjacente(X,Y,Xn,Yn),
            \+ visitado(Xn,Yn),
            \+ certeza_poco(Xn,Yn),
            \+ possivel_poco(Xn,Yn)
        ),
        Lista),
    forall(member((A,B),Lista),
           assertz(possivel_poco(A,B)) ).

% handle_no_brisa/2:
%   Quando não sinto brisa em (X,Y), todos os vizinhos estão livres de poço
handle_no_brisa(X,Y) :-
    findall((Xn,Yn),
        (   adjacente(X,Y,Xn,Yn),
            \+ certeza_poco(Xn,Yn)
        ),
        Lista),
    forall(member((A,B),Lista),
           (   retractall(possivel_poco(A,B)),
               assertz(certeza_sem_poco(A,B))
           )).

% handle_passos/2:
%   Quando ouço passos em (X,Y), todos os vizinhos não visitados podem ter inimigo
handle_passos(X,Y) :-
    findall((Xn,Yn),
        (   adjacente(X,Y,Xn,Yn),
            \+ visitado(Xn,Yn),
            \+ certeza_sem_inimigo(Xn,Yn),
            \+ possivel_inimigo(Xn,Yn)
        ),
        Lista),
    forall(member((A,B),Lista),
           assertz(possivel_inimigo(A,B)) ).

% handle_no_passos/2:
%   Quando não ouço passos em (X,Y), todos os vizinhos estão livres de inimigo
handle_no_passos(X,Y) :-
    findall((Xn,Yn),
        (   adjacente(X,Y,Xn,Yn),
            \+ certeza_sem_inimigo(Xn,Yn)
        ),
        Lista),
    forall(member((A,B),Lista),
           (   retractall(possivel_inimigo(A,B)),
               assertz(certeza_sem_inimigo(A,B))
           )).

% handle_flash/2:
%   Quando vejo flash em (X,Y), todos os vizinhos não visitados podem ter morcego
handle_flash(X,Y) :-
    findall((Xn,Yn),
        (   adjacente(X,Y,Xn,Yn),
            \+ visitado(Xn,Yn),
            \+ certeza_sem_morcego(Xn,Yn),
            \+ possivel_morcego(Xn,Yn)
        ),
        Lista),
    forall(member((A,B),Lista),
           assertz(possivel_morcego(A,B)) ).

% handle_no_flash/2:
%   Quando não vejo flash em (X,Y), todos os vizinhos estão livres de morcego
handle_no_flash(X,Y) :-
    findall((Xn,Yn),
        (   adjacente(X,Y,Xn,Yn),
            \+ certeza_sem_morcego(Xn,Yn)
        ),
        Lista),
    forall(member((A,B),Lista),
           (   retractall(possivel_morcego(A,B)),
               assertz(certeza_sem_morcego(A,B))
           )).
% retorna uma célula segura ainda não visitada
proximo_seguro_nao_visitado(X,Y) :-
    seguro(X,Y),
    \+ visitado(X,Y).