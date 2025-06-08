:- use_module(library(lists)).

adjacente(X,Y,X1,Y) :-
    X1 is X+1, X1 =< 12.
adjacente(X,Y,X1,Y) :-
    X1 is X-1, X1 >= 1.
adjacente(X,Y,X,Y1) :-
    Y1 is Y+1, Y1 =< 12.
adjacente(X,Y,X,Y1) :-
    Y1 is Y-1, Y1 >= 1.

update_kb(X,Y) :-
    ( \+ visitado(X,Y) -> assertz(visitado(X,Y)) ; true ),
    (   brisa(X,Y)
    ->  handle_brisa(X,Y)
    ;   handle_no_brisa(X,Y)
    ),
    (   som_passos(X,Y)
    ->  handle_passos(X,Y)
    ;   handle_no_passos(X,Y)
    ),
    (   flash(X,Y)
    ->  handle_flash(X,Y)
    ;   handle_no_flash(X,Y)
    ),
    (   brilho(X,Y)
    ->  ( \+ seguro(X,Y) -> assertz(seguro(X,Y)) ; true )
    ;   true
    ),
    findall((Xn,Yn),
        (   adjacente(X,Y,Xn,Yn),
            \+ visitado(Xn,Yn),
            certeza_sem_poco(Xn,Yn),
            certeza_sem_inimigo(Xn,Yn),
            certeza_sem_morcego(Xn,Yn)
        ),
        VizinhosSeguros),
    forall(member((A,B),VizinhosSeguros),
           ( \+ seguro(A,B) -> assertz(seguro(A,B)) ; true )).

decide_next_action(X,Y,Dir) :-
    (   brilho(X,Y)
    ->  format('  --> PERCEPÇÃO: brilho em (~w,~w). Ação: pegar~n', [X,Y]),
        executa_acao(pegar)
    ;
        (   adjacente(X,Y,Xn,Yn),
            seguro(Xn,Yn),
            \+ visitado(Xn,Yn)
        ->  format('  --> VIZINHO SEGURO encontrado (~w,~w). Ação: turn_and_move_to~n', [Xn,Yn]),
            turn_and_move_to(X,Y,Dir,Xn,Yn)
        ;
            (   proximo_seguro_nao_visitado(Xs,Ys)
            ->  format('  --> Nenhum vizinho seguro, mas há célula segura em (~w,~w). Usar planejamento.~n', [Xs,Ys]),
                planejar_ate(X,Y,Dir,Xs,Ys)
            ;   format('  --> Nenhuma ação possível a partir de (~w,~w).~n', [X,Y]),
                !, fail
            )
        )
    ).

turn_and_move_to(X,Y,Dir,Xn,Yn) :-
    (   Xn =:= X,  Yn =:= Y+1   -> DirDesejada = norte
    ;   Xn =:= X,  Yn =:= Y-1   -> DirDesejada = sul
    ;   Xn =:= X+1, Yn =:= Y    -> DirDesejada = leste
    ;   Xn =:= X-1, Yn =:= Y    -> DirDesejada = oeste
    ),
    align_direction(Dir, DirDesejada),
    format('    (girou para ~w e agora anda para (~w,~w))~n', [DirDesejada,Xn,Yn]),
    executa_acao(andar).

align_direction(Dir, Dir) :- !.
align_direction(DirAtual, DirDesejada) :-
    virar_direita,
    estado(_,_,NovoDir,_,_),
    (   NovoDir = DirDesejada
    ->  true
    ;   align_direction(NovoDir, DirDesejada)
    ).

start_agent :-
    carrega_mapa_pitfall('mapa_pitfall.txt'),
    inicializa_estado,
    inicializa_kb,
    writeln('--- Início da Execução ---'),
    loop_agent.

loop_agent :-
    (   estado(X,Y,Dir,En,Pt)
    ->  (   En =< 0
        ->  format('    *** AGENTE MORREU em (~w,~w). Pontuação final: ~w~n', [X,Y,Pt])
        ;   findall((A,B), ouro(A,B), ListaDeOuros),
            (   ListaDeOuros = []
            ->  format('    *** TODO OURO RECOLHIDO! Pontuação final: ~w~n', [Pt])
            ;   format('[Estado atual: (~w,~w) Dir=~w  E=~w  P=~w]~n', [X,Y,Dir,En,Pt]),
                percepcao(Perc),
                format('  Percepções em (~w,~w): ~w~n', [X,Y,Perc]),
                update_kb(X,Y),
                (   decide_next_action(X,Y,Dir)
                ->  sleep(1),
                    loop_agent
                ;   format('    *** ATENÇÃO: sem ação válida em (~w,~w); encerrar.~n', [X,Y])
                )
            )
        )
    ;   writeln('    *** ERRO: estado/5 não encontrado. Abortando.')
    ).
% planejar_ate/5 atualizado: lê comandos linha a linha e executa
planejar_ate(X,Y,_,Xf,Yf) :-
    exporta_celulas_seguras('safe_cells.txt'),
    format(atom(Cmd), 'python3 src/python/a_star.py ~w ~w ~w ~w > caminho.txt', [X,Y,Xf,Yf]),
    shell(Cmd),
    open('caminho.txt', read, Stream),
    estado(_,_,Dir,_,_),
    processar_caminho(Stream, Dir),
    close(Stream).
% processar_caminho(+Stream, +DirAtual)
processar_caminho(Stream, DirAtual) :-
    read_line_to_string(Stream, Line),
    (   Line == end_of_file
    ->  true
    ;   (   sub_string(Line, 0, 5, _, "goto(")
        ->  sub_atom(Line, 5, _, 1, Args),
            split_string(Args, ",", "", [Xs,Ys]),
            number_string(X, Xs),
            number_string(Y, Ys),
            estado(Xa,Ya,_,_,_),
            turn_and_move_to(Xa,Ya,DirAtual,X,Y),
            estado(_,_,DirNovo,_,_),           % atualiza a direção após o movimento
            processar_caminho(Stream, DirNovo)
        ;   processar_caminho(Stream, DirAtual)
        )
    ). 
exporta_celulas_seguras(Arquivo) :-
    open(Arquivo, write, Stream),
    forall(seguro(X,Y),
           format(Stream, '~w,~w~n', [X,Y])),
    close(Stream).