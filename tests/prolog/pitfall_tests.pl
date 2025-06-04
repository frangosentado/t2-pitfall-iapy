:- begin_tests(pitfall).

% -------------------------------
% setup_env/0: carrega mapa e inicializa estado
% -------------------------------
setup_env :-
    user:carrega_mapa_pitfall('mapa_pitfall.txt'),
    user:inicializa_estado.

% --- Testes de percepção ---

test(brisa_true, [setup(setup_env)]) :-
    user:brisa(2,3).

test(brisa_false, [setup(setup_env)]) :-
    \+ user:brisa(1,1).

test(som_passos_true, [setup(setup_env)]) :-
    user:som_passos(2,3).

test(som_passos_false, [setup(setup_env)]) :-
    \+ user:som_passos(1,1).

test(flash_true, [setup(setup_env)]) :-
    user:flash(3,2).

test(flash_false, [setup(setup_env)]) :-
    \+ user:flash(1,1).

test(brilho_true, [setup(setup_env)]) :-
    user:brilho(9,2).

test(brilho_false, [setup(setup_env)]) :-
    \+ user:brilho(1,1).

test(impacto_norte_true, [setup(setup_env)]) :-
    user:impacto(_,12,norte).

test(impacto_norte_false, [setup(setup_env)]) :-
    \+ user:impacto(_,11,norte).

test(impacto_sul_true, [setup(setup_env)]) :-
    user:impacto(_,1,sul).

test(impacto_sul_false, [setup(setup_env)]) :-
    \+ user:impacto(_,2,sul).

test(impacto_leste_true, [setup(setup_env)]) :-
    user:impacto(12,_,leste).

test(impacto_leste_false, [setup(setup_env)]) :-
    \+ user:impacto(11,_,leste).

test(impacto_oeste_true, [setup(setup_env)]) :-
    user:impacto(1,_,oeste).

test(impacto_oeste_false, [setup(setup_env)]) :-
    \+ user:impacto(2,_,oeste).

% --- Testes de ações (estado inicial = 1,1,norte,100,0) ---

test(estado_inicial, [setup(setup_env)]) :-
    user:estado(1,1,norte,100,0).

test(girar_direita, [setup(setup_env)]) :-
    user:virar_direita,
    user:estado(1,1,leste,100,-1).

test(girar_esquerda, [setup(setup_env)]) :-
    setup_env,  % volta ao estado inicial antes de girar à esquerda
    user:virar_esquerda,
    user:estado(1,1,oeste,100,-1).

test(pegar_powerup, [setup(setup_env)]) :-
    % em (1,1) há power-up
    user:pegar,
    user:estado(1,1,norte,120,-1).

test(pegar_vazio, [setup(setup_env)]) :-
    % primeiro pega o power-up para remover U
    user:pegar,
    % depois tenta pegar novamente, só gasta -1
    user:pegar,
    user:estado(1,1,norte,120,-2).

test(pegar_ouro, [setup(setup_env)]) :-
    % posiciona em (9,2) para pegar ouro
    user:retractall(user:estado(_,_,_,_,_)),
    user:assertz(user:estado(9,2,norte,50,0)),
    user:pegar,
    user:estado(9,2,norte,50,999).

test(andar_normal, [setup(setup_env)]) :-
    user:inicializa_estado,
    user:executa_acao(andar),
    user:estado(1,2,norte,100,-1).

test(colisao_parede, [setup(setup_env)]) :-
    user:retractall(user:estado(_,_,_,_,_)),
    user:assertz(user:estado(1,12,norte,50,0)),
    user:executa_acao(andar),
    user:estado(1,12,norte,50,-1).

test(poco, [setup(setup_env)]) :-
    user:retractall(user:estado(_,_,_,_,_)),
    user:assertz(user:estado(2,1,norte,80,0)),
    user:executa_acao(andar),
    user:estado(2,2,_,0,-1000).

test(inimigo20, [setup(setup_env)]) :-
    user:retractall(user:estado(_,_,_,_,_)),
    user:assertz(user:estado(2,3,norte,30,0)),
    user:executa_acao(andar),
    user:estado(2,4,_,10,-20).



test(morcego, [setup(setup_env)]) :-
    user:retractall(user:estado(_,_,_,_,_)),
    user:assertz(user:estado(5,5,norte,50,0)),
    user:executa_acao(andar),
    user:estado(X,Y,_,50,-1),
    X @>= 1, X @=< 12,
    Y @>= 1, Y @=< 12.

:- end_tests(pitfall).