% pitfall.pro – Versão ajustada para limpar fatos antigos e manter pontuação/energia conforme esperado

% -------------------------------
% Declarações dinâmicas (Pitfall)
% -------------------------------
:- dynamic
    poco/2,        % poço (morte imediata)
    inimigo20/2,   % inimigo que tira 20 de energia
    inimigo50/2,   % inimigo que tira 50 de energia
    morcego/2,     % teletransportador (quando o agente entra, teleporta)
    ouro/2,        % pepita de ouro (+1000 de pontuação)
    powerup/2,     % power-up (+20 de energia)
    estado/5.      % estado(X, Y, Direcao, Energia, Pontuacao)

% -------------------------------
% Rotina para carregar um mapa de texto 12×12
% -------------------------------

% carrega_mapa_pitfall(+Arquivo)
%   Antes de abrir o arquivo, limpa todos os fatos dinâmicos
%   (poço, inimigos, morcegos, ouro, power‐up). Depois, lê cada
%   linha e assertz/1 conforme o caractere encontrado.
%
carrega_mapa_pitfall(File) :-
    % Remove quaisquer fatos antigos antes de reassertar
    retractall(poco(_,_)),
    retractall(inimigo20(_,_)),
    retractall(inimigo50(_,_)),
    retractall(morcego(_,_)),
    retractall(ouro(_,_)),
    retractall(powerup(_,_)),
    open(File, read, Stream),
    carrega_linhas(Stream, 1),
    close(Stream).

carrega_linhas(Stream, Y) :-
    Y =< 12,
    read_line_to_string(Stream, Line),
    string_chars(Line, Chars),
    carrega_colunas(Chars, Y, 1),
    Y2 is Y + 1,
    carrega_linhas(Stream, Y2).
carrega_linhas(_, Y) :- Y > 12.

carrega_colunas([C|Rest], Y, X) :-
    interpreta_char(C, X, Y),
    X2 is X + 1,
    carrega_colunas(Rest, Y, X2).
carrega_colunas([], _, _) :- !.

interpreta_char('P', X, Y) :- assertz(poco(X,Y)).
interpreta_char('d', X, Y) :- assertz(inimigo20(X,Y)).
interpreta_char('D', X, Y) :- assertz(inimigo50(X,Y)).
interpreta_char('T', X, Y) :- assertz(morcego(X,Y)).
interpreta_char('O', X, Y) :- assertz(ouro(X,Y)).
interpreta_char('U', X, Y) :- assertz(powerup(X,Y)).
interpreta_char('.', _, _) :- !.  % sala vazia
interpreta_char(_, _, _) :- !.     % ignora outros caracteres

% -------------------------------
% 3.1) Percepções do Pitfall
% -------------------------------

% brisa(X,Y): existe um poço numa das 4 vizinhas de (X,Y)?
brisa(X,Y) :-
    X1 is X+1,   poco(X1,Y), !;
    X2 is X-1,   poco(X2,Y), !;
    Y1 is Y+1,   poco(X,Y1), !;
    Y2 is Y-1,   poco(X,Y2), !.

% som_passos(X,Y): existe um inimigo (20 ou 50) em alguma vizinha de (X,Y)?
som_passos(X,Y) :-
    X1 is X+1,   (inimigo20(X1,Y); inimigo50(X1,Y)), !;
    X2 is X-1,   (inimigo20(X2,Y); inimigo50(X2,Y)), !;
    Y1 is Y+1,   (inimigo20(X,Y1); inimigo50(X,Y1)), !;
    Y2 is Y-1,   (inimigo20(X,Y2); inimigo50(X,Y2)), !.

% flash(X,Y): existe um morcego em alguma vizinha de (X,Y)?
flash(X,Y) :-
    X1 is X+1,   morcego(X1,Y), !;
    X2 is X-1,   morcego(X2,Y), !;
    Y1 is Y+1,   morcego(X,Y1), !;
    Y2 is Y-1,   morcego(X,Y2), !.

% brilho(X,Y): há pepita de ouro NA SALA atual (X,Y)?
brilho(X,Y) :- ouro(X,Y).

% impacto(X,Y,Dir): sinaliza se (X,Y), olhando para Dir, está diante de uma parede (fora dos limites 1..12)
impacto(_,Y,norte) :- Y =:= 12.
impacto(_,Y,sul)   :- Y =:= 1.
impacto(X,_,leste) :- X =:= 12.
impacto(X,_,oeste) :- X =:= 1.

% -------------------------------
% 3.2) Estado inicial
% -------------------------------

% estado(X, Y, Direcao, Energia, Pontuacao)
%  – Direcao ∈ {norte, sul, leste, oeste}
%  – Energia inicia em 100, Pontuação em 0
inicializa_estado :-
    retractall(estado(_,_,_,_,_)),
    assertz(estado(1,1,norte,100,0)).

% -------------------------------
% 3.3) Ações do agente
% -------------------------------

% movimento(Direcao, X, Y, Xn, Yn, Impacto)
%  – calcula (Xn,Yn) a partir de (X,Y) se “andar” em Direcao;
%  – Impacto = sim se sair do grid 1..12, caso contrário Impacto = nao.
movimento(norte, X, Y, X, Yn, sim) :- Yn is Y + 1, Yn > 12.
movimento(norte, X, Y, X, Yn, nao) :- Yn is Y + 1, Yn =< 12.

movimento(sul, X, Y, X, Yn, sim) :- Yn is Y - 1, Yn < 1.
movimento(sul, X, Y, X, Yn, nao) :- Yn is Y - 1, Yn >= 1.

movimento(leste, X, Y, Xn, Y, sim) :- Xn is X + 1, Xn > 12.
movimento(leste, X, Y, Xn, Y, nao) :- Xn is X + 1, Xn =< 12.

movimento(oeste, X, Y, Xn, Y, sim) :- Xn is X - 1, Xn < 1.
movimento(oeste, X, Y, Xn, Y, nao) :- Xn is X - 1, Xn >= 1.

% girar_direita: gira 90° no sentido horário, custa -1 de pontuação
virar_direita :-
    estado(X,Y,Dir,En,Pt),
    Pt1 is Pt - 1,
    proxima_direcao(Dir, direita, DirNova),
    retractall(estado(_,_,_,_,_)),
    assertz(estado(X,Y,DirNova,En,Pt1)).

% girar_esquerda: gira 90° no sentido anti-horário, custa -1 de pontuação
virar_esquerda :-
    estado(X,Y,Dir,En,Pt),
    Pt1 is Pt - 1,
    proxima_direcao(Dir, esquerda, DirNova),
    retractall(estado(_,_,_,_,_)),
    assertz(estado(X,Y,DirNova,En,Pt1)).

% proxima_direcao(DirAtual, AcaoGiro, DirNova)
proxima_direcao(norte, direita, leste).
proxima_direcao(leste, direita, sul).
proxima_direcao(sul, direita, oeste).
proxima_direcao(oeste, direita, norte).

proxima_direcao(norte, esquerda, oeste).
proxima_direcao(oeste, esquerda, sul).
proxima_direcao(sul, esquerda, leste).
proxima_direcao(leste, esquerda, norte).

% pegar: se tiver ouro ou power-up em (X,Y), colhe e ajusta pontuação/energia
pegar :-
    estado(X,Y,Dir,En,Pt),
    ( ouro(X,Y) ->
        retract(ouro(X,Y)),
        Pt1 is Pt + 1000 - 1,    % pega ouro: +1000 de pontuação, -1 de custo
        retractall(estado(_,_,_,_,_)),
        assertz(estado(X,Y,Dir,En,Pt1))
    ; powerup(X,Y) ->
        retract(powerup(X,Y)),
        En1 is En + 20,         % pega power-up: +20 de energia, -1 de custo
        Pt1 is Pt - 1,
        retractall(estado(_,_,_,_,_)),
        assertz(estado(X,Y,Dir,En1,Pt1))
    ;   % se não houver nada para pegar, apenas gasta 1 ponto
        Pt1 is Pt - 1,
        retractall(estado(_,_,_,_,_)),
        assertz(estado(X,Y,Dir,En,Pt1))
    ), !.

% executar AÇÃO, atualizando estado e tratando eventos na nova posição
executa_acao(andar) :-
    estado(X,Y,Dir,En,Pt),
    movimento(Dir, X, Y, Xn, Yn, Impacto),
    ( Impacto = sim ->
        % bateu na parede: permanece em (X,Y), -1 ponto apenas
        Pt1 is Pt - 1,
        retractall(estado(_,_,_,_,_)),
        assertz(estado(X,Y,Dir,En,Pt1))
    ; % caso não bata:
        ( poco(Xn,Yn) ->
            % poço: energia vai a 0, pontuação = Pontuação atual - 1000
            En1 is 0,
            Pt1 is Pt - 1000,
            retractall(estado(_,_,_,_,_)),
            assertz(estado(Xn,Yn,Dir,En1,Pt1))
        ; inimigo20(Xn,Yn) ->
            % inimigo20: subtrai 20 de energia; se morrer, -1000 de pontuação
            En2 is En - 20,
            ( En2 =< 0 ->
                En3 is 0,
                Pt2 is Pt - 1000,
                retractall(estado(_,_,_,_,_)),
                assertz(estado(Xn,Yn,Dir,En3,Pt2))
            ;
                Pt2 is Pt - 20,   % causa -20 de pontuação
                retractall(estado(_,_,_,_,_)),
                assertz(estado(Xn,Yn,Dir,En2,Pt2))
            )
        ; inimigo50(Xn,Yn) ->
            % inimigo50: subtrai 50 de energia; se morrer, -1000 de pontuação
            En4 is En - 50,
            ( En4 =< 0 ->
                En5 is 0,
                Pt3 is Pt - 1000,
                retractall(estado(_,_,_,_,_)),
                assertz(estado(Xn,Yn,Dir,En5,Pt3))
            ;
                Pt3 is Pt - 50,
                retractall(estado(_,_,_,_,_)),
                assertz(estado(Xn,Yn,Dir,En4,Pt3))
            )
        ; morcego(Xn,Yn) ->
            % morcego: teleporta aleatoriamente, -1 ponto apenas
            Pt1 is Pt - 1,
            random_between(1,12, Xt),
            random_between(1,12, Yt),
            retractall(estado(_,_,_,_,_)),
            assertz(estado(Xt,Yt,Dir,En,Pt1))
        ;   % célula vazia (ou contêm power-up/ouro, mas isso só é tratado quando chamar pegar/0)
            Pt1 is Pt - 1,
            retractall(estado(_,_,_,_,_)),
            assertz(estado(Xn,Yn,Dir,En,Pt1))
        )
    ).

% -------------------------------
% (Fim do arquivo)
% -------------------------------