% knowledge.pl – Base de Conhecimento (KB) do agente Pitfall

:- dynamic
    visitado/2,           % visitado(X,Y)
    possivel_poco/2,      % possivel_poco(X,Y)
    certeza_poco/2,       % certeza_poco(X,Y)
    possivel_inimigo/2,   % possivel_inimigo(X,Y)
    certeza_sem_inimigo/2,% certeza_sem_inimigo(X,Y)
    possivel_morcego/2,   % possivel_morcego(X,Y)
    certeza_sem_morcego/2,% certeza_sem_morcego(X,Y)
    possivel_ouro/2,      % possivel_ouro(X,Y)
    certeza_sem_ouro/2,   % certeza_sem_ouro(X,Y)
    seguro/2.             % seguro(X,Y)

% inicializa_kb/0 – limpa toda a KB e marca a posição inicial (1,1) como visitada e segura
inicializa_kb :-
    retractall(visitado(_,_)),
    retractall(possivel_poco(_,_)),
    retractall(certeza_poco(_,_)),
    retractall(possivel_inimigo(_,_)),
    retractall(certeza_sem_inimigo(_,_)),
    retractall(possivel_morcego(_,_)),
    retractall(certeza_sem_morcego(_,_)),
    retractall(possivel_ouro(_,_)),
    retractall(certeza_sem_ouro(_,_)),
    retractall(seguro(_,_)),
    assertz(visitado(1,1)),
    assertz(seguro(1,1)).
