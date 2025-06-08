# src/python/engine.py

from python.map_loader import load_map_to_prolog
from python.maze import Maze

def run_game(map_file: str, prolog_file: str):
    """
    1) carrega o mapa em Prolog (gera todos os fatos de perigo e ouro)
    2) inicializa o estado do agente em Prolog
    3) executa o loop principal de interação (Passo 7)
    """
    # 1) carrega o mapa e asserta todos os fatos de poço/inimigo/etc.
    pif = load_map_to_prolog(map_file, prolog_file)

    # 2) inicializa o estado do agente usando o predicado já definido em pitfall.pro
    pif.query("inicializa_estado.")

    # instancia Maze em Python para usar sense() e atualizar grid se pegar ouro
    maze = Maze.load_from_file(map_file)

    # (mostra estado inicial)
    q = list(pif.query("estado(X,Y,Dir,En,Pt)"))
    if q:
        X, Y, Dir, En, Pt = (
            q[0]["X"], q[0]["Y"], q[0]["Dir"], q[0]["En"], q[0]["Pt"]
        )
        print(f"[início] Posição=({X},{Y}), Direção={Dir}, Energia={En}, Pontos={Pt}")

    # 3) loop de interação até término
    while True:
        # 3.1) busca estado atual em Prolog
        res = list(pif.query("estado(X,Y,Dir,En,Pt)"))
        if not res:
            print("erro: estado não encontrado em Prolog.")
            break
        X = res[0]["X"]
        Y = res[0]["Y"]
        Dir = res[0]["Dir"]
        En = res[0]["En"]
        Pt = res[0]["Pt"]

        # 3.2) calcula percepções em Python (indexação: Python 0..11, Prolog 1..12)
        brisa, passos, flash_, brilho_, impacto = maze.sense((X-1, Y-1))

        # 3.3) limpa fatos antigos de percepção
        pif.retract_all("brisa(_,_)")
        pif.retract_all("som_passos(_,_)")
        pif.retract_all("flash(_,_)")
        pif.retract_all("brilho(_,_)")

        # 3.4) assert novas percepções no Prolog
        if brisa:
            pif.assert_fact(f"brisa({X},{Y})")
        if passos:
            pif.assert_fact(f"som_passos({X},{Y})")
        if flash_:
            pif.assert_fact(f"flash({X},{Y})")
        if brilho_:
            pif.assert_fact(f"brilho({X},{Y})")

        # 3.5) solicita a próxima ação a partir de next_action/1
        acao = pif.get_next_action()
        if acao is None:
            print("agente não encontrou ação viável – encerrando.")
            break

        # 3.6) executa a ação em Prolog via move/1
        pif.move_agent(acao)

        # 3.7) atualiza o grid Python se o agente pegar algo (ouro ou power-up)
        #    após move, resgatamos o novo estado para checar pontuação/energia
        res2 = list(pif.query("estado(X2,Y2,Dir2,En2,Pt2)"))
        if res2:
            X2, Y2, Dir2, En2, Pt2 = (
                res2[0]["X2"], res2[0]["Y2"], res2[0]["Dir2"],
                res2[0]["En2"], res2[0]["Pt2"]
            )
            # se o agente permaneceu na mesma célula e houve aumento de pontos,
            # provavelmente pegou ouro ou power-up: limpamos o símbolo no Maze.grid
            if (X2, Y2) == (X, Y) and Pt2 > Pt:
                maze.grid[Y-1][X-1] = '.'

            X, Y, Dir, En, Pt = X2, Y2, Dir2, En2, Pt2

        # 3.8) exibe log da jogada
        print(f"Posição: ({X},{Y}), Direção: {Dir}, Energia: {En}, Pontos: {Pt}, Ação: {acao}")

        # 3.9) critério de parada: energia zerada ou pontuação negativa/extrema
        if En <= 0:
            print("energia zerada – fim de jogo.")
            break
        # se quiser parar ao coletar todo o ouro, adapte aqui:
        # ex: if total_de_ouros == Pt // 1000: break

    print("jogo concluído.")
    return pif

if __name__ == "__main__":
    pif = run_game(
        map_file="maps/mapa_pitfall.txt",
        prolog_file="src/prolog/pitfall.pro"
    )