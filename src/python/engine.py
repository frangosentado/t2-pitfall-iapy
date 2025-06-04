# src/python/engine.py

from python.map_loader import load_map_to_prolog
from python.maze import Maze

def run_game(map_file: str, prolog_files: list):
    """
    1) carrega o mapa em Prolog (gera todos os fatos de perigo e ouro)
    2) inicializa o estado do agente em Prolog
    3) entra no loop principal (Passo 7, que será implementado a seguir)
    """
    # 1) carrega o mapa e asserta todos os fatos de poço/inimigo/etc.
    pif = load_map_to_prolog(map_file, prolog_files)

    # 2) inicializa o estado do agente (sempre em (1,1), norte, 100 energia, 0 pontos)
    pif.retract_all("estado(_,_,_,_,_)")
    pif.assert_fact("estado(1,1,norte,100,0)")

    # (aqui, se quiser, já busque posição atual e percepções para visualizar suposição inicial)
    q = list(pif.query("estado(X,Y,Dir,En,Pt)"))
    if q:
        X, Y, Dir, En, Pt = q[0]["X"], q[0]["Y"], q[0]["Dir"], q[0]["En"], q[0]["Pt"]
        print(f"[início] Posição=({X},{Y}), Direção={Dir}, Energia={En}, Pontos={Pt}")

    # 3) daqui em diante começaria o loop de iterações (chamaremos Passo 7)
    #     mas como este Passo 6 está focado apenas na carga do mapa e estado,
    #     podemos parar aqui e considerar esta etapa concluída.
    return pif

if __name__ == "__main__":
    pif = run_game(
        map_file="maps/mapa_pitfall.txt",
        prolog_files=[
            "src/prolog/pitfall.pro"
        ]
    )
    # (Em seguida, chamaremos o laço principal do Step 7...)