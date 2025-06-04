"""
map_loader.py

converte o objeto Maze em fatos Prolog e inicializa o estado do agente.
"""

from python.maze import Maze
from python.interface import PrologInterface

def load_map_to_prolog(map_file: str, prolog_file: str) -> PrologInterface:
    """
    1) carrega o arquivo de mapa (map_file) em Python usando Maze.load_from_file
    2) inicializa PrologInterface carregando pitfall.pro (o único arquivo Prolog)
    3) percorre cada célula do Maze.grid e faz assertz(...) para cada símbolo de perigo/ouro/power-up
    4) retorna a instância PrologInterface já com fatos de ambiente inseridos
    """
    # 1) carregar labirinto em Python
    maze = Maze.load_from_file(map_file)

    # 2) criar interface Prolog e carregar pitfall.pro
    pif = PrologInterface(prolog_file)

    # 3) percorrer grid para gerar fatos Prolog
    # Prolog indexa de 1, Python de 0 -> soma +1
    for y in range(maze.height):
        for x in range(maze.width):
            symbol = maze.grid[y][x]
            px, py = x + 1, y + 1

            if symbol == 'P':
                pif.assert_fact(f"poco({px},{py})")
            elif symbol == 'E':
                pif.assert_fact(f"inimigo20({px},{py})")
            elif symbol == 'D':
                pif.assert_fact(f"inimigo50({px},{py})")
            elif symbol == 'U':
                pif.assert_fact(f"morcego({px},{py})")
            elif symbol == 'O':
                pif.assert_fact(f"ouro({px},{py})")
            elif symbol == '+':
                pif.assert_fact(f"powerup({px},{py})")
            # '#' e '.' não precisam de assert, pois walls e espaços já estão definidos em pitfall.pro

    return pif