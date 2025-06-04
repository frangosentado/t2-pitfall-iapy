import pytest
import os
import sys
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "../../src")))
from python.maze import Maze

# este mapa 5×5 tem bordas de parede (#) e alguns elementos internos:
#  posição    comentário
#  (2,1) = P  → poço
#  (3,2) = E  → inimigo
#  (1,3) = T  → teletransportador
#  (2,3) = G  → ouro
#
#   0 1 2 3 4
# 0 # # # # #
# 1 # . P . #
# 2 # . . E #
# 3 # T G . #
# 4 # # # # #
TEST_GRID = [
    ['#', '#', '#', '#', '#'],
    ['#', '.', 'P', '.', '#'],
    ['#', '.', '.', 'E', '#'],
    ['#', 'T', 'G', '.', '#'],
    ['#', '#', '#', '#', '#'],
]

@pytest.fixture
def custom_maze(tmp_path):
    # salva esse layout em arquivo temporário
    path = tmp_path / "custom_map.txt"
    with open(path, 'w') as f:
        for row in TEST_GRID:
            f.write(''.join(row) + "\n")
    return Maze.load_from_file(str(path))

def test_percept_at_start(custom_maze):
    # na posição (1,1): 
    # adjacentes: (1,0)=# (impacto), (1,2)='.' , (0,1)='#'(impacto), (2,1)='P'(brisa)
    # brisa=True (poço adjacente), passos=False, flash=False, brilho=False, impacto=True
    percept = custom_maze.sense((1, 1))
    assert percept == [True, False, False, False, True]

def test_percept_near_enemy(custom_maze):
    # na posição (2,2):
    # adjacentes: (2,1)='P'(brisa), (2,3)='G'(brilho na própria célula), (1,2)='.' , (3,2)='E'(passos)
    # brisa=True, passos=True, flash=False, brilho=False (porque brilho só se a célula atual tiver 'G'), impacto=False
    percept = custom_maze.sense((2, 2))
    assert percept == [True, True, False, False, False]

def test_percept_on_gold(custom_maze):
    # na posição (2,3) onde há ouro
    # adjacentes: (2,2)='.', (2,4)='#'(impacto), (1,3)='T'(flash), (3,3)='.'
    # brisa=False, passos=False, flash=True, brilho=True (há 'G' na própria célula), impacto=True
    percept = custom_maze.sense((2, 3))
    assert percept == [False, False, True, True, True]

def test_percept_on_teleporter(custom_maze):
    # na posição (1,3) onde há teletransportador
    # adjacentes: (1,2)='.', (1,4)='#'(impacto), (0,3)='#'(impacto), (2,3)='G'(brilho na adjacente, mas brilho considera só a própria célula)
    # brisa=False, passos=False, flash=False (flash só se adjacente tiver T; aqui T está na própria célula, mas não conta), 
    # brilho=False, impacto=True
    percept = custom_maze.sense((1, 3))
    assert percept == [False, False, False, False, True]

def test_percept_at_wall_edge(custom_maze):
    # na posição (1,0) que é parede:
    # adjacentes: (1,-1) Fora de grid → ignora, (1,1)='.', (0,0)='#', (2,0)='#' 
    # brisa=False, passos=False, flash=False, brilho=False, impacto=True
    percept = custom_maze.sense((1, 0))
    assert percept == [False, False, False, False, True]