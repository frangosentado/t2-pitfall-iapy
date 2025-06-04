import pytest
import os
import sys
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "../../src")))
from python.maze import Maze
def test_maze_dimensions():
    # verifica se o grid tem as dimensões corretas
    m = Maze(width=8, height=5, seed=0, num_enemies=0, num_pits=0, num_powerups=0, num_teleporters=0)
    assert m.width == 8
    assert m.height == 5
    assert len(m.grid) == 5
    assert all(len(row) == 8 for row in m.grid)


def test_border_walls():
    # verifica se as paredes de borda foram adicionadas
    m = Maze(width=6, height=6, seed=1, num_enemies=0, num_pits=0, num_powerups=0, num_teleporters=0)
    # todas as células no contorno devem ser '#'
    for x in range(6):
        assert m.grid[0][x] == '#'
        assert m.grid[5][x] == '#'
    for y in range(6):
        assert m.grid[y][0] == '#'
        assert m.grid[y][5] == '#'


def test_agent_initial_position():
    # agente deve estar sempre em (1,1)
    m = Maze(seed=42, num_enemies=0, num_pits=0, num_powerups=0, num_teleporters=0)
    assert m.agent_pos == (1, 1)
    assert m.grid[1][1] == 'A'


def test_seed_reproducibility():
    # com mesma seed, posições das entidades devem ser iguais
    seed = 123
    m1 = Maze(seed=seed, num_enemies=2, num_pits=2, num_powerups=1, num_teleporters=1)
    m2 = Maze(seed=seed, num_enemies=2, num_pits=2, num_powerups=1, num_teleporters=1)
    assert m1.enemies == m2.enemies
    assert m1.pits == m2.pits
    assert m1.powerups == m2.powerups
    assert m1.teleporters == m2.teleporters
    assert m1.gold_pos == m2.gold_pos


def test_display_format():
    # verifica se display retorna uma string com linhas separadas por '\n'
    m = Maze(width=4, height=4, seed=0, num_enemies=0, num_pits=0, num_powerups=0, num_teleporters=0)
    disp = m.display()
    lines = disp.split('\n')
    assert len(lines) == 4
    assert all(len(line) == 4 for line in lines)
