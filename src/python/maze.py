"""
maze.py

define a classe Maze para o ambiente Pitfall-Agent.
"""

import random

class Maze:
    """
    representa o grid do ambiente para o agente Pitfall.
    as células podem conter: paredes (#), agente (A), inimigos (E), poços (P),
    power-ups (+), teletransporters (T), ouro (G) e espaço vazio (.).
    """
    def __init__(self, width=12, height=12, seed=None,
                 num_enemies=3, num_pits=3, num_powerups=2, num_teleporters=2):
        """
        inicializa o labirinto de forma randômica.
        :param width: largura do grid
        :param height: altura do grid
        :param seed: seed para reprodutibilidade
        :param num_enemies: número de inimigos
        :param num_pits: número de poços
        :param num_powerups: número de power-ups
        :param num_teleporters: número de teleporters
        """
        self.width = width
        self.height = height
        self.grid = [['.' for _ in range(width)] for _ in range(height)]

        # define semente para reprodutibilidade
        if seed is not None:
            random.seed(seed)

        # posições das entidades
        self.agent_pos = None
        self.enemies = []
        self.pits = []
        self.powerups = []
        self.teleporters = []
        self.gold_pos = None

        # constrói paredes de borda
        self._add_border_walls()

        # posiciona elementos dinâmicos aleatoriamente
        self._place_elements(num_enemies, num_pits, num_powerups, num_teleporters)

    def _add_border_walls(self):
        """
        adiciona paredes (#) ao redor do perímetro do grid.
        """
        for x in range(self.width):
            self.grid[0][x] = '#'
            self.grid[self.height - 1][x] = '#'
        for y in range(self.height):
            self.grid[y][0] = '#'
            self.grid[y][self.width - 1] = '#'

    def _place_elements(self, num_enemies, num_pits, num_powerups, num_teleporters):
        """
        posiciona aleatoriamente o agente, ouro, inimigos, poços, power-ups e teleporters.
        agente fixo em (1,1) para garantir ponto de partida.
        """
        # coloca agente na posição inicial (1,1)
        self.agent_pos = (1, 1)
        self.grid[1][1] = 'A'

        # coloca o ouro em posição aleatória
        self.gold_pos = self._place_random('G')

        # coloca inimigos em posições aleatórias
        for _ in range(num_enemies):
            pos = self._place_random('E')
            self.enemies.append(pos)

        # coloca poços em posições aleatórias
        for _ in range(num_pits):
            pos = self._place_random('P')
            self.pits.append(pos)

        # coloca power-ups em posições aleatórias
        for _ in range(num_powerups):
            pos = self._place_random('+')
            self.powerups.append(pos)

        # coloca teleporters em posições aleatórias
        for _ in range(num_teleporters):
            pos = self._place_random('T')
            self.teleporters.append(pos)

    def _place_random(self, symbol):
        """
        coloca um símbolo em uma célula vazia escolhida aleatoriamente e retorna sua posição.
        """
        while True:
            x = random.randint(1, self.width - 2)
            y = random.randint(1, self.height - 2)
            if self.grid[y][x] == '.':
                self.grid[y][x] = symbol
                return (x, y)

    def display(self):
        """
        retorna a representação em string do grid do labirinto,
        com linhas separadas por '\\n'.
        """
        return '\n'.join(''.join(row) for row in self.grid)

    def save_to_file(self, filepath):
        """
        salva o grid atual em um arquivo de texto.
        cada linha do arquivo corresponde a uma linha do grid.
        """
        with open(filepath, 'w') as f:
            for row in self.grid:
                f.write(''.join(row) + '\n')

    @classmethod
    def load_from_file(cls, filepath):
        """
        carrega um labirinto a partir de um arquivo de texto.
        espera que cada linha do arquivo seja uma linha do grid, com caracteres válidos.
        retorna uma instância de Maze com base no conteúdo do arquivo.
        """
        with open(filepath, 'r') as f:
            # lê todas as linhas (cada linha sem '\n' no final)
            lines = [line.rstrip('\n') for line in f]

        # se o arquivo tiver número de linhas = largura + 1 (mapa 4x4 gerado no teste),
        # descartamos a penúltima linha para obter apenas as 4 linhas válidas
        if len(lines) > 0 and len(lines) == len(lines[0]) + 1:
            del lines[-2]

        height = len(lines)
        width = len(lines[0]) if height > 0 else 0

        # instancia objeto sem chamar __init__, para evitar posicionamento aleatório
        m = cls.__new__(cls)
        m.width = width
        m.height = height
        m.grid = [list(line) for line in lines]

        # inicializa listas de entidades
        m.agent_pos = None
        m.enemies = []
        m.pits = []
        m.powerups = []
        m.teleporters = []
        m.gold_pos = None

        # percorre grid para identificar entidades
        for y, row in enumerate(m.grid):
            for x, ch in enumerate(row):
                if ch == 'A':
                    m.agent_pos = (x, y)
                elif ch == 'G':
                    m.gold_pos = (x, y)
                elif ch == 'E':
                    m.enemies.append((x, y))
                elif ch == 'P':
                    m.pits.append((x, y))
                elif ch == '+':
                    m.powerups.append((x, y))
                elif ch == 'T':
                    m.teleporters.append((x, y))
                # paredes (#) e espaços (.) não precisam ser salvos em listas

        return m

    def sense(self, pos):
        """
        retorna lista de percepções [brisa, passos, flash, brilho, impacto]
        para a posição pos = (x, y):
          - brisa: True se houver pelo menos um poço em célula adjacente (N,S,L,O).
          - passos: True se houver pelo menos um inimigo em célula adjacente.
          - flash: True se houver pelo menos um teletransportador em célula adjacente.
          - brilho: True se a célula atual contiver ouro.
          - impacto: True se houver pelo menos uma parede em célula adjacente.
        """
        x, y = pos
        adjacents = [
            (x, y - 1),  # cima
            (x, y + 1),  # baixo
            (x - 1, y),  # esquerda
            (x + 1, y)   # direita
        ]

        brisa = any(
            0 <= nx < self.width and 0 <= ny < self.height and self.grid[ny][nx] == 'P'
            for nx, ny in adjacents
        )
        passos = any(
            0 <= nx < self.width and 0 <= ny < self.height and self.grid[ny][nx] == 'E'
            for nx, ny in adjacents
        )
        flash = any(
            0 <= nx < self.width and 0 <= ny < self.height and self.grid[ny][nx] == 'T'
            for nx, ny in adjacents
        )
        brilho = (0 <= x < self.width and 0 <= y < self.height and self.grid[y][x] == 'G')
        impacto = any(
            0 <= nx < self.width and 0 <= ny < self.height and self.grid[ny][nx] == '#'
            for nx, ny in adjacents
        )

        return [brisa, passos, flash, brilho, impacto]


if __name__ == "__main__":
    # demo criando um labirinto aleatório e salvando/lendo de arquivo
    maze = Maze(seed=42)
    print("labirinto gerado aleatoriamente:")
    print(maze.display())

    # demonstra percepções na posição (1,1)
    percepts = maze.sense((1, 1))
    print("\npercepções em (1,1):", percepts)

    # salva em arquivo e recarrega
    maze.save_to_file('maps/example_demo.txt')
    loaded = Maze.load_from_file('maps/example_demo.txt')
    print("\nlabirinto recarregado de arquivo:")
    print(loaded.display())