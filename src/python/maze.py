import random

class Maze:
    """
    representa o grid do ambiente para o agente Pitfall.
    as células podem conter: paredes (#), agente (A), inimigos (E), poços (P), power-ups (+), teletransporters (T), ouro (G) e espaço vazio (.).
    """
    def __init__(self, width=12, height=12, seed=None,
                 num_enemies=3, num_pits=3, num_powerups=2, num_teleporters=2):
        """
        inicializa o labirinto.
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

        # posiciona elementos dinâmicos
        self._place_elements(num_enemies, num_pits, num_powerups, num_teleporters)

    def _add_border_walls(self):
        for x in range(self.width):
            self.grid[0][x] = '#'
            self.grid[self.height - 1][x] = '#'
        for y in range(self.height):
            self.grid[y][0] = '#'
            self.grid[y][self.width - 1] = '#'

    def _place_elements(self, num_enemies, num_pits, num_powerups, num_teleporters):
        """
        posiciona aleatoriamente o agente, ouro, inimigos, poços, power-ups e teleporters.
        agente fixo em (1,1).
        """
        # coloca agente na posição inicial
        self.agent_pos = (1, 1)
        self.grid[1][1] = 'A'

        # coloca o ouro
        self.gold_pos = self._place_random('G')

        # coloca inimigos
        for _ in range(num_enemies):
            pos = self._place_random('E')
            self.enemies.append(pos)

        # coloca poços
        for _ in range(num_pits):
            pos = self._place_random('P')
            self.pits.append(pos)

        # coloca power-ups
        for _ in range(num_powerups):
            pos = self._place_random('+')
            self.powerups.append(pos)

        # coloca teleporters
        for _ in range(num_teleporters):
            pos = self._place_random('T')
            self.teleporters.append(pos)

    def _place_random(self, symbol):
        """
        coloca um símbolo em uma célula aleatória vazia e retorna sua posição.
        """
        while True:
            x = random.randint(1, self.width - 2)
            y = random.randint(1, self.height - 2)
            if self.grid[y][x] == '.':
                self.grid[y][x] = symbol
                return (x, y)

    def display(self):
        """
        retorna a representação em string do grid do labirinto.
        """
        return '\n'.join(''.join(row) for row in self.grid)


if __name__ == "__main__":
    # demo
    maze = Maze(seed=42)
    print(maze.display())
