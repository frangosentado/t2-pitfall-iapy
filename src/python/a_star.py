import sys
from collections import deque

def get_neighbors(x, y):
    # Retorna células vizinhas válidas no grid 12x12
    moves = [(1,0), (-1,0), (0,1), (0,-1)]
    neighbors = []
    for dx, dy in moves:
        nx, ny = x+dx, y+dy
        if 1 <= nx <= 12 and 1 <= ny <= 12:
            neighbors.append((nx, ny))
    return neighbors

def bfs(start, goal, safe_cells):
    queue = deque()
    queue.append((start, [start]))
    visited = set()
    visited.add(start)

    while queue:
        (x, y), path = queue.popleft()
        if (x, y) == goal:
            return path

        for nx, ny in get_neighbors(x, y):
            if (nx, ny) in safe_cells and (nx, ny) not in visited:
                visited.add((nx, ny))
                queue.append(((nx, ny), path + [(nx, ny)]))
    
    return []  # Se não encontrou caminho

def load_safe_cells():
    # Lê células seguras conhecidas a partir de um arquivo
    try:
        with open("safe_cells.txt", "r") as f:
            return set(tuple(map(int, line.strip().split(","))) for line in f if line.strip())
    except FileNotFoundError:
        print("[PYTHON] safe_cells.txt não encontrado")
        return set()

def main():
    if len(sys.argv) != 5:
        print("Uso: python3 a_star.py x0 y0 xf yf")
        return

    x0, y0, xf, yf = map(int, sys.argv[1:])
    safe_cells = load_safe_cells()
    path = bfs((x0, y0), (xf, yf), safe_cells)

    print("[PYTHON] Executar plano:")
    for (x, y) in path[1:]:  # ignora a posição atual
        print(f"goto({x},{y})")

if __name__ == "__main__":
    main()