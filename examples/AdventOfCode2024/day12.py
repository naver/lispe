from collections import deque

inp = []
with open('data/day12.txt', 'r') as f:
  for line in f:
    inp.append(list(line.strip()))

# print(inp)
num_rows = len(inp)
num_cols = len(inp[0])

def in_bounds(rc):
  r, c = rc
  return (0 <= r < num_rows) and (0 <= c < num_cols)

def get_plant(rc):
  r, c = rc
  return inp[r][c]

def get_neighbors(rc):
  r, c = rc
  neighbors = []
  ds = [(-1, 0), (0, 1), (1, 0), (0, -1)] # NESW
  for (dr, dc) in ds:
    neighbors.append((r + dr, c + dc))
  return [n for n in neighbors if in_bounds(n)]

def get_plant_neighbors(rc):
  neighbors = get_neighbors(rc)
  return [n for n in neighbors if get_plant(n)==get_plant(rc)]

# BFS
def get_region(rc):
  visited = set()
  region = set()
  queue = deque([rc])
  while queue:
    node = queue.popleft()
    if node not in visited:
      visited.add(node)
      # visit node
      region.add(node)
      # add all unvisited neighbors to the queue
      neighbors = get_plant_neighbors(node)
      unvisited_neighbors = [n for n in neighbors if n not in visited]
      # print(f'At node {node}, ns: {neighbors}, unvisited: {unvisited_neighbors}')
      queue.extend(unvisited_neighbors)
  return region


def calc_edges(region):
  edges = 0
  for (r, c) in region:
    north_n = (r - 1, c) # TODO: Add const NORTH vector of (-1, 0)
    west_n = (r, c - 1)
    nw_n = (r - 1, c - 1)
    # TODO: Do this once, rotate 90 degrees and do it in a loop
    if (north_n not in region):
      # Top is an edge. But is it a new edge?
      # it's the same edge if the spot west of plot is in_bounds
      # and the NW plot is not the same plant (or is out of bounds)
      same_edge = (west_n in region) and (nw_n not in region)
      if not same_edge:
        edges += 1

    south_n = (r + 1, c)
    sw_n = (r + 1, c - 1)
    if south_n not in region:
      # bottom is an edge
      same_edge = (west_n in region) and (sw_n not in region)
      if not same_edge:
        edges += 1

    if west_n not in region:
      # left is an edge
      same_edge = (north_n in region) and (nw_n not in region)
      if not same_edge:
        edges += 1

    east_n = (r, c + 1)
    ne_n = (r - 1, c + 1)
    if east_n not in region:
      # right is an edge
      same_edge = (north_n in region) and (ne_n not in region)
      if not same_edge:
        edges += 1
  return edges


regions = []
visited = set()
for r in range(num_rows):
  for c in range(num_cols):
    rc = (r, c)
    if rc not in visited:
      region = get_region(rc)
      visited |= region
      regions.append(region)

# print(regions)

total_price = 0
for region in regions:
  print(region, next(iter(region)))
  plant = get_plant(next(iter(region)))
  area = len(region)
  edges = calc_edges(region)
  price = area * edges
  total_price += price
  # print(f'{plant} (area: {area}, edges: {edges}): {region}')

print(total_price)