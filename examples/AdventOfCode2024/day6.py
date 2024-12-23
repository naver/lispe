import sys
import re
from collections import defaultdict, Counter, deque
import pyperclip as pc

def pr(s):
    print(s)
    pc.copy(s)
sys.setrecursionlimit(10**6)
infile = sys.argv[1] if len(sys.argv)>=2 else '6.in'
p1 = 0
p2 = 0
D = open(infile).read().strip()

G = D.split('\n')
R = len(G)
C = len(G[0])
for r in range(R):
    for c in range(C):
        if G[r][c] == '^':
            sr,sc = r,c

for o_r in range(R):
    for o_c in range(C):
        print(o_r, o_c)
        r,c = sr,sc
        d = 0 # 0=up, 1=right, 2=down, 3=left
        SEEN = set()
        SEEN_RC = set()
        while True:
            if (r,c,d) in SEEN:
                p2 += 1
                break
            SEEN.add((r,c,d))
            SEEN_RC.add((r,c))
            dr,dc = [(-1,0),(0,1),(1,0),(0,-1)][d]
            rr = r+dr
            cc = c+dc
            if not (0<=rr<R and 0<=cc<C):
                if G[o_r][o_c]=='#':
                    p1 = len(SEEN_RC)
                break
            if G[rr][cc]=='#' or rr==o_r and cc==o_c:
                d = (d+1)%4
            else:
                r = rr
                c = cc
pr(p1)
pr(p2)
