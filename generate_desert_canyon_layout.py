"""Shared canyon plan, independent of generated images."""
from pathlib import Path
import math

from generate_massive_mountain import edn, path_cells

ORIGIN = (68, 64)
SIZE = (32, 36)
MAIN = path_cells([(74,66),(79,66),(83,68),(88,68),(92,69),(96,72),
                   (96,77),(95,81),(96,85),(93,90),(90,93),(85,94),(82,92)])
BRANCHES = [path_cells(points) for points in [
    [(76,83),(77,79),(78,75),(77,71),(76,68),(76,66)],
    [(83,68),(83,71),(86,73),(87,73)],
    [(96,77),(93,76),(92,74),(91,74)],
    [(93,90),(89,88),(85,87),(82,88),(82,92)],
]]
BRIDGES = [(x,68) for x in range(86,89)] + [(95,y) for y in range(80,83) if (95,y) in MAIN]
CAMPS = [(81,69),(94,76),(89,94)]
PROPS = {(81,69):"campfire",(95,75):"campfire",(89,94):"campfire",
         (81,70):"crate",(95,76):"barrel",(88,94):"log-bundle",
         (75,65):"sign",(92,91):"sign",(79,68):"saguaro",(91,71):"dry-bush",
         (94,85):"bones",(83,91):"dry-bush",(92,93):"saguaro"}


def oasis(x,y):
    return ((x-90)/5.8)**2+((y-79)/4.5)**2<1 or ((x-75)/6.1)**2+((y-89)/5.0)**2<1


def geometry():
    route = set(MAIN).union(*BRANCHES)
    free = route | set(PROPS)
    free.update((x+dx,y+dy) for x,y in CAMPS for dx in (-1,0,1) for dy in (-1,0,1))
    # Braided escarpments separated by the main valley, branching gullies and oasis approaches.
    walls,pits=set(),set()
    for y in range(64,99):
        for x in range(73,99):
            p=x,y
            if p in free or oasis(x,y):
                continue
            def lobe(cx,cy,rx,ry,tilt):
                return ((x-cx-(y-cy)*tilt)/rx)**2+((y-cy)/ry)**2
            ridges=min(lobe(*shape) for shape in [
                (84,65,9,2.7,.82),(82,73,4.8,3.8,.48),(92,71,4.3,4.8,-.69),
                (98,77,3,9,.11),(85,84,5.5,3.4,.52),(90,89,6.3,3.1,.52),
                (97,90,2.8,5.5,-.28),(87,97,7.5,2.7,-.28)])
            if abs(x-(87+1.1*math.sin((y-66)*.53)))<1.08 and 64<=y<=72:
                pits.add(p)
            elif y in (80,81,82) and 94<=x<=98:
                pits.add(p)
            elif y <= 97 and ridges < .78+.15*math.sin(.91*x+.76*y):
                walls.add(p)
    return {"origin":ORIGIN,"size":SIZE,"trail":MAIN,"branches":BRANCHES,"bridges":BRIDGES,
            "camps":CAMPS,"platforms":sorted(free),"walls":sorted(walls),"pits":sorted(pits),
            "props":[[list(p),kind] for p,kind in sorted(PROPS.items())]}


if __name__=="__main__":
    Path("resources/desert_canyon.edn").write_text(edn(geometry())+"\n",encoding="utf-8")

