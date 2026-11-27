"""Periodic terrain textures and 4-corner shoreline tiles."""
from math import cos, pi, sin
from random import Random

from PIL import Image

from .primitives import Canvas

TERRAIN = {
    "grass": ("#708d47","#668043","#78964e","#56743e","#8ea252"),
    "flowering_grass": ("#708d47","#668043","#78964e","#56743e","#8ea252"),
    "dirt": ("#9c744a","#916841","#a98054","#7a5638","#b89060"),
    "dirt_road": ("#a98357","#987149","#b28960","#876247","#c39c70"),
    "stone_road": ("#77746c","#68645e","#8e897d","#55544f","#a09b88"),
    "sand": ("#c6a373","#b99968","#d0af7f","#a7865a","#dbbb87"),
    "desert": ("#b78959","#a97a4e","#c39865","#956845","#d4aa71"),
    "rocky_ground": ("#797570","#6b6561","#89827b","#585555","#a19b92"),
    "snow": ("#cbd1ce","#bac6c9","#d6dbd3","#aabac2","#e5e6d8"),
    "ice": ("#8caeb6","#7fa0af","#9bbfc4","#668c9f","#b3ccd0"),
    "swamp": ("#626e45","#535f41","#6f7850","#414f3d","#828b58"),
    "clay": ("#a16e57","#94644e","#ac7960","#7f5347","#bd8869"),
}

WATER = ("#386376","#31596e","#447589","#2e5268","#55879a")


def periodic(canvas, points, color, line=False, width=1):
    """Paint the translated copies that intersect the 32px working tile."""
    for dx in (-32,0,32):
        for dy in (-32,0,32):
            vertices=[(x+dx,y+dy) for x,y in points]
            if line:
                canvas.line(vertices,color,width)
            else:
                canvas.poly(vertices,color)


def terrain_tile(name):
    c=Canvas(32,270 if name in ("grass","flowering_grass") else 211+list(TERRAIN).index(name))
    palette=TERRAIN[name]
    c.rect((0,0,31,31),palette[0])
    phase=c.rng.uniform(0,2*pi)
    # The continuous base shares its gradients across opposite edges.
    for y in range(32):
        for x in range(32):
            wave=(sin(x*pi/16+phase)+cos(y*pi/16-phase)
                  +sin((x+y)*pi/8+phase)*.4)
            if abs(wave)>1.3:
                shade=2 if wave>0 else 1
                c.rect((x,y,x,y),palette[shade])
    if name=="stone_road":
        for row in range(4):
            for col in range(4):
                x,y=col*8+(row%2)*4,row*8
                periodic(c,[(x,y+1),(x+2,y),(x+6,y),(x+8,y+2),(x+7,y+7),
                            (x+1,y+7),(x,y+5)],palette[3])
                periodic(c,[(x+1,y+2),(x+3,y+1),(x+6,y+1),(x+7,y+3),
                            (x+6,y+6),(x+2,y+6)],palette[c.rng.choice((0,1,2))])
                periodic(c,[(x+2,y+2),(x+5,y+2)],palette[4],True)
    elif name in ("sand","desert","snow"):
        for x,y in [(1,7),(11,18),(24,28),(24,10),(6,26)]:
            periodic(c,[(x-5,y+1),(x,y),(x+4,y-1),(x+7,y)],palette[1],True)
            periodic(c,[(x-4,y),(x,y-1),(x+4,y-2)],palette[4],True)
    elif name=="ice":
        for x,y in [(1,5),(18,15),(29,26)]:
            periodic(c,[(x-7,y+6),(x-3,y),(x,y),(x+4,y-8)],palette[3],True)
            periodic(c,[(x-6,y+6),(x-2,y+1),(x+1,y+1)],palette[4],True)
            periodic(c,[(x,y),(x+8,y+4)],palette[1],True)
    elif name=="swamp":
        for x,y in [(3,7),(23,19),(7,29)]:
            periodic(c,[(x-4,y),(x,y-2),(x+6,y-1),(x+8,y+1),(x+2,y+3),(x-4,y+2)],"#48584c")
            periodic(c,[(x-2,y),(x+2,y-1),(x+4,y-1)],"#697d64",True)
    elif name in ("grass","flowering_grass"):
        for x,y in [(0,2),(7,22),(12,9),(20,27),(27,14),(29,31)]:
            periodic(c,[(x-2,y-2),(x,y+1),(x+1,y-3),(x+2,y)],palette[3],True)
            periodic(c,[(x+1,y-2),(x+1,y-1)],palette[4],True)
        if name=="flowering_grass":
            for x,y in [(5,9),(24,7),(14,21),(22,26)]:
                periodic(c,[(x,y),(x,y-1),(x+1,y-1),(x+1,y)],"#dbc77d")
                periodic(c,[(x,y-1),(x,y-1)],"#f0e2a2",True)
    elif name=="rocky_ground":
        for x,y in [(4,4),(25,7),(16,14),(3,24),(23,26),(32,17)]:
            periodic(c,[(x-3,y+1),(x-2,y-2),(x+1,y-3),(x+3,y-1),(x+3,y+1),
                        (x,y+2)],palette[3])
            periodic(c,[(x-2,y),(x,y-2),(x+2,y-1),(x+1,y+1)],palette[2])
            periodic(c,[(x,y-2),(x+1,y-2)],palette[4],True)
    else:
        for x,y in [(2,8),(8,24),(15,15),(19,3),(27,20),(29,31)]:
            periodic(c,[(x-2,y),(x,y-1),(x+2,y-1),(x+3,y)],palette[1],True)
    for _ in range(51):
        x,y=c.rng.randrange(32),c.rng.randrange(32)
        periodic(c,[(x,y),(x+c.rng.randrange(2),y)],c.rng.choice(palette[1:]),True)
    return c.image


def water_tile(kind):
    c=Canvas(32,832)
    c.rect((0,0,31,31),WATER[0])
    for _ in range(25):
        x,y=c.rng.randrange(32),c.rng.randrange(32)
        length=c.rng.randint(2,10)
        periodic(c,[(x,y),(x+length//2,y-1),(x+length,y-1)],
                 c.rng.choice((WATER[1],WATER[2],WATER[2],WATER[3])),True)
    if kind=="shallow_water":
        for y in range(3,29):
            for x in range(3,29):
                fade=min(1,min(x-2,29-x,y-2,29-y)/7)
                wave=sin((y+.4*sin(x*pi/10))*pi/4)+cos((x+y)*pi/12)*.5
                if wave*fade>.58:
                    c.rect((x,y,x,y),("#4b7b88" if wave<1.06 else "#588893"))
        for x,y,l in [(10,7,7),(7,15,10),(16,22,10),(8,25,6)]:
            c.line([(x,y),(x+l//2,y),(x+l,y-1)],"#6f9998")
    if kind in ("river","lake","lily_water"):
        strokes = {
            "river": [(5,9,9),(14,15,12),(6,22,10),(17,26,8)],
            "lake": [(6,10,15),(12,12,13),(4,21,13),(11,25,14)],
            "lily_water": [(5,11,13),(14,24,12)],
        }
        for x,y,l in strokes[kind]:
            c.line([(x,y),(x+l//2,y-1),(x+l,y-1)],WATER[4])
            if kind=="river":
                c.line([(x+l-3,y-2),(x+l,y-2)],"#78a0a3")
    if kind=="lily_water":
        for x,y in [(9,10),(19,19),(11,24)]:
            c.poly([(x-4,y),(x-3,y-2),(x+1,y-3),(x+4,y-1),(x+1,y),
                    (x+4,y+1),(x+2,y+2),(x-2,y+2)],"#466243")
            c.line([(x-3,y),(x-1,y-1),(x+1,y-1)],"#8a9d59")
        c.poly([(16,18),(16,16),(18,17),(19,14),(20,17),(22,16),
                (21,19),(18,19)],"#d8a4a3")
        c.line([(18,17),(19,16)],"#efc1ad")
    return c.image


CORNERS = {
    "straight_n": (1,1,0,0), "straight_e": (0,1,1,0),
    "straight_s": (0,0,1,1), "straight_w": (1,0,0,1),
    "outer_nw": (1,0,0,0), "outer_ne": (0,1,0,0),
    "outer_se": (0,0,1,0), "outer_sw": (0,0,0,1),
    "inner_nw": (0,1,1,1), "inner_ne": (1,0,1,1),
    "inner_se": (1,1,0,1), "inner_sw": (1,1,1,0),
    "diagonal_nw_se": (1,0,1,0), "diagonal_ne_sw": (0,1,0,1),
}
SHORE = {
    "grass": ("#54673d","#927454","#b29565"),
    "sand": ("#a98860","#b99868","#d3b584"),
    "rocky_ground": ("#535355","#727274","#95938a"),
}


def shore_tile(ground, water, terrain, corners):
    nw,ne,se,sw=corners
    result=Image.new("RGBA",(32,32))
    for y in range(32):
        v=y/31
        for x in range(32):
            u=x/31
            north=nw+(ne-nw)*u
            south=sw+(se-sw)*u
            wobble=.083*sin((3*u+2*v)*pi)*sin(pi*u)*sin(pi*v)
            value=north+(south-north)*v+wobble
            if value>=.66:
                pixel=terrain.getpixel((x,y))
            elif value>=.48:
                index=0 if value>.60 else 1 if value>.535 else 2
                pixel=SHORE[ground][index]
            elif value>=.42:
                pixel="#94afa7" if value>=.454 else "#639096"
            elif value>=.29:
                pixel="#4b7b88" if value>=.35 else "#426f82"
            else:
                pixel=water.getpixel((x,y))
            if isinstance(pixel,str):
                pixel=tuple(int(pixel[i:i+2],16) for i in (1,3,5))+(255,)
            result.putpixel((x,y),pixel)
    return result


def waterfall(c):
    for path,color in [
        ([(8,26),(34,15),(48,20),(76,17),(87,29),(83,37),(64,43),(30,39),(11,38)],"#3a5359"),
        ([(15,30),(36,21),(47,25),(65,22),(82,25),(78,34),(66,37),(29,35)],"#608d98"),
        ([(24,34),(35,30),(69,28),(78,34),(71,56),(70,72),(77,79),(66,85),
          (28,88),(17,79),(23,65),(25,48)],"#326277"),
        ([(27,33),(43,32),(67,28),(76,33),(65,57),(64,74),(55,84),(28,84),
          (23,78),(30,63),(30,49)],"#578c9e"),
        ([(36,35),(50,34),(67,30),(70,34),(62,57),(61,74),(51,83),(35,82),
          (40,64),(41,46)],"#81abb0"),
        ([(27,77),(38,75),(52,79),(66,74),(82,80),(86,84),(73,90),(41,91),(13,87),(10,83)],"#4c7f8f"),
    ]:
        c.poly(path,color)
    for x,y,tx,ty in [(34,38,30,73),(41,41,37,70),(49,35,45,78),
                       (57,37,52,66),(66,40,60,71),(73,36,66,55)]:
        c.line([(x,y),((x+tx)/2,y+(ty-y)*.65),(tx,ty)],"#adc6c2",2)
    for x,y,w,h in [(18,82,7,2),(29,79,9,2),(45,84,13,2),(63,81,10,2),(42,88,11,1)]:
        c.line([(x,y),(x+w//2,y-h),(x+w,y)],"#a1bdba",2)
