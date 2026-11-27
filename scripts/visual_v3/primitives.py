"""Shared pixel primitives. Object artwork uses a 2x nearest-neighbor grid."""
from math import cos, pi, sin
from random import Random

from PIL import Image, ImageDraw


class Canvas:
    def __init__(self, size, seed):
        self.image = Image.new("RGBA", (size, size))
        self.draw = ImageDraw.Draw(self.image)
        self.rng = Random(seed)

    def poly(self, points, color):
        self.draw.polygon([(round(x), round(y)) for x, y in points], fill=color)

    def line(self, points, color, width=1):
        self.draw.line([(round(x), round(y)) for x, y in points], fill=color, width=width)

    def rect(self, box, color):
        self.draw.rectangle(box, fill=color)

    def oval(self, box, color):
        self.draw.ellipse(box, fill=color)

    def stamp(self, other, xy):
        self.image.alpha_composite(other.image, xy)


WOOD = ("#392a24", "#59402e", "#825539", "#a97448", "#cc995d", "#e2b979")
STONE = ("#323439", "#49474e", "#645f68", "#827d83", "#a49c9c", "#c5bab2")
SAND = ("#533729", "#744b33", "#996844", "#ba8858", "#d6a66d", "#ecc18a")
MOSS = ("#293e29", "#40572d", "#5b7536", "#7c9347", "#a2aa5c")
LEAF = ("#233a2b", "#365531", "#4e6e37", "#6c873f", "#94aa55")


def bough(c, points, width=3, palette=WOOD):
    c.line(points, palette[0], width + 2)
    c.line(points, palette[2], width)
    c.line([(x - 1, y) for x, y in points], palette[3])


def clump(c, x, y, rx, ry, palette=LEAF, light=0):
    outline = []
    for step in range(18):
        angle = step * pi / 9
        edge = c.rng.uniform(.8, 1.12)
        outline.append((x + rx*cos(angle)*edge, y + ry*sin(angle)*edge))
    c.poly(outline, palette[0])
    for scale, shift, shade in [(.85, .04, 1), (.71, -.12, 2 + light),
                                (.43, -.28, 3 + light)]:
        c.poly([(x + (px-x)*scale - rx*.10, y + (py-y)*scale + ry*shift)
                for px, py in outline], palette[shade])
    for _ in range(max(2, int(rx*ry/9))):
        u, v = c.rng.uniform(-.57, .51), c.rng.uniform(-.56, .47)
        px, py = x + u*rx, y + v*ry
        shade = c.rng.choice((2, 3, 4)) if u+v < -.25 else c.rng.choice((1, 2, 3))
        c.line([(px, py), (px+1, py-.5)], palette[shade])


def bush(c, berry=False, x=32, y=56, scale=1):
    bough(c, [(x, y), (x-2, y-12*scale), (x-6, y-22*scale)], 3)
    for dx, dy, rx, ry, light in [(15,13,11,8,0),(-14,13,11,8,0),
                                  (10,22,11,9,0),(-10,23,12,9,1),
                                  (1,30,12,9,1),(1,15,13,10,0),(-5,21,8,7,1)]:
        clump(c, x+dx*scale, y-dy*scale, rx*scale, ry*scale, LEAF, light)
    if berry:
        for dx, dy in [(-14,-20),(-7,-29),(6,-22),(12,-17),(2,-13),(-6,-14),(18,-12)]:
            px, py = round(x+dx*scale), round(y+dy*scale)
            c.poly([(px-2,py),(px,py-2),(px+2,py),(px+1,py+2),(px-1,py+2)], "#562836")
            c.rect((px,py-1,px+1,py), "#c45656")
            c.rect((px-1,py-1,px-1,py-1), "#ea8174")


def blades(c, x, y, high, count, reed=False):
    for _ in range(count):
        height = c.rng.uniform(.40, 1)*high
        offset = c.rng.uniform(-4, 4)
        spread = c.rng.uniform(-.43, .43)*height
        px = x + offset
        tip = px + spread
        c.poly([(px-1,y), (px+spread*.35-1,y-height*.62), (tip,y-height),
                (px+spread*.50+1,y-height*.61), (px+1,y)], c.rng.choice(LEAF[:4]))
        if reed and height > high*.67:
            c.line([(tip,y-height+3),(tip,y-height-2)], "#8b633c", 2)
            c.line([(tip-1,y-height+1),(tip-1,y-height-2)], "#594532")


FLOWERS = {
    "white": ("#8a9a88", "#e7e4cc", "#fff4d8"),
    "yellow": ("#a36a32", "#dda842", "#f4d477"),
    "blue": ("#4b567d", "#7991b6", "#a9bbd3"),
}


def blossom(c, x, y, palette, large=False):
    radius = 2 if large else 1
    for dx,dy in [(1,0),(0,1),(-1,0),(0,-1)]:
        px,py = x+dx*radius,y+dy*radius
        c.rect((px,py,px+int(large),py+int(large)), palette[0 if dy==1 else 1])
    c.rect((x,y,x,y), "#b47b3b")
    c.rect((x,y-radius,x,y-radius), palette[2])


def flowers(c, color):
    blades(c, 15, 28, 15, 10)
    for x,y in [(7,17),(12,11),(16,18),(19,9),(23,14),(25,20)]:
        c.line([(15,27),(x+1,y+5),(x,y)], LEAF[1])
        blossom(c,x,y,FLOWERS[color],True)


def fern(c):
    for tx,ty in [(4,16),(8,9),(15,4),(23,8),(27,16)]:
        c.line([(15,28),((15+tx)/2,ty+5),(tx,ty)], LEAF[3])
        for step in range(1,6):
            t = step/6
            x,y = 15+(tx-15)*t, 28+(ty-28)*t-6*t*(1-t)
            radius = 3.7*(1-t)+.7
            c.poly([(x,y),(x-radius,y-2),(x-radius+1,y-3),(x,y-1)], LEAF[3])
            c.poly([(x,y),(x+radius,y-3),(x+radius+1,y-2),(x+1,y+1)], LEAF[2])


def mushroom(c,x,y,r,palette):
    c.poly([(x-1,y),(x-2,y-2),(x-1,y-r),(x+2,y-r),(x+2,y-2),(x+1,y)], "#70614c")
    c.line([(x,y-2),(x,y-r)], "#dfc69b")
    cy = y-r-1
    c.poly([(x-r,cy),(x-r+1,cy-2),(x-2,cy-r+1),(x+2,cy-r+1),
            (x+r-1,cy-2),(x+r,cy),(x+2,cy+2),(x-2,cy+2)], palette[0])
    c.poly([(x-r+1,cy-1),(x-2,cy-r+2),(x+1,cy-r+1),(x+r-1,cy-1),
            (x+1,cy),(x-2,cy)], palette[1])
    c.line([(x-2,cy-2),(x-1,cy-2)], palette[2])


RED_CAP = ("#66372f","#b85b42","#efd2a5")
GOLD_CAP = ("#674d2d","#ae7944","#ebbc77")


def mushrooms(c, group):
    for x,y,r,palette in ([(10,24,4,GOLD_CAP),(21,21,5,RED_CAP),(16,28,6,RED_CAP),
                           (24,28,3,GOLD_CAP),(7,28,3,RED_CAP)] if group else
                          [(13,26,7,RED_CAP),(23,27,4,GOLD_CAP)]):
        mushroom(c,x,y,r,palette)


def log(c, x, y, length, radius, slant=None):
    rise = slant if slant is not None else length*.35
    tx,ty = x+length,y-rise
    c.poly([(x,y-radius),(tx,ty-radius),(tx+radius*.7,ty),(tx,ty+radius),
            (x,y+radius),(x-radius*.7,y)], WOOD[0])
    c.poly([(x+1,y-radius+1),(tx,ty-radius+1),(tx+radius*.4,ty-2),(x,y+1)],WOOD[3])
    c.poly([(x,y+1),(tx+radius*.4,ty-2),(tx,ty+radius-1),(x,y+radius-1)],WOOD[2])
    for _ in range(int(length/2)):
        t = c.rng.uniform(.1,.83)
        i = c.rng.uniform(-radius*.65,radius*.65)
        px,py=x+length*t,y-rise*t+i
        c.line([(px,py),(min(tx-1,px+c.rng.randint(2,6)),py-1)],c.rng.choice((WOOD[1],WOOD[4])))
    ex = round(radius*.73)
    r = round(radius)
    c.oval((round(x)-ex,round(y)-r,round(x)+ex,round(y)+r),WOOD[0])
    c.oval((round(x)-ex+1,round(y)-r+1,round(x)+ex-1,round(y)+r-1),WOOD[4])
    if r>=5:
        c.oval((round(x)-ex+2,round(y)-r+2,round(x)+ex-2,round(y)+r-2),WOOD[2])
        c.oval((round(x)-ex+3,round(y)-r+3,round(x)+ex-3,round(y)+r-3),WOOD[5])
    c.line([(x,y),(x+ex-1,y+r*.3)],WOOD[2])


def stump(c):
    for tx,ty in [(5,27),(10,27),(23,27),(26,25)]:
        bough(c,[(15,21),(tx,ty)],2)
    c.poly([(8,11),(22,11),(22,21),(24,25),(15,28),(7,25)],WOOD[0])
    c.poly([(10,12),(21,13),(20,24),(14,26),(9,24)],WOOD[2])
    c.line([(11,14),(11,22),(9,24)],WOOD[4])
    c.line([(18,16),(17,22),(18,24)],WOOD[1],2)
    for box,shade in [((8,7,22,16),0),((9,8,21,15),4),((11,9,19,14),2),((12,10,18,13),5)]:
        c.oval(box,WOOD[shade])
    c.line([(15,12),(20,13),(22,12)],WOOD[1])


def fallen(c):
    bough(c,[(36,37),(43,38),(51,44),(56,44)],3)
    bough(c,[(46,31),(45,25),(49,20)],4)
    log(c,15,47,39,8,19)
    bough(c,[(29,39),(28,32),(24,28)],3)
    for x,y in [(32,38),(38,35),(45,32),(51,30)]:
        clump(c,x,y-3,3,2,MOSS)


def log_bundle(c):
    for x,y,length in [(11,18,15),(8,24,17),(14,25,12),(11,19,14)]:
        log(c,x,y,length,4,6)
    for x,y in [(16,13),(21,11)]:
        c.line([(x,y),(x+3,y+2),(x+5,y+6),(x+4,y+9)],WOOD[0],2)
        c.line([(x,y),(x+3,y+2),(x+5,y+6)],WOOD[5])



