"""Timber structures and props, lit from the upper left."""
from math import cos, pi, sin

from .natural import rock
from .primitives import STONE, WOOD, bough, log

ROOFS = {
    "blue": ("#283643","#394e67","#4b6b8c","#668ba8","#8eacca"),
    "red": ("#4a2c2d","#6e3a39","#935045","#b66c53","#d58e70"),
    "neutral": ("#42352b","#634934","#815d3e","#a2784b","#c69960"),
}


def panel(c,x,y,w,h,depth=8,palette=WOOD):
    c.poly([(x-1,y-1),(x+depth,y-depth-1),(x+w+depth,y-depth-1),
            (x+w+depth,y+h-depth),(x+w,y+h),(x-1,y+h)],palette[0])
    c.poly([(x,y),(x+depth,y-depth),(x+w+depth-1,y-depth),(x+w,y)],palette[4])
    c.poly([(x+w,y),(x+w+depth-1,y-depth),(x+w+depth-1,y+h-depth-1),
            (x+w,y+h-1)],palette[2])
    c.rect((x,y,x+w-1,y+h-1),palette[3])
    for dy in range(4,h,5):
        c.line([(x,y+dy),(x+w-1,y+dy)],palette[1])
        c.line([(x+w,y+dy),(x+w+depth-1,y+dy-depth)],palette[1])
        start=x+c.rng.randrange(0,max(1,w//2))
        c.line([(start,y+dy-1),(min(start+8,x+w-1),y+dy-1)],palette[4])


def roof(c,x,y,w,h,depth,colors):
    # Roof ridge projects up and right. The front gable exposes the timber.
    ridge=(x+w//2,y-h)
    back=(ridge[0]+depth,ridge[1]-depth)
    c.poly([(x-1,y),(ridge[0],ridge[1]-1),back,(x+w+depth,y-depth),
            (x+w,y+2),(x+w//2,y-1)],colors[0])
    c.poly([ridge,back,(x+depth,y-depth),(x,y)],colors[3])
    c.poly([ridge,back,(x+w+depth-1,y-depth),(x+w-1,y+1)],colors[2])
    for t in [i/8 for i in range(1,8)]:
        for edge_x,shade in [(x,1),(x+w,1)]:
            px=round(ridge[0]+(edge_x-ridge[0])*t)
            py=round(ridge[1]+(y-ridge[1])*t)
            c.line([(px,py),(px+depth-1,py-depth+1)],colors[shade])
            c.line([(px,py+1),(px+depth-2,py-depth+3)],colors[4 if edge_x==x else 3])
            for offset in range(3+(int(t*8)%2)*2,depth-1,5):
                c.line([(px+offset,py-offset),(px+offset+1,py-offset+1)],colors[0])
    c.line([ridge,back],colors[4],2)


def aperture(c,x,y,w,h,door=False):
    c.rect((x-1,y-1,x+w,y+h),WOOD[1])
    c.rect((x,y,x+w-1,y+h-1),"#332d2c")
    if door:
        for dx in range(1,w,3):
            c.line([(x+dx,y+1),(x+dx,y+h-1)],WOOD[2])
        c.rect((x+w-3,y+h//2,x+w-2,y+h//2),"#d4ac66")
    else:
        c.rect((x+1,y+1,x+w-2,y+h-2),"#76929a")
        c.line([(x+1,y+h-2),(x+w-2,y+1)],"#aac3b8")
        c.line([(x+w//2,y),(x+w//2,y+h-1)],WOOD[1])
        c.line([(x,y+h//2),(x+w-1,y+h//2)],WOOD[1])
    c.line([(x-1,y+h),(x+w,y+h)],WOOD[4])


def house(c,team):
    colors=ROOFS[team]
    rock(c,12,70,64,17)
    panel(c,15,49,53,33,16)
    roof(c,11,52,61,26,18,colors)
    c.poly([(14,52),(41,27),(69,52),(69,55),(14,55)],WOOD[0])
    c.poly([(18,51),(41,31),(65,51),(65,53),(18,53)],"#c6a67a")
    c.line([(41,32),(41,53)],WOOD[2],3)
    c.line([(21,51),(34,41)],WOOD[2],2)
    c.line([(61,51),(48,41)],WOOD[2],2)
    for x in (16,40,65):
        c.line([(x,55),(x,80)],WOOD[1],3)
    aperture(c,29,62,12,18,True)
    aperture(c,49,61,11,10)
    aperture(c,41,39,5,6)
    c.line([(29,82),(41,82)],STONE[4],2)
    x,y=62,28
    panel(c,x,y,7,14,4,STONE)
    c.line([(x-1,y),(x+7,y),(x+11,y-4)],STONE[5],2)
    c.line([(x,y-1),(x+7,y-1),(x+9,y-3)],STONE[1])
    c.line([(52,55),(64,55)],colors[2],2)


def post(c,x,y,height=25):
    c.poly([(x-3,y),(x-3,y-height),(x,y-height-3),(x+3,y-height),
            (x+3,y),(x,y+2)],WOOD[0])
    c.poly([(x-2,y-1),(x-2,y-height),(x,y-height-2),(x,y)],WOOD[3])
    c.line([(x-1,y-height+1),(x-1,y-2)],WOOD[4])


def fence(c,gate):
    for y in (31,43):
        panel(c,8,y,47,6,3)
    if gate:
        c.poly([(14,49),(21,49),(50,30),(45,28)],WOOD[1])
        c.line([(19,46),(47,29)],WOOD[4],2)
        c.rect((47,39,51,41),STONE[4])
    for x in (10,54) if gate else (8,31,56):
        post(c,x,53,27 if gate else 24)
        c.rect((x-1,37,x,37),WOOD[0])


def tower(c):
    rock(c,18,74,59,14)
    for x,y in [(31,82),(64,77),(74,70)]:
        post(c,x,y,46)
    for a,b in [((31,78),(63,46)),((64,72),(34,46)),((64,60),(73,47))]:
        bough(c,[a,b],4)
    panel(c,24,38,47,15,12)
    for x in range(26,72,8):
        post(c,x,50,13)
    roof(c,21,36,54,21,12,ROOFS["neutral"])
    c.poly([(24,36),(48,17),(72,36)],WOOD[1])
    c.poly([(30,35),(48,22),(66,35)],WOOD[3])
    c.rect((45,28,51,34),WOOD[0])
    for y in range(50,83,5):
        c.line([(38,y),(47,y)],WOOD[4],2)
    for x in (37,48):
        c.line([(x,84),(x,47)],WOOD[1],2)


def sawmill(c):
    panel(c,12,45,59,33,14)
    roof(c,9,47,67,24,14,ROOFS["neutral"])
    c.poly([(12,47),(42,25),(73,47)],WOOD[1])
    c.poly([(19,47),(42,29),(64,47)],WOOD[3])
    aperture(c,33,51,24,26,True)
    for x,y in [(18,83),(31,85),(21,76)]:
        log(c,x,y,23,4,7)
    center=(69,65)
    wheel=[]
    for i in range(32):
        angle=i*pi/16
        r=11 if i%2 else 9
        wheel.append((center[0]+r*cos(angle),center[1]+r*sin(angle)))
    c.poly(wheel,STONE[2])
    c.oval((61,57,77,73),STONE[4])
    c.oval((63,59,75,71),STONE[1])
    for dx,dy in [(0,-6),(4,-4),(6,0),(4,4),(0,6),(-4,4),(-6,0),(-4,-4)]:
        c.line([center,(69+dx,65+dy)],STONE[3])
    c.oval((67,63,71,67),WOOD[3])
    c.rect((69,64,70,65),STONE[5])


def mine(c):
    rock(c,6,28,53,30)
    c.poly([(14,54),(17,40),(24,32),(38,32),(47,41),(51,54)],"#24252a")
    c.poly([(21,53),(23,39),(39,38),(45,53)],"#191c22")
    for x in (18,44):
        post(c,x,54,19)
    c.poly([(13,38),(22,31),(41,31),(49,37),(48,40),(15,40)],WOOD[0])
    c.poly([(15,37),(23,32),(41,32),(47,37)],WOOD[3])
    for y in range(48,60,3):
        c.line([(25,y),(43,y)],WOOD[2],2)
    c.line([(26,60),(29,47)],STONE[4],2)
    c.line([(42,59),(36,47)],STONE[3],2)


def tent(c):
    c.poly([(6,49),(24,22),(43,15),(58,41),(39,51),(24,54)],"#534637")
    c.poly([(24,22),(43,16),(57,41),(40,49)],"#ba9c67")
    c.poly([(7,49),(24,22),(39,50)],"#dcc38c")
    c.poly([(17,51),(24,30),(31,51)],"#584334")
    c.poly([(16,50),(24,29),(22,41),(19,51)],"#f1d9a0")
    c.line([(43,17),(55,39)],"#d9b878")
    c.line([(24,22),(42,16)],"#f1d9a0")
    c.line([(24,23),(8,54)],WOOD[2])
    c.line([(43,17),(58,46)],WOOD[2])
    c.line([(6,55),(9,53)],WOOD[1],2)


def bridge(c):
    def deck(i):
        return 8+5.7*i, 89-2.5*i-.066*i*(17-i)

    edge=[deck(i) for i in range(18)]
    front=[(x+15,y+13) for x,y in edge]
    c.poly(edge+[(x,y+5) for x,y in reversed(front)],WOOD[0])
    c.line([(x,y+3) for x,y in front],WOOD[2],2)
    for i,((x,y),(tx,ty)) in enumerate(zip(edge,edge[1:])):
        c.poly([(x,y),(tx,ty),(tx+14,ty+12),(x+15,y+12)],WOOD[4 if i%3 else 3])
        c.line([(x+4,y+4),(x+9,y+8)],WOOD[3 if i%3 else 2])
    for x,y in edge[1:-1]:
        c.line([(x,y+1),(x+14,y+12)],WOOD[2])
    for rail in (edge,front):
        anchors=[rail[i] for i in (0,4,9,13,17)]
        for x,y in anchors:
            post(c,round(x),round(y),11)
        c.line([(x,y-9) for x,y in anchors],WOOD[0],3)
        c.line([(x,y-10) for x,y in anchors],WOOD[4])


def flag(c,team):
    color=ROOFS[team]
    post(c,9,27,20)
    c.poly([(10,5),(15,4),(20,6),(27,6),(24,10),(27,15),
            (19,14),(14,12),(10,13)],color[0])
    c.poly([(11,6),(15,5),(20,7),(25,7),(23,10),(25,13),
            (20,12),(14,11),(11,12)],color[2])
    c.line([(12,6),(15,5),(20,7)],color[4])
    c.line([(14,10),(19,11),(23,11)],color[1])


def sign(c):
    post(c,15,27,17)
    c.poly([(5,8),(22,8),(27,12),(22,16),(5,16)],WOOD[0])
    c.poly([(6,9),(22,9),(25,12),(21,15),(6,15)],WOOD[3])
    c.line([(6,9),(21,9)],WOOD[5])
    c.line([(8,13),(20,13)],WOOD[2])
    c.rect((13,11,14,11),STONE[2])


def barrel(c):
    c.poly([(6,12),(9,8),(22,8),(25,12),(26,20),(23,26),(19,28),
            (11,28),(7,25),(5,20)],WOOD[0])
    c.poly([(7,12),(23,12),(25,19),(22,25),(19,27),(11,27),(8,25),(6,19)],WOOD[2])
    for x,y in [(10,25),(14,26),(19,26),(22,24)]:
        c.line([(x,11),(x-1,19),(x,y)],WOOD[4 if x<16 else 1])
    for y in (14,22):
        c.line([(6,y),(12,y+2),(20,y+2),(25,y)],STONE[1],2)
        c.line([(7,y),(12,y+1),(20,y+1)],STONE[4])
    c.oval((6,6,24,14),WOOD[0])
    c.oval((8,7,22,12),WOOD[4])
    c.line([(10,9),(20,9)],WOOD[2])
    c.line([(11,11),(20,11)],WOOD[2])


def crate(c):
    panel(c,5,13,17,14,6)
    c.rect((6,14,20,25),WOOD[1])
    c.rect((7,15,19,24),WOOD[3])
    c.line([(7,24),(19,15)],WOOD[1],3)
    c.line([(7,23),(18,15)],WOOD[4])
    c.line([(8,12),(23,12),(25,9)],WOOD[2])


def chest(c):
    panel(c,5,15,19,12,4)
    c.poly([(4,16),(6,11),(11,7),(24,7),(28,11),(27,16),(23,18),(5,18)],WOOD[0])
    c.poly([(6,15),(8,11),(12,8),(24,8),(26,11),(23,15)],WOOD[3])
    c.line([(7,15),(9,11),(14,8)],"#bd9656",2)
    c.line([(19,15),(21,11),(24,8)],"#bd9656",2)
    c.line([(6,18),(23,18),(27,15)],WOOD[1],2)
    c.rect((13,16,17,21),"#e1b464")
    c.rect((15,18,15,19),WOOD[0])


def campfire(c):
    for x,y,w,h in [(6,19,8,6),(17,19,8,6),(9,23,8,5),(17,23,7,5)]:
        rock(c,x,y,w,h)
    c.line([(9,24),(22,19)],WOOD[1],3)
    c.line([(10,19),(21,25)],WOOD[3],3)
    c.poly([(10,21),(9,15),(12,10),(13,16),(16,10),(15,5),(20,9),
            (20,14),(23,12),(24,18),(21,23),(15,24)],"#913d32")
    c.poly([(12,20),(12,16),(15,19),(17,14),(17,10),(20,14),(19,18),(22,16),
            (22,20),(19,22),(14,23)],"#e28236")
    c.poly([(14,21),(17,18),(18,15),(19,19),(21,20),(18,22)],"#f6c56b")


def well(c):
    c.oval((12,40,54,59),STONE[0])
    c.oval((13,36,53,55),STONE[2])
    c.oval((13,32,53,49),STONE[4])
    c.oval((18,35,48,46),"#263a40")
    c.oval((22,39,44,45),"#496670")
    for x in (18,44):
        post(c,x,49,31)
    c.line([(17,26),(46,26)],WOOD[1],3)
    c.line([(31,26),(31,41)],"#caba8e")
    c.oval((28,39,34,44),WOOD[3])
    roof(c,12,24,40,10,7,ROOFS["neutral"])
    c.poly([(15,24),(32,15),(49,24)],WOOD[1])
    c.line([(15,24),(32,15),(49,24)],WOOD[4])

