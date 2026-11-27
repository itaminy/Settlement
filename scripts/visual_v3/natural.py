"""Rock formations, desert plants and ground props."""
from math import cos, pi, sin

from .primitives import LEAF, MOSS, SAND, STONE, WOOD, blossom, bough, clump


def rock(c, x, y, w, h, palette=STONE, moss=False, ore=None):
    def point(u,v):
        return x+w*u, y+h*v
    def face(vertices, shade):
        c.poly([point(*p) for p in vertices],palette[shade])
    skew=c.rng.uniform(-.09,.11)
    peak=(.39+skew,.04)
    face([(0,.76),(.12,.39),peak,(.64,.11),(.91,.36),(1,.72),
          (.87,.93),(.60,1),(.26,.97),(.08,.89)],0)
    face([(.04,.75),(.14,.40),peak,(.64,.13),(.87,.39),(.65,.55),
          (.35,.46),(.22,.71)],4)
    face([(.14,.42),peak,(.64,.13),(.55,.27),(.31,.33)],5)
    face([(.04,.76),(.22,.71),(.35,.46),(.65,.55),(.60,.92),
          (.26,.94),(.10,.86)],3)
    face([(.65,.55),(.89,.39),(.97,.72),(.85,.90),(.60,.94)],2)
    face([(.66,.57),(.84,.63),(.82,.89),(.61,.93)],1)
    face([(.12,.79),(.25,.78),(.34,.66),(.37,.73),(.31,.85),(.23,.91)],2)
    for _ in range(round(w*h/34)):
        u,v=c.rng.uniform(.19,.80),c.rng.uniform(.29,.83)
        px,py=point(u,v)
        c.line([(px,py),(px+c.rng.randint(1,max(2,round(w/16))),py-1)],
               palette[c.rng.choice((2,3,4))])
    if moss:
        for u,v,r in [(.33,.42,.11),(.49,.32,.17),(.59,.43,.12),(.26,.60,.08)]:
            px,py=point(u,v)
            clump(c,px,py,w*r,h*r*.7,MOSS)
    if ore:
        for u,v,r in [(.42,.48,.075),(.57,.60,.09),(.66,.40,.055),(.33,.68,.05),
                      (.75,.61,.035),(.49,.72,.045)]:
            px,py=point(u,v)
            size=max(2,w*r)
            c.poly([(px-size,py),(px-1,py-size),(px+size,py-size*.6),
                    (px+size,py+size*.45),(px,py+size)],ore[0])
            c.poly([(px-size+1,py),(px-1,py-size),(px+size-1,py-size*.5),
                    (px,py+1)],ore[1])
            c.line([(px-1,py-1),(px+size*.45,py-size*.4)],ore[2])


GOLD = ("#724624","#bc8134","#f2ce72")
IRON = ("#543c41","#9f6a5b","#d8a784")


def formation(c, mode):
    n=c.image.width
    palette=SAND if mode=="desert" else STONE
    points={
        "peak": [(5,81),(17,62),(26,35),(39,13),(49,7),(62,39),(71,30),
                 (81,54),(90,79),(83,88),(52,92),(28,90)],
        "mountain": [(5,106),(17,88),(25,71),(37,59),(49,34),(64,11),
                     (73,19),(87,43),(99,60),(106,79),(122,105),(112,115),(58,121),(19,116)],
        "cliff": [(5,72),(12,37),(27,24),(45,19),(66,22),(83,32),
                  (88,54),(90,74),(81,86),(54,92),(21,86)],
        "desert": [(4,77),(14,64),(19,43),(32,36),(36,46),(45,45),(49,24),
                   (58,17),(70,22),(76,39),(83,48),(91,74),(83,85),(47,90),(16,86)]
    }
    silhouette=points["mountain" if mode=="snow" else mode]
    c.poly(silhouette,palette[0])
    mid=(n*.54,n*.64)
    for index,(a,b) in enumerate(zip(silhouette,silhouette[1:]+silhouette[:1])):
        if index < len(silhouette)//2:
            shade=4 if (a[0]+b[0])/2 < n*.52 else 2
        else:
            shade=2 if a[0]>n*.58 else 3
        if index%4==0:
            shade=min(5,shade+1)
        c.poly([a,b,mid],palette[shade])
    if mode in ("mountain","snow","peak"):
        peak=silhouette[4 if mode=="peak" else 5]
        px,py=peak
        c.poly([peak,(px+9,py+26),(px+4,py+39),(px+19,py+68),
                (px+28,n*.86),mid,(px-3,py+42),(px-10,py+32)],palette[1])
        c.poly([peak,(px-10,py+20),(px-21,py+31),(px-26,py+55),
                (px-13,py+48),(px-15,py+60),(px-4,py+37),(px-8,py+31)],palette[5])
        if mode=="snow":
            c.poly([(37,59),(49,34),(64,11),(73,19),(87,43),(90,51),
                    (79,46),(77,55),(69,45),(65,51),(58,43),(52,50),
                    (51,43),(46,54)],"#a8bbc6")
            c.poly([(48,37),(64,11),(73,19),(75,28),(68,24),(66,35),
                    (60,37),(55,47),(55,38),(49,42)],"#e1e3da")
            c.poly([(65,35),(68,24),(78,32),(87,43),(81,40),(75,44),(72,38)],"#7f96aa")
    else:
        for i in range(6):
            y=45+i*6
            c.line([(16,y),(30,y-7),(52,y-9),(70,y-6),(82,y+1)],palette[1],2)
            c.line([(17,y-1),(30,y-8),(52,y-10),(68,y-7)],palette[4])
        for x in (28,51,71):
            c.poly([(x,48),(x+4,53),(x+1,71),(x+5,84),(x-1,88),
                    (x-3,74)],palette[2])
    # Strata and scree follow the illuminated and shaded faces.
    for _ in range(n*2):
        x,y=c.rng.randint(8,n-9),c.rng.randint(n//4,n-8)
        pixel=c.image.getpixel((x,y))
        if not pixel[3] or mode=="snow" and y<54:
            continue
        rgb=tuple(int(palette[0][i:i+2],16) for i in (1,3,5))
        if pixel[:3]==rgb:
            continue
        length=c.rng.randint(1,n//13)
        destination=(min(n-2,x+length),y-c.rng.randint(0,3))
        if c.image.getpixel(destination)[3]:
            c.line([(x,y),destination],palette[c.rng.choice((1,2,3,3,4))])
    for x,y,w,h in [(n*.13,n*.78,n*.21,n*.12),(n*.67,n*.79,n*.23,n*.13),
                    (n*.39,n*.84,n*.22,n*.105)]:
        rock(c,x,y,w,h,palette)


def cave(c):
    rock(c,5,15,54,43)
    rock(c,8,22,19,32)
    rock(c,44,24,16,30)
    mouth=[(15,54),(17,42),(22,30),(29,25),(37,25),(44,31),(49,44),(51,54),
           (41,57),(23,57)]
    c.poly(mouth,STONE[5])
    c.poly([(17,54),(20,43),(24,33),(30,28),(37,28),(43,34),(47,45),(49,54),
            (42,56),(23,56)],"#28272e")
    c.poly([(22,53),(25,37),(32,32),(38,33),(42,49),(44,55),(27,55)],"#171b22")
    c.poly([(17,54),(24,52),(30,53),(40,51),(49,54),(44,57),(24,58)],STONE[2])
    c.line([(18,54),(25,54),(30,55)],STONE[3])


def crystal(c):
    rock(c,7,39,50,18)
    for x,y,w,h,lean in [(22,47,9,27,-7),(41,46,9,31,10),(31,49,13,40,-1),
                          (17,49,6,14,-5),(45,51,7,18,6)]:
        a,b=x+lean,y-h
        c.poly([(x-w/2,y-2),(a-w/2,b+5),(a,b),(a+w/2,b+6),(x+w/2,y-2),(x,y+1)],
               "#334158")
        c.poly([(x-w/2+1,y-3),(a-w/2+1,b+5),(a,b+1),(x,y)],"#689eac")
        c.poly([(a,b+1),(a+w/2-1,b+6),(x+w/2-1,y-3),(x,y)],"#4a6087")
        c.poly([(a-w/2+1,b+5),(a,b+1),(a+w/2-1,b+6),(a,b+10)],"#b8d9d0")
        c.line([(a,b+11),(x-1,y-5)],"#8abec5")


CACTUS = ("#294337","#385c45","#507451","#749354","#a3aa61")


def cactus(c, bloom):
    for points,width in [([(30,43),(16,39),(13,33),(13,24)],7),
                         ([(33,38),(46,32),(48,21)],8),
                         ([(31,54),(29,17),(32,10)],12)]:
        c.line(points,CACTUS[0],width+2)
        x,y=points[-1]
        c.oval((x-width//2-1,y-width//2-1,x+width//2+1,y+width//2),CACTUS[0])
        c.line(points,CACTUS[2],width)
        c.oval((x-width//2,y-width//2,x+width//2,y+width//2),CACTUS[2])
        c.line([(u-2,v) for u,v in points],CACTUS[3],2)
        for u,v in points[:-1]:
            c.line([(u-2,v-3),(u+1,v-2)],CACTUS[4])
    c.line([(34,49),(33,19),(34,12)],CACTUS[1],2)
    for _ in range(28):
        x,y=c.rng.randint(8,53),c.rng.randint(8,53)
        if c.image.getpixel((x,y))[3]:
            c.rect((x,y,x,y),c.rng.choice((CACTUS[1],CACTUS[4])))
    if bloom:
        for x,y in [(12,20),(32,7),(48,18)]:
            blossom(c,x,y,("#8d4850","#d7796b","#f4af81"),True)


def dry_bush(c):
    for x,y in [(5,16),(8,10),(13,7),(17,5),(22,8),(26,12),(28,18)]:
        mid=((x+14)/2,(y+26)/2)
        c.line([(15,27),mid,(x,y)],SAND[1],2)
        c.line([mid,((x+mid[0])/2-3,(y+mid[1])/2-3)],SAND[3])
        c.line([(x,y),(x+1,y-1)],SAND[4])


def bones(c):
    for x,y,tx,ty in [(6,24,18,14),(17,25,25,20)]:
        c.line([(x,y),(tx,ty)],"#6b5949",4)
        c.line([(x,y-1),(tx,ty-1)],"#cdb997",2)
        for xx,yy in [(x,y),(tx,ty)]:
            c.oval((xx-2,yy-2,xx+2,yy+2),"#826d56")
            c.oval((xx-2,yy-2,xx+1,yy),"#e2cba0")
    for box,color in [((19,7,28,16),"#715d4e"),((19,7,27,14),"#d5bd97"),
                      ((21,10,22,11),"#605148"),((25,10,26,11),"#605148")]:
        c.oval(box,color)
    c.rect((22,14,25,16),"#b49a77")


def palm(c):
    bough(c,[(45,88),(43,76),(40,58),(43,40),(48,28)],7)
    for y,x in [(80,43),(74,42),(66,40),(59,40),(53,41),(47,42),(41,43),(34,46)]:
        c.line([(x-3,y-1),(x+3,y+1)],WOOD[1],2)
        c.line([(x-3,y-2),(x,y-1)],WOOD[4])
    for tx,ty,arc,shade in [(15,44,-19,1),(13,28,-14,2),(21,16,-14,2),
                             (46,9,-11,3),(75,13,-15,3),(82,27,-17,2),
                             (78,47,-20,1),(66,63,-13,2),(52,55,-8,2),(31,54,-13,3)]:
        ox,oy=47,27
        prev=(ox,oy)
        previous_depth=0
        for i in range(1,12):
            t=i/11
            x,y=ox+(tx-ox)*t,oy+(ty-oy)*t+arc*sin(pi*t)
            d=7*sin(pi*t)
            c.poly([prev,(x,y),(x,y+d),(prev[0]-1,prev[1]+previous_depth)],LEAF[shade-1])
            c.line([prev,(x,y)],LEAF[shade])
            if i%2==0:
                c.line([(x,y+1),(x+(tx-ox)*.065,y+d)],LEAF[min(4,shade+1)])
            prev=(x,y)
            previous_depth=d
    for x,y in [(45,27),(50,30),(47,34)]:
        c.oval((x-3,y-3,x+3,y+3),WOOD[1])
        c.oval((x-3,y-3,x+1,y+1),WOOD[3])


def small_rock(c):
    rock(c,5,13,22,14)


def stone_pile(c):
    for x,y,w,h in [(6,16,12,9),(15,13,11,10),(13,20,15,8),(5,23,9,5)]:
        rock(c,x,y,w,h)


def drift_rocks(c):
    for x,y,w,h in [(4,18,13,8),(15,10,12,10),(17,22,10,6),(5,24,8,4)]:
        rock(c,x,y,w,h,("#343b3c","#475457","#637071","#778881","#9aab95","#b8bfa5"),x==15)


