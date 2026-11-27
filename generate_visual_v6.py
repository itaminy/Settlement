"""Build original World V6 artwork at two world pixels per drawing pixel."""
from pathlib import Path
from random import Random
from math import sin, cos, pi, hypot
from PIL import Image, ImageChops, ImageDraw

OUT = Path("resources/images/visual_v6")
OUT.mkdir(parents=True, exist_ok=True)
ROCK = [(38,41,46),(57,60,66),(74,78,81),(102,103,101),(127,125,114),(153,149,132)]
SAND = [(87,53,39),(113,65,43),(145,82,49),(178,108,59),(200,137,75),(222,161,94)]
ICE = [(51,65,83),(70,91,109),(96,121,141),(131,154,167),(170,193,201),(221,230,226)]
GREEN = [(46,61,32),(64,74,37),(85,93,44),(106,112,54)]


def save(name, img):
    img.resize((2*img.width, 2*img.height), Image.Resampling.NEAREST).save(OUT/f"{name}.png")


def rock(d, r, x, y, w, h, p):
    d.polygon([(x,y+h),(x+w//6,y+h//3),(x+w//2,y),(x+w,y+2*h//3),
               (x+3*w//4,y+h)], fill=p[1])
    d.polygon([(x,y+h),(x+w//6,y+h//3),(x+w//2,y),(x+w//2,y+2*h//3)],
              fill=p[r.choice((3,4))])


def ridge(name, cells, height, mode, phase):
    w = cells*32
    img = Image.new("RGBA",(w,height))
    shade = Image.new("RGBA",img.size)
    sh = ImageDraw.Draw(shade)
    p = {"rock":ROCK,"sand":SAND,"ice":ICE}[mode]
    r = Random(name)
    d = ImageDraw.Draw(img)
    base_y = height-17
    sh.ellipse((5,base_y-15,w-3,height-4), fill=(37,31,27,69))
    img.alpha_composite(shade)
    for layer in range(4):
        left, right = 5+(3-layer)*10, w-6-(3-layer)*12
        base = base_y-(3-layer)*20
        ceiling = [(x, max(7, base-int(
            sin(pi*(x-left)/(right-left))**.42
            * ((height*.43-layer*8) + 14*sin(x/26+phase) + 11*cos(x/57+layer))
            )+r.randrange(-3,4))) for x in range(left,right+1,3)]
        lip = [(x, base+int(4*sin(x/18+phase))+r.randrange(-1,2)) for x,_ in ceiling]
        bounds = Image.new("L",img.size)
        ImageDraw.Draw(bounds).polygon(ceiling+lip[::-1],fill=255)
        face = Image.new("RGBA",img.size)
        f = ImageDraw.Draw(face)
        f.polygon(ceiling+lip[::-1],fill=p[1])
        i=0
        while i<len(ceiling)-1:
            end=min(len(ceiling)-1,i+r.randint(4,22))
            x,top=ceiling[i]
            xe,ye=ceiling[end]
            foot=lip[i][1]
            spread=r.randint(4,13)
            middle=(2*foot+top)//3
            bend=x+int((xe-x)*r.uniform(.25,.69))
            ledge=middle+r.randrange(-11,9)
            f.polygon(ceiling[i:end+1]+[(xe+spread,foot),(x+spread,foot+2),(x-3,foot-3)],
                      fill=p[r.choice((1,2,2,3))])
            f.polygon(ceiling[i:end+1]+[(bend,ledge-7),(bend+8,ledge),
                       (x+spread,middle+5),(x-3,foot),(x+4,middle-3)],
                      fill=p[r.choice((3,3,4))])
            f.polygon(ceiling[i:end+1]+[(xe-6,ye+5),(bend,top+r.randrange(7,25)),(x-1,top+8)],
                      fill=p[5 if mode=="ice" and layer<3 else 4])
            if r.random()<.65:
                f.line([(x-2,middle),(bend,middle-4),(bend+8,middle-1),(xe,ledge)],
                       fill=p[1],width=2)
            if mode=="ice" and layer>=2 and r.random()<.7:
                f.polygon([(x+2,top+12),(bend,top+17),(bend+7,top+35),(x+8,top+23)],
                          fill=p[4])
            i=end
        for x in range(left,right,2):
            for _ in range(9):
                y=r.randrange(8,max(9,base+5))
                px=face.getpixel((x,y))
                if not px[3]:
                    continue
                v=r.choice((-10,-5,5,10))
                f.line((x,y,x+r.randint(1,6),y+int(mode=="sand" and x%6==0)),
                       fill=tuple(max(0,min(255,c+v)) for c in px[:3]))
        for y in range(26,base+3,8 if mode=="sand" else 14):
            for x in range(left+3,right-4,6):
                vy=y+int(5*sin(x/41+layer)+2*cos(x/19))
                if face.getpixel((x,vy))[3] and r.random()<.67:
                    f.line((x,vy,x+r.randint(4,11),vy+1),fill=p[r.choice((1,2,4))])
        face.putalpha(ImageChops.multiply(bounds,face.getchannel("A")))
        img.alpha_composite(face)
    for _ in range(cells*12):
        x=r.randrange(7,w-18)
        y=r.randrange(base_y-9,height-9)
        rock(d,r,x,y,r.randrange(3,14),r.randrange(2,7),p)
    if name in {"gorge-north", "glacier-front"}:
        cx=w*5//8
        arch=[(cx-22,base_y+4),(cx-19,base_y-17),(cx-13,base_y-27),
              (cx-3,base_y-36),(cx+9,base_y-36),(cx+20,base_y-26),
              (cx+25,base_y-13),(cx+23,base_y+4)]
        d.polygon(arch,fill=p[3])
        d.polygon([(cx-15,base_y+4),(cx-13,base_y-14),(cx-5,base_y-25),
                   (cx+4,base_y-29),(cx+14,base_y-19),(cx+17,base_y+4)],
                  fill=(27,40,49) if mode=="ice" else (27,28,31))
        d.line(arch[1:6],fill=p[5],width=2)
        for _ in range(26):
            x=r.randrange(cx-25,cx+29)
            y=r.randrange(base_y+3,base_y+12)
            rock(d,r,x,y,r.randrange(3,10),r.randrange(2,5),p)
    if mode=="rock":
        for _ in range(cells*9):
            x=r.randrange(10,w-12)
            y=r.randrange(base_y-1,base_y+11)
            if sin(x/16+phase)>0.6:
                d.line((x,y,x+2,y-r.randrange(2,6)),fill=r.choice(GREEN))
    save(name,img)


def dunes(name, seed):
    r=Random(seed)
    w=h=512
    im=Image.new("RGBA",(w,h))
    d=ImageDraw.Draw(im)
    for j in range(10):
        cx,cy=r.randrange(65,447),r.randrange(44,480)
        wide,deep=r.randrange(55,147),r.randrange(14,35)
        left,right=max(6,cx-wide),min(w-7,cx+wide)
        crest=[]
        end=[]
        for x in range(left,right):
            t=(x-cx)/wide
            envelope=max(0,1-t*t)
            y=cy+int(13*sin(t*2.7+j*.9)+5*sin(x/31))
            if 6<y<h-6:
                crest.append((x,y))
                end.append((x,min(h-5,y+int(deep*envelope))))
        if len(crest)<2:
            continue
        for part in range(8,0,-1):
            band=[(x, (y*(8-part)+bottom*part)//8) for (x,y),(_,bottom) in zip(crest,end)]
            d.polygon(crest+band[::-1],fill=(136+part*4,86+part*4,48+part*2,119-part*10))
        light=[(x,y-2) for x,y in crest]
        d.line(light,fill=(245,197,128,100),width=2)
        for x,y in crest[::5]:
            if r.random()<.61:
                d.line((x,y+4,x+r.randrange(2,6),y+3),fill=(238,186,111,80))
    save(name,im)


def forest_mat():
    r=Random(602)
    im=Image.new("RGBA",(256,256))
    d=ImageDraw.Draw(im)
    for _ in range(170):
        x,y=r.randrange(8,248),r.randrange(8,248)
        if hypot(x-128,y-128)>115 or sin(x/21)+cos(y/31)<-.3:
            continue
        d.ellipse((x-3,y-2,x+4,y+3), fill=(*r.choice(GREEN),r.randrange(38,90)))
        for k in range(3):
            d.line((x,y,x+r.randrange(-5,6),y-r.randrange(2,8)),fill=(*r.choice(GREEN),110))
        if r.randrange(10)==0:
            d.rectangle((x,y-2,x+1,y+1),fill=(173,143,82,169))
            d.line((x-2,y-3,x+2,y-3),fill=(142,79,53,185))
    save("forest-mat",im)


def ruins(name, marsh=False):
    im=Image.new("RGBA",(256,192))
    d=ImageDraw.Draw(im)
    r=Random(name)
    p=ROCK if marsh else SAND
    for y in range(106,177,9):
        for x in range(31,228,13):
            if r.random()<.7:
                rock(d,r,x+r.randrange(-2,3),y,12,6,p)
    for x,y in [(38,129),(65,124),(140,132),(181,144),(211,150)]:
        ht=r.randrange(23,76)
        for k in range(y,y-ht,-7):
            d.polygon([(x,k),(x+6,k-4),(x+14,k-4),(x+18,k),(x+16,k+5),(x,k+5)],fill=p[2])
            d.line((x+2,k+3,x+2,k,x+8,k-3,x+14,k-3),fill=p[4],width=2)
    if marsh:
        d.polygon([(74,123),(76,78),(112,64),(149,78),(153,124),(123,139)],
                  fill=(78,54,36))
        d.polygon([(66,84),(99,49),(142,56),(158,83),(118,100)],
                  fill=(97,101,52))
        for y in range(61,84,5):
            d.line((81,y+9,143,y),fill=(68,81,44),width=2)
        for x in range(81,146,6):
            d.line((x,97,x,128-abs(x-117)//5),fill=(112,82,49))
        d.rectangle((107,103,126,132),fill=(43,39,34))
    else:
        d.arc((91,96,139,133),0,359,fill=p[4],width=5)
        d.ellipse((98,102,133,125),fill=p[0])
    save(name,im)


def frost():
    r=Random(603)
    im=Image.new("RGBA",(256,192))
    d=ImageDraw.Draw(im)
    for _ in range(43):
        x,y=r.randrange(8,236),r.randrange(11,176)
        w,h=r.randrange(4,17),r.randrange(3,12)
        rock(d,r,x,y,w,h,ICE)
        d.line((x+1,y+h//2,x+w//2,y,x+w-2,y+h//2),fill=ICE[5])
    save("snow-drifts",im)


for name,width,height,material,phase in [
    ("west-cordillera",14,240,"rock",.41),("gorge-north",10,191,"rock",2.9),
    ("gorge-south",11,195,"rock",5.2),("east-rampart",8,174,"rock",3.8),
    ("glacier-crown",17,260,"ice",1.7),("glacier-front",12,222,"ice",3.3),
    ("canyon-west",13,188,"sand",.2),("canyon-east",12,202,"sand",2.2),
    ("canyon-south",9,163,"sand",5.1)]:
    ridge(name,width,height,material,phase)
dunes("dune-basin",601)
dunes("dune-north",615)
forest_mat()
ruins("desert-court")
ruins("marsh-hamlet",True)
frost()

def water_lattice(name, palette, frozen=False, pads=False):
    r=Random(name)
    im=Image.new("RGBA",(32,32),palette[0])
    d=ImageDraw.Draw(im)
    for _ in range(90):
        x,y=r.randrange(2,27),r.randrange(2,30)
        d.line((x,y,x+r.randrange(1,5),y),fill=r.choice(palette))
    if pads:
        for x,y in [(10,9),(18,17),(14,22),(23,19)]:
            d.ellipse((x-2,y-1,x+2,y+1),fill=(100,122,51))
            d.line((x-2,y-1,x+1,y-1),fill=(149,154,71))
            d.point((x+1,y),fill=(56,74,48))
        d.point((18,16),fill=(209,186,108))
    if frozen:
        for _ in range(r.randrange(1,3)):
            x,y = r.randrange(3,15),r.randrange(5,19)
            joints=[(x,y),(x+4,y-2),(x+6,y+1),(x+12,y+3)]
            d.line(joints,fill=palette[-1])
            d.line([joints[2],(x+8,y+7),(x+11,y+8)],fill=palette[2])
    save(name,im)


water_lattice("mire-water",[(53,74,57),(59,81,61),(65,85,64),(68,89,64)])
water_lattice("mire-shallows",[(81,93,61),(88,98,63),(88,96,68),(95,105,73)])
for number in range(6):
    name = "ice-water" if number == 0 else f"ice-water_{number}"
    water_lattice(name,[(115,150,166),(123,159,175),(130,164,182),(152,180,196)],True)


water_lattice("mire-pads",[(53,74,57),(59,81,61),(65,85,64),(68,89,64)],pads=True)

def frost_pine():
    source = Path("resources/images/nature_v3/spruce.png")
    with Image.open(source) as original:
        art = original.convert("RGBA")
    d = ImageDraw.Draw(art)
    rng = Random(604)
    mask = art.copy()
    for y in range(8,92,5):
        for x in range(13,115):
            pixel = mask.getpixel((x,y))
            if (pixel[3] == 255 and pixel[1]>pixel[0]*1.15 and
                    mask.getpixel((x,y-3))[3] < 160 and rng.random()>.16):
                d.line((x,y,x,y+rng.randrange(1,4)),fill=rng.choice(ICE[3:]))
    art.save(OUT/"frost-pine.png")

frost_pine()
