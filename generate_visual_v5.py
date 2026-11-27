"""Draw World V5 scenery and a contact sheet from deterministic pixel geometry."""
from pathlib import Path
from random import Random
from math import sin, cos, pi
from PIL import Image, ImageDraw, ImageFilter

ROOT = Path("resources/images/visual_v5")
PREVIEW = Path("docs/previews/world_v5")
ROCK = [(47, 48, 52), (73, 72, 75), (104, 102, 101), (126, 123, 115), (157, 151, 134)]
SNOW = [(46, 58, 72), (70, 87, 104), (107, 128, 144), (161, 183, 193), (219, 229, 221)]
SAND = [(87, 56, 47), (129, 79, 54), (169, 109, 65), (196, 140, 81), (226, 174, 104)]
WOOD = [(54, 38, 29), (83, 53, 36), (120, 77, 45), (156, 109, 60)]
LEAF = [(36, 55, 34), (56, 77, 42), (81, 106, 48), (112, 130, 57)]
SPECS = {
    "massif": (192, 139, 4), "escarpment": (160, 108, 4),
    "snow-ridge": (192, 145, 4), "sand-ridge": (192, 113, 4),
    "dune-field": (96, 44, 6), "dry-channel": (112, 42, 5),
    "scrub-cluster": (46, 32, 6), "relict-well": (49, 43, 4),
    "ruin-court": (94, 62, 5), "abandoned-camp": (75, 49, 5),
    "saguaro": (42, 51, 6), "rooted-snags": (54, 59, 5),
    "drowned-log": (62, 30, 5), "marsh-bank": (58, 32, 6),
    "forest-floor": (54, 35, 6), "talus-fan": (67, 37, 6),
}


def rock(draw, rng, x, y, w, h, colors):
    points = [(x, y + h), (x + w//7, y+h//2), (x+w//3, y),
              (x+3*w//4, y+h//5), (x+w, y+3*h//4), (x+4*w//5, y+h)]
    draw.polygon(points, fill=colors[0])
    draw.polygon([points[i] for i in (0, 1, 2, 3)] + [(x+2*w//3, y+3*h//4)], fill=colors[2])
    draw.polygon([points[i] for i in (1, 2, 3)] + [(x+w//2, y+h//2)], fill=colors[3])
    if w > 9:
        draw.line((x+w//3, y+2, x+2*w//3, y+h//4), fill=colors[4])


def ridge(img, rng, name):
    w, h = img.size
    d = ImageDraw.Draw(img)
    colors = SAND if name == "sand-ridge" else SNOW if name == "snow-ridge" else ROCK
    d.ellipse((7, h-24, w-5, h-7), fill=(34, 29, 25, 66))
    phase = rng.uniform(-1.3, 1.3)
    for layer in range(3):
        left, right = 6 + 9*(2-layer), w-7-5*(2-layer)
        base = h-11-16*(2-layer)
        points = []
        for x in range(left, right+1, 3):
            t = (x-left)/(right-left)
            envelope = sin(pi*t)**.38
            rise = (h*((.51 if name == "escarpment" else .57)-layer*.088)) * envelope
            if name == "sand-ridge":
                rise = min(rise, h*(.44-.06*layer)) + envelope*6*sin(t*16)
            rise += envelope*(10*sin(t*19+layer+phase) + 12*sin(t*9+1.1+phase))
            points.append((x, max(5,base-int(rise)+rng.randint(-2, 2))))
        bottom = [(x, base+rng.randint(-3, 2)-int(9*abs((x-left)/(right-left)-.5))) for x, _ in points]
        d.polygon(points + bottom[::-1], fill=colors[0])
        columns = []
        for index in range(1, len(points)-1, rng.randint(5, 8)):
            x, top = points[index]
            end = min(len(points)-1, index+rng.randint(4, 8))
            xe, _ = points[end]
            foot = bottom[index][1]
            fold = rng.randint(3, 12)
            if foot-top < 9:
                continue
            d.polygon(points[index:end+1] + [(xe+2, foot-2), (x+fold, foot-4), (x-2, foot-2)],
                      fill=colors[1])
            d.polygon(points[index:end+1] + [(x+fold, (top+2*foot)//3), (x-2, foot-3), (x-3, top+11)],
                      fill=colors[2])
            d.polygon(points[index:end+1] + [(x+fold, top+max(4,(foot-top)//4)), (x-2, top+12)],
                      fill=colors[3])
            if name == "snow-ridge":
                d.polygon(points[index:end+1] + [(xe-5, top+12), (x+4, top+8)], fill=colors[4])
            columns.append((x, xe, top, foot))
        for x, xe, top, foot in columns:
            for band in range(top+8, foot-3, rng.randint(5, 9)):
                step = rng.randint(3, 9)
                d.line((x+1, band, x+step, band+2, min(xe, x+step+5), band+2),
                       fill=colors[rng.choice((0, 3))])
            for _ in range(12):
                u, v = rng.randrange(x, xe), rng.randrange(top+6, foot-1)
                if img.getpixel((u, v))[3] == 255:
                    d.line((u, v, u+1, v), fill=colors[rng.randint(1, 3)])
    for _ in range(32):
        x = rng.randrange(12, w-16)
        y = rng.randrange(h-22, h-10)
        rock(d, rng, x, y, rng.randint(3, 10), rng.randint(2, 5), colors)


def branches(d, rng, x, y, n, reach, palette):
    for j in range(n):
        side = -1 if j % 2 else 1
        tip = (x+side*rng.randint(3, reach), y-rng.randint(4, reach))
        elbow = ((tip[0]+x)//2, (tip[1]+y)//2)
        d.line([(x,y), elbow, tip], fill=palette[0], width=3)
        d.line([(x-1,y-1), elbow, tip], fill=palette[2])
        d.line([elbow,(elbow[0]+side*2, tip[1]-2)], fill=palette[1])


def prop(img, rng, name, variant):
    w, h = img.size
    d = ImageDraw.Draw(img)
    x, y = w//2, h-7
    if name in {"dune-field", "dry-channel"}:
        for j in range(3):
            left, right = 7+j*7, w-8-j*4
            line = [(u, 13+7*j+int(5*sin(u/11+j*.8))+int(2*cos(u/6)))
                    for u in range(left,right)]
            for i in range(3 if name == "dry-channel" else 8, -1, -1):
                color = (152+i*4, 99+i*5, 61+i*3, 72-i*5) if name == "dry-channel" else (222-i*5, 165-i*5, 94-i*2, 88-i*7)
                d.line([(px, py+i) for px,py in line[2*i:len(line)-2*i]], fill=color)
        for _ in range(39):
            u, v = rng.randrange(12,w-12),rng.randrange(8,h-6)
            d.line((u,v,u+rng.randint(1,4),v), fill=(141,89,53,rng.randint(35,82)))
    elif name in {"ruin-court", "relict-well", "abandoned-camp"}:
        d.ellipse((7,h-25,w-6,h-5), fill=(45,33,25,80))
        if name == "relict-well":
            d.ellipse((9,18,w-7,y), fill=SAND[0])
            d.ellipse((9,13,w-7,y-7), fill=SAND[3])
            d.ellipse((14,17,w-12,y-11), fill=(42,34,33))
            for u in (14, w-13):
                d.rectangle((u,9,u+2,y-5), fill=WOOD[2])
            d.line((11,9,w-10,8), fill=WOOD[0], width=3)
            d.line((x,10,x,27), fill=WOOD[3])
        elif name == "ruin-court":
            for row in range(6):
                for col in range(6):
                    if col not in (0, 1) and row > 2 or rng.random() < .17:
                        continue
                    u, v = 9+12*col+4*(row%2), 41-5*row+(col//2)
                    rock(d,rng,u,v,12,8,SAND)
            for u,v in [(19,22),(22,11),(33,20),(61,23),(67,28)]:
                rock(d,rng,u,v,13,13,SAND)
        else:
            d.polygon([(9,38),(26,12),(54,17),(66,37),(44,42)], fill=(85,55,39))
            d.polygon([(12,35),(26,15),(49,18),(48,31),(32,26),(30,36)],fill=(158,126,77))
            d.line((25,10,25,39), fill=WOOD[0],width=3)
            d.line((52,13,54,38), fill=WOOD[2],width=2)
        for _ in range(20):
            rock(d,rng,rng.randrange(9,w-12),rng.randrange(h-13,h-8),rng.randint(2,6),3,SAND)
    elif name == "saguaro":
        d.ellipse((8,h-12,w-6,h-5), fill=(55,47,26,74))
        for side in (-1,1):
            u = x+side*rng.randint(8,13)
            top = rng.randrange(15,26)
            d.line([(x,y-9),(u,y-15),(u,top)], fill=LEAF[0], width=7)
            d.line([(x-1,y-10),(u-1,y-17),(u-1,top)], fill=LEAF[2], width=3)
        d.rounded_rectangle((x-5,6,x+5,y),4,fill=LEAF[0])
        d.rounded_rectangle((x-4,7,x+2,y-2),3,fill=LEAF[2])
        d.line((x-2,11,x-2,y-4),fill=LEAF[3])
        for v in range(14,y-2,5):
            d.point((x+1,v),fill=(193,171,101))
        if variant % 2:
            for u,v in [(x-2,8),(x,5),(x+3,8)]:
                d.rectangle((u,v,u+1,v+1), fill=(218,114,89))
    elif name == "rooted-snags":
        d.ellipse((6,h-15,w-6,h-5),fill=(34,42,30,94))
        branches(d,rng,x, y-3,7,17,WOOD)
        d.polygon([(x-7,y),(x-3,y-23),(x-6,14),(x+1,8),(x+6,y-23),(x+5,y)],fill=WOOD[0])
        d.line((x-3,y-2,x-1,y-21,x-3,15),fill=WOOD[2],width=2)
        for u,v in [(8,y),(12,y-3),(w-10,y-2),(w-14,y+1)]:
            d.line((x,y-10,u,v),fill=WOOD[1],width=3)
            d.line((x-1,y-11,u,v-1),fill=WOOD[3])
        branches(d,rng,x-1,24,4,14,WOOD)
    elif name == "drowned-log":
        for j in range(3):
            d.arc((6+j*4,8+j*3,w-6-j*3,h-5),10,170,fill=(117,150,106,110))
        d.line((13,12,w-12,h-11),fill=WOOD[0],width=10)
        d.line((13,10,w-12,h-13),fill=WOOD[2],width=4)
        branches(d,rng,x,19,3,10,WOOD)
        d.ellipse((w-17,h-15,w-8,h-6), fill=WOOD[3],outline=WOOD[0])
        d.arc((w-15,h-13,w-10,h-8),20,330,fill=WOOD[1])
    else:
        for _ in range(18):
            u,v = rng.randrange(8,w-11),rng.randrange(12,h-9)
            length = rng.randint(3,8)
            if name == "talus-fan":
                rock(d,rng,u,v,length,rng.randint(2,6),ROCK)
            elif name == "scrub-cluster":
                branches(d,rng,u,v+4,3,rng.randint(4,6),WOOD)
            elif name == "marsh-bank":
                d.ellipse((u-3,v-1,u+5,v+3),fill=LEAF[rng.randrange(3)])
                d.line((u,v,u+rng.randint(-3,3),v-length),fill=LEAF[3])
            else:
                d.line((u,v,u+length,v+1),fill=rng.choice((WOOD[1],WOOD[3],LEAF[2])))
                if length == 8:
                    d.rectangle((u+2,v-3,u+3,v-1),fill=(183,139,79))


def tile(name, variant):
    rng = Random(f"terrain:{name}:{variant}")
    palettes = {
        "dune": [(184,134,76),(190,141,81),(195,147,85),(179,130,74)],
        "shale": [(107,103,93),(111,108,98),(101,99,93),(115,111,98)],
        "bog": [(83,93,62),(92,101,66),(80,91,62),(89,95,64)],
        "litter": [(88,105,54),(91,109,56),(96,111,58),(84,102,54)],
        "lagoon": [(57,118,117),(62,126,119),(63,122,119),(55,116,115)],
        "depth": [(44,91,109),(48,97,114),(49,101,116),(47,95,111)],
        "wash": [(163,113,68),(174,125,75),(153,104,66),(185,134,80)],
    }
    colors = palettes[name]
    img = Image.new("RGB",(32,32),colors[0])
    d = ImageDraw.Draw(img)
    for _ in range(240):
        x,y = rng.randrange(32),rng.randrange(32)
        length = rng.randint(1,4)
        color = rng.choice(colors)
        for dx in range(length):
            xx,yy=(x+dx)%32,(y+(dx//3 if name in {"shale","bog"} else 0))%32
            if min(xx,yy,31-xx,31-yy) > 0:
                d.point((xx,yy),fill=color)
    return img.resize((64,64),Image.Resampling.NEAREST)


def atmosphere(name, variant):
    rng = Random(f"atmosphere:{name}:{variant}")
    if name == "cascade":
        img = Image.new("RGBA", (164, 78))
        d = ImageDraw.Draw(img)
        for row in range(3):
            for col in range(15):
                u = 6+10*col+rng.randrange(-2, 3)
                if row == 0 and 4 <= col <= 10:
                    continue
                v = 34+7*row+rng.randrange(-3, 4)
                rock(d, rng, u, v, min(160-u,rng.randint(9,16)), rng.randint(9,15), ROCK)
        for _ in range(18):
            u = rng.choice((rng.randint(7,36),rng.randint(126,148)))
            rock(d, rng, u, rng.randint(23,57), rng.randint(4,9), rng.randint(3,6), ROCK)
    else:
        img = Image.new("RGBA", (116, 55))
        alpha = Image.new("L", img.size)
        d = ImageDraw.Draw(alpha)
        for _ in range(23):
            x,y = rng.randint(14,75), rng.randint(13,37)
            d.ellipse((x,y,x+rng.randint(10,25),y+rng.randint(2,5)),fill=rng.randint(13,36))
        img.paste((188,207,173), (0,0,img.width,img.height))
        img.putalpha(alpha.filter(ImageFilter.GaussianBlur(4)))
        ImageDraw.Draw(img).rectangle((0,0,img.width-1,img.height-1),outline=(0,0,0,0),width=4)
    return img.resize((img.width*2,img.height*2),Image.Resampling.NEAREST)


def save(img, name, index):
    path = ROOT / f"{name}_{index}.png"
    img.save(path)
    with Image.open(path) as saved:
        saved.verify()
    if img.mode == "RGBA":
        left,top,right,bottom = img.getbbox()
        assert min(left,top,img.width-right,img.height-bottom) >= 4, (name,img.getbbox())
    else:
        assert img.mode == "RGB" and img.size == (64, 64), path
        for i in range(64):
            assert img.getpixel((i, 0)) == img.getpixel((i, 63)), path
            assert img.getpixel((0, i)) == img.getpixel((63, i)), path


def main():
    ROOT.mkdir(parents=True,exist_ok=True)
    PREVIEW.mkdir(parents=True,exist_ok=True)
    groups={}
    for name,(w,h,count) in SPECS.items():
        images=[]
        for index in range(count):
            rng=Random(f"{name}:{index}")
            img=Image.new("RGBA",(w,h))
            if name in ("massif","escarpment","snow-ridge","sand-ridge"):
                ridge(img,rng,name)
            else:
                prop(img,rng,name,index)
            ImageDraw.Draw(img).rectangle((0,0,w-1,h-1),outline=(0,0,0,0),width=2)
            img=img.resize((w*2,h*2),Image.Resampling.NEAREST)
            save(img,name,index)
            images.append(img)
        groups[name]=images
    for name in ("dune","shale","bog","litter","lagoon","depth","wash"):
        groups[name]=[tile(name,i) for i in range(6)]
        for index,img in enumerate(groups[name]):
            save(img,name,index)
    for name in ("cascade","marsh-haze"):
        groups[name]=[atmosphere(name,i) for i in range(4)]
        for index,img in enumerate(groups[name]):
            save(img,name,index)
    for start in range(0,len(groups),8):
        rows=list(groups.items())[start:start+8]
        sheet=Image.new("RGB",(1136,130*len(rows)),(51,56,50))
        d=ImageDraw.Draw(sheet)
        for row,(name,images) in enumerate(rows):
            d.text((8,row*130+56),name,fill=(238,225,191))
            for col,img in enumerate(images):
                img=img.copy()
                img.thumbnail((152,120),Image.Resampling.NEAREST)
                sheet.paste(img,(116+col*169+(152-img.width)//2,row*130+(130-img.height)//2),
                            img if img.mode=="RGBA" else None)
        sheet.save(PREVIEW/f"assets-{start//8+1}.png")
    print(f"{sum(map(len,groups.values()))} assets in {len(groups)} families")


if __name__ == "__main__":
    main()


