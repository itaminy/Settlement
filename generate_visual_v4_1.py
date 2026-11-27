"""World V4.1 environment sprites, drawn on a two-pixel grid."""
from pathlib import Path
from random import Random
from math import sin
from PIL import Image, ImageDraw

OUT = Path(__file__).resolve().parent / "resources/images/visual_v4_1"
OUT.mkdir(parents=True, exist_ok=True)


def rgb(base, light):
    return tuple(max(0, min(255, c + light)) for c in base)


def save(im, name):
    im.resize((im.width * 2, im.height * 2), Image.Resampling.NEAREST).save(OUT / f"{name}.png")


def rock(im, profile, floor, palette, rng, snow=False):
    d = ImageDraw.Draw(im)
    points = profile + [(profile[-1][0]+3, floor-5), (profile[-1][0]-3, floor),
                        (profile[0][0]+5, floor+2), (profile[0][0]-3, floor-4)]
    d.polygon(points, rgb(palette, -26))
    fill = Image.new("1", im.size)
    ImageDraw.Draw(fill).polygon(points, 1)
    paint = Image.new("RGBA", im.size)
    p = ImageDraw.Draw(paint)
    for i, ((x,y),(xx,yy)) in enumerate(zip(profile,profile[1:])):
        if xx <= x:
            continue
        kink = min(floor-3, max(y,yy) + rng.randrange(7,20))
        q = [(x,y),(xx,yy),(xx+7,kink),(xx+1,floor+5),(x+1,floor+5),(x+5,kink)]
        p.polygon(q,rgb(palette,rng.randrange(-35,13)))
        p.polygon([(x,y),(xx,yy), (xx+7,kink),(x+5,kink)],rgb(palette,rng.randrange(14,32)))
        p.polygon([(x+5,kink),(xx+7,kink),(xx+3,kink+6),(x-6,kink+12)],
                  rgb(palette,rng.randrange(-34,-13)))
        p.line([(x,y),(xx,yy),(xx+5,yy+5)],rgb(palette,35))
        if snow and y < floor-25 and yy < floor-25:
            p.polygon([(x,y),(xx,yy),(xx+4,yy+7),(xx-2,yy+12),(x+4,y+8)], (199,213,215))
    for _ in range(420):
        x,y = rng.randrange(4,im.width-5), rng.randrange(12,im.height-6)
        if paint.getpixel((x,y))[3]:
            c = paint.getpixel((x,y))[:3]
            p.line([(x,y),(x+rng.randrange(1,5),y)],rgb(c,rng.choice([-24,-14,10,19])))
    im.paste(paint,(0,0),fill)


def ridge(name, i):
    rng = Random(f"{name}/{i}")
    im = Image.new("RGBA",(128,110))
    d = ImageDraw.Draw(im)
    palette = (119,109,102) if name != "sand-ridge" else (168,120,73)
    d.ellipse((5,83,123,105),(27,24,23,110))
    xs = [8,14,23,34,46,54,62,76,85,95,105,116]
    tops = [72,56,49,42,44,29,31,52,45,50,62,81]
    if name == "escarpment":
        tops = [72,58,58,50,51,44,44,50,52,50,60,73]
    profile = [(x,y+rng.randrange(-4,5)) for x,y in zip(xs,tops)]
    rock(im, [(x,y-12) for x,y in profile[1:-1]],86,rgb(palette,-8),rng,name=="snow-ridge")
    rock(im,profile,98,palette,rng,name=="snow-ridge")
    for _ in range(16):
        x,y = rng.randrange(9,117),rng.randrange(95,103)
        p = rng.randrange(1,4)
        d.polygon([(x,y),(x+p,y-2),(x+p*2,y),(x+p,y+2)],rgb(palette,rng.randrange(-24,30)))
    save(im,f"{name}_{i}")


def saddle(index):
    rng = Random(4020+index)
    im = Image.new("RGBA", (128, 72))
    profile = [(5,36),(13,25),(27,26),(39,30),(57,43),(72,39),
               (89,30),(108,31),(119,39)]
    rock(im, profile, 58, (112,104,96), rng)
    d = ImageDraw.Draw(im)
    for i in range(20):
        x,y = rng.randrange(9,117),rng.randrange(47,64)
        if not im.getpixel((x,y))[3]:
            d.polygon([(x,y),(x+3,y-2),(x+6,y),(x+2,y+1)], (113,105,95,162))
    save(im, f"saddle_{index}")


def waterfall():
    base = Image.new("RGBA",(160,94))
    rng = Random(1281)
    # The banks form one horizontal sill with a central gap for the spill.
    rock(base,[(4,32),(12,23),(24,24),(36,15),(49,20),(58,24),(66,29)],69,(99,104,100),rng)
    rock(base,[(94,29),(103,22),(113,23),(122,14),(135,17),(147,26),(155,28)],70,(101,105,100),rng)
    d = ImageDraw.Draw(base)
    d.polygon([(61,28),(69,26),(90,26),(99,29),(95,34),(66,34)],(72,111,119))
    d.polygon([(61,31),(67,30),(95,29),(99,33),(97,60),(93,68),(65,68),(61,61)],(63,126,146))
    d.polygon([(61,64),(76,62),(100,65),(112,75),(99,84),(68,85),(52,79),(50,73)],(67,130,146,205))
    d.line([(61,30),(72,29),(91,29),(97,31)],(153,207,212),2)
    save(base,"fall_sill")
    for frame in range(6):
        im = Image.new("RGBA",base.size)
        p = ImageDraw.Draw(im)
        for i,x in enumerate(range(63,99,3)):
            top = 31+(i%3)
            p.line([(x,top),(x+(i%3)-1,60),(x-2,68)],
                   (158,210,213,195) if i%3 else (101,174,186,205),2)
            y=top+((frame*5+i*11)%27)
            p.line([(x,y),(x,y+6)],(219,242,233,230))
        for i in range(19):
            x=54+(i*17)%54
            y=68+(i*11+frame*2)%14
            p.line([(x,y),(x+2+i%5,y)],(188,224,219,175+i%50))
        save(im,f"fall_flow_{frame}")


def ground():
    colors={"duff":(92,115,66),"mud":(83,93,68),"dune":(195,154,90),
            "scree":(118,112,103),"packed":(142,112,72)}
    for kind,base in colors.items():
        for variant in range(4):
            rng=Random(f"{kind}/{variant}")
            im=Image.new("RGBA",(32,32),base+(255,))
            d=ImageDraw.Draw(im)
            for i in range(88):
                x,y=rng.randrange(2,28),rng.randrange(2,28)
                offset=rng.choice([-14,-8,6,11])
                d.line([(x,y),(x+1+i%3,y)],rgb(base,offset))
            for i in range(7):
                x,y=rng.randrange(3,20),rng.randrange(5,27)
                if kind=="dune":
                    d.line([(x,y),(x+3,y-2),(x+8,y-2),(x+11,y-4)],rgb(base,16))
                    d.line([(x+1,y+1),(x+6,y),(x+11,y-2)],rgb(base,-14))
                elif kind=="duff":
                    d.line([(x,y),(x+2,y-1),(x+4,y)],(141,119,69) if i%3 else (55,82,48))
                    d.line([(x+2,y),(x+3,y+2)],(86,73,51))
                else:
                    d.polygon([(x,y),(x+2,y-2),(x+5,y-2),(x+7,y),(x+5,y+2)],rgb(base,-13))
                    d.line([(x,y),(x+2,y-2),(x+5,y-2)],rgb(base,12))
            save(im,f"{kind}_{variant}")


if __name__ == "__main__":
    for name in ("massif","escarpment","snow-ridge","sand-ridge"):
        for i in range(4):
            ridge(name,i)
    for i in range(4):
        saddle(i)
    waterfall()
    ground()
