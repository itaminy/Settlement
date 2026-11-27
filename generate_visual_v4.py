"""Generate the Settlement World V4 pixel-art tiles and props with Pillow."""
from pathlib import Path
from random import Random
from PIL import Image, ImageDraw

ROOT = Path(__file__).resolve().parent / "resources/images/visual_v4"
NEAREST = Image.Resampling.NEAREST

GROUND = {
    "grass": (105, 140, 65), "flowering_grass": (116, 148, 75),
    "rocky_ground": (116, 109, 103), "sand": (185, 149, 88),
    "desert": (192, 153, 89), "snow": (204, 219, 222), "swamp": (89, 111, 75),
}
WATER = {"river": (46, 102, 138), "lake": (39, 87, 119), "shallow_water": (64, 125, 145)}
ROAD = {
    "dirt": ((158, 120, 78), 19), "forest": ((125, 105, 70), 12),
    "mountain": ((140, 126, 112), 13), "sand": ((169, 126, 72), 17),
    "stone": ((148, 139, 118), 19),
}


def shade(color, offset):
    return tuple(max(0, min(255, channel + offset)) for channel in color)


def save(image, folder, name):
    folder = ROOT / folder
    folder.mkdir(parents=True, exist_ok=True)
    image.resize((image.width * 2, image.height * 2), NEAREST).save(folder / f"{name}.png")


def ground(kind, base, index):
    rng = Random(f"{kind}/{index}")
    image = Image.new("RGBA", (32, 32), base + (255,))
    d = ImageDraw.Draw(image)
    for _ in range(107):
        x, y = rng.randrange(2, 29), rng.randrange(2, 29)
        color = shade(base, rng.choice([-13, -8, -4, 4, 8, 13]))
        d.line([(x, y), (x + rng.randrange(1, 3), y)], fill=color)
    for _ in range(7):
        x, y = rng.randrange(3, 27), rng.randrange(3, 28)
        if kind in ("desert", "sand", "snow"):
            d.line([(x, y), (x + 2, y - 1), (x + 4, y - 1)], shade(base, -10))
            d.line([(x, y + 1), (x + 2, y), (x + 4, y)], shade(base, 9))
        elif kind in ("rocky_ground", "swamp"):
            d.polygon([(x, y), (x + 2, y - 1), (x + 4, y), (x + 3, y + 2), (x, y + 1)],
                      shade(base, -14))
            d.line([(x, y), (x + 2, y - 1)], shade(base, 14))
        else:
            d.line([(x, y + 1), (x + 1, y - 1)], shade(base, -17))
            d.point((x + 1, y - 2), (216, 190, 102) if kind == "flowering_grass" else shade(base, 21))
    save(image, "terrain", f"{kind}_{index}")


def water(kind, base, index):
    rng = Random(f"{kind}/{index}")
    image = Image.new("RGBA", (32, 32), base + (255,))
    d = ImageDraw.Draw(image)
    for _ in range(31):
        x, y = rng.randrange(2, 25), rng.randrange(2, 29)
        w = rng.randrange(2, 7)
        d.line([(x, y), (x + w, y)], shade(base, rng.choice([-9, -6, 5, 10, 17, 20])))
        if w > 4:
            d.line([(x + 1, y - 1), (x + 3, y - 1)], shade(base, 14))
    if kind == "lake" and index == 4:
        for x, y in [(7, 8), (16, 20), (25, 13)]:
            d.polygon([(x-3,y),(x-1,y-2),(x+2,y-2),(x+4,y),(x+2,y+2),(x-2,y+2)], (55, 102, 64))
            d.line([(x-2,y),(x,y-1),(x+2,y-1)], (107, 147, 76))
            d.point((x+1,y-1), (224, 160, 165))
    save(image, "water", f"{kind}_{index}")


def road(kind, base, width, mask):
    image = Image.new("RGBA", (32, 32))
    d = ImageDraw.Draw(image)
    ends = [(16, -1), (32, 16), (16, 32), (-1, 16)]
    exits = [i for i in range(4) if mask & (1 << i)]
    turn = len(exits) == 2 and (exits[0] - exits[1]) % 2 == 1
    center = (16, 16)
    paths = [(ends[i], center, center) for i in exits] or [(center, center, center)]
    if turn:
        paths = [(ends[exits[0]], center, ends[exits[1]])]
    for inset, offset in [(0, -23), (2, 11), (4, 0)]:
        for a, b, c in paths:
            rgba = shade(base, offset) + (255,)
            for i in range(65):
                t = i / 64
                x, y = (round((1-t)**2*a[k] + 2*t*(1-t)*b[k] + t*t*c[k]) for k in range(2))
                edge = min(x, y, 31-x, 31-y)
                weighted_width = 13 + (width-13)*min(1, max(0, (edge-4)/12))
                r = (round(weighted_width)-inset)//2
                d.ellipse((x-r,y-r,x+r,y+r), rgba)
    rng = Random(f"{kind}/{mask}")
    for _ in range(115):
        x, y = rng.randrange(2, 30), rng.randrange(2, 30)
        if image.getpixel((x,y))[:3] != base:
            continue
        length = 1
        if kind == "stone" and x < 28 and image.getpixel((x+1,y))[:3] == base:
            length = 2
        d.line([(x,y),(x+length-1,y)], shade(base, rng.choice([-15,-8,6,11])) + (255,))
    save(image, "roads", f"{kind}_{mask}")


def prop(kind, variant):
    rng = Random(f"{kind}/{variant}")
    large = kind in ("raider-tent", "ruined-wall", "boardwalk", "islet")
    image = Image.new("RGBA", (64, 64) if large else (32, 32))
    d = ImageDraw.Draw(image)
    if kind == "raider-tent":
        x = 28 + variant * 2
        d.ellipse((8, 43, 57, 56), (21, 26, 21, 102))
        d.polygon([(8,46),(x,13),(53,25),(59,47),(40,54)], (48, 41, 46))
        d.polygon([(8,46),(x,13),(40,51)], (106, 88, 76))
        d.polygon([(x,13),(53,25),(59,47),(40,51)], (75, 65, 75))
        d.line([(10,45),(x,14),(53,26)], (164, 132, 103), 2)
        d.polygon([(24,48),(x-1,27),(36,50)], (34, 30, 34))
        for i in range(12):
            ix, iy = rng.randrange(39, 51), rng.randrange(27, 47)
            d.line([(ix,iy),(ix+2,iy+1)], (95, 81, 91))
    elif kind == "ruined-wall":
        d.ellipse((4,48,60,57), (38, 34, 30, 100))
        for row in range(4):
            for col in range(7):
                if row > 0 and (rng.random() < row*.15 or col == 6):
                    continue
                x, y = 7+col*7+row%2*2, 48-row*5
                base = shade((134,115,95), rng.randrange(-12,16))
                d.polygon([(x,y+3),(x,y-1),(x+4,y-3),(x+7,y-1),(x+7,y+4),(x+3,y+5)], base)
                d.line([(x,y-1),(x+4,y-3),(x+7,y-1)], shade(base, 24))
                d.line([(x+7,y),(x+7,y+3),(x+3,y+5)], shade(base, -32))
    elif kind == "boardwalk":
        d.polygon([(8,25),(46,19),(58,38),(20,51)], (26,24,21,104))
        for step in range(9):
            x, y = 9+4*step, 25-step
            base = shade((130,96,61), rng.randrange(-13,17))
            d.polygon([(x,y),(x+4,y-1),(x+15,y+17),(x+11,y+19)], base)
            d.line([(x,y),(x+11,y+18)], shade(base, 29))
            d.point((x+10,y+14), (43,40,35))
    elif kind == "ember-pit":
        d.ellipse((4,18,28,28), (22,22,20,104))
        d.ellipse((7,18,25,26), (64,58,55))
        for x,y in [(6,21),(10,25),(17,26),(23,24),(25,20),(20,17),(10,17)]:
            d.rectangle((x-1,y-1,x+1,y+1), shade((115,107,95), rng.randrange(-15,18)))
            d.point((x-1,y-1), (161,147,122))
        d.polygon([(14,23),(12,19),(16,15),(15,10),(20,15),(21,20),(18,23)], (218,91,41))
        d.polygon([(15,22),(16,16),(18,14),(19,21),(18,23)], (253,184,65))
    elif kind == "islet":
        outline = [(6,40),(11,30),(20,26),(28,29-variant),(43,24),(54,29),
                   (59,38),(56,45),(46,51),(35,49),(25,54),(13,50)]
        d.polygon([(x,y+2) for x,y in outline], (88,145,151,170))
        d.polygon(outline, (158,128,84))
        d.polygon([(11,38),(16,31),(26,30),(34,34),(43,28),(49,30),(53,37),
                   (47,43),(32,45),(23,49),(16,45)], (105,128,65))
        d.line([(10,39),(15,32),(22,28)], (193,162,102), 2)
        for i in range(22):
            x,y = rng.randrange(16,49), rng.randrange(32,45)
            if image.getpixel((x,y))[:3] == (105,128,65):
                d.point((x,y), shade((105,128,65), rng.randrange(-20,20)))
        d.polygon([(32,39),(35,33),(43,32),(47,37),(44,42),(37,43)], (98,101,87))
        d.line([(34,38),(36,34),(43,33)], (153,148,121))
    elif kind == "lantern":
        x = 13 + variant % 3
        d.ellipse((6,25,27,29), (27,26,21,99))
        d.rectangle((x-1,8,x+1,27), (91,67,46))
        d.line([(x,9),(x+8,9),(x+8,12)], (163,121,72), 2)
        d.polygon([(x+4,13),(x+8,11),(x+12,13),(x+11,23),(x+5,23)], (67,58,46))
        d.rectangle((x+6,15,x+10,21), (222,151,68))
        d.rectangle((x+7,16,x+9,20), (251,207,111))
        d.line([(x+5,23),(x+11,23)], (134,105,63))
    elif kind in ("bank-stones", "cairn"):
        d.ellipse((4,22,28,28), (28,30,27,101))
        for i in range(4+variant):
            x, y = rng.randrange(5,24), rng.randrange(18,24)
            if kind == "cairn":
                x, y = 14+i%2, 24-i*2
            d.polygon([(x-2,y),(x,y-3),(x+3,y-3),(x+5,y),(x+2,y+2),(x-2,y+1)], (99,99,94))
            d.line([(x-1,y-1),(x,y-3),(x+3,y-3)], (153,150,127))
    else:
        colors = {"rushes": ((56,89,49),(125,154,81)), "dune-grass": ((117,102,56),(180,155,92)),
                  "forest-fern": ((47,91,48),(99,145,71))}
        dark, light = colors[kind]
        d.ellipse((5,23,27,28), (25,34,22,89))
        for i in range(6+variant):
            root = rng.randrange(11,21)
            x, y = rng.randrange(6,27), rng.randrange(6,19)
            d.line([(root,26),(root+(x-root)//3,19),(x,y)], dark, 2)
            if kind == "forest-fern":
                for t in range(2,7,2):
                    lx = root + (x-root)*t//6
                    ly = 26 + (y-26)*t//6
                    d.line([(lx-3,ly-1),(lx,ly),(lx+2,ly-3)], light)
            else:
                d.point((x,y), light)
                if kind == "rushes" and i%2:
                    d.line([(x,y),(x,y+3)], (111,79,50), 2)
    assert all(edge >= 2 for edge in (image.getbbox()[0], image.getbbox()[1],
                                    image.width-image.getbbox()[2], image.height-image.getbbox()[3]))
    save(image, "props", f"{kind}_{variant}")


if __name__ == "__main__":
    for kind, base in GROUND.items():
        for i in range(7):
            ground(kind, base, i)
    for kind, base in WATER.items():
        for i in range(5):
            water(kind, base, i)
    for kind, (base, width) in ROAD.items():
        for mask in range(16):
            road(kind, base, width, mask)
    for kind in ("raider-tent", "ruined-wall", "ember-pit", "boardwalk", "cairn",
                 "bank-stones", "rushes", "dune-grass", "forest-fern", "lantern", "islet"):
        for index in range(4):
            prop(kind, index)
