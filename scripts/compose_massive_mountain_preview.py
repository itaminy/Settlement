"""Assemble the Massive Mountain comparison and encode the captured Swing walk."""
from pathlib import Path
from sys import argv

from PIL import Image, ImageDraw

REDESIGN = argv[1:] == ["--v62"]
REFINEMENT = argv[1:] == ["--v63"]
POLISH = argv[1:] == ["--v631"]
ROOT = Path("docs/previews")
OUTPUT = ROOT / ("world_v6_3_1" if POLISH else "world_v6_3" if REFINEMENT else "world_v6_2" if REDESIGN else "world_v6_1")
SWING = [("foothills","FOOTHILLS"),("lower-slope","LOWER SLOPE"),
         ("gorge","GORGE") if REFINEMENT or POLISH else ("switchbacks","SWITCHBACKS"),
         ("camp-1","CAMP 1"),("camp-2","CAMP 2"),("camp-3","CAMP 3"),("camp-4","CAMP 4"),
         ("highlands","HIGHLANDS"),("summit","SUMMIT")]


def load(name):
    path = ((ROOT / f"world_v6_{name[2]}" / f"{name[4:]}.png") if name.startswith(("v61-", "v62-", "v63-"))
            else OUTPUT / f"{name}.png")
    with Image.open(path) as im:
        return im.convert("RGB")


def grid(views, columns, size, filename):
    width, height = size
    margin = 8
    rows = (len(views)+columns-1)//columns
    sheet = Image.new("RGB",(columns*(width+margin)+margin,rows*(height+30)+margin),
                      (38,43,39))
    draw = ImageDraw.Draw(sheet)
    for i,(name,label) in enumerate(views):
        img = load(name)
        img.thumbnail(size,Image.Resampling.NEAREST)
        x,y=margin+i%columns*(width+margin),margin+i//columns*(height+30)
        draw.text((x,y+3),label,fill=(232,218,178))
        sheet.paste(img,(x+(width-img.width)//2,y+22))
    sheet.save(OUTPUT / filename)


def encode_gif():
    folder = {OUTPUT.name: ".mountain-frames", "world_v6_2": ".mountain-v62-frames",
              "world_v6_3": ".mountain-v63-frames", "world_v6_3_1": ".mountain-v631-frames"}[OUTPUT.name]
    filenames = sorted(Path(folder).glob("frame-*.png"))
    if not filenames:
        raise RuntimeError("Run the TCP/Swing capture script first")
    reduced = []
    for path in filenames:
        with Image.open(path) as im:
            frame=im.convert("RGB")
            frame.thumbnail((616,440),Image.Resampling.NEAREST)
            reduced.append(frame)
    atlas=Image.new("RGB",(756,484))
    for idx in range(36):
        frame=reduced[idx*(len(reduced)-1)//35].resize((126,80),Image.Resampling.NEAREST)
        atlas.paste(frame,(idx%6*126,idx//6*80))
    palette=atlas.quantize(colors=256)
    frames=[frame.quantize(palette=palette,dither=Image.Dither.NONE) for frame in reduced]
    frames[0].save(OUTPUT/"ascent.gif",save_all=True,append_images=frames[1:],duration=80,loop=0,optimize=True)
    with Image.open(OUTPUT/"ascent.gif") as gif:
        print(f"{gif.width}x{gif.height}, {gif.n_frames} encoded frames from {len(frames)} Swing frames")


if __name__ == "__main__":
    if POLISH:
        grid([("v63-massif","WORLD V6.3"),("massif","WORLD V6.3.1")],2,(580,670),"compare-massif.png")
        grid(SWING+[(f"fullscreen-{name}",f"FULLSCREEN | {name.upper()}")
                    for name in ("foothills","slope","gorge","highlands","summit")],3,(400,286),"gallery.png")
        encode_gif()
        raise SystemExit(0)
    compare=[]
    for name in ["foothills","camp-1","highlands","summit"]:
        title=dict(SWING)[name]
        if REFINEMENT:
            compare.extend([(f"v62-{name}",f"WORLD V6.2 | {title}"),(name,f"WORLD V6.3 | {title}")])
        elif REDESIGN:
            compare.extend([(f"v61-{name}",f"WORLD V6.1 | {title}"),(name,f"WORLD V6.2 | {title}")])
        else:
            compare.extend([(f"v6-{name}",f"WORLD V6 | {title}"),(name,f"WORLD V6.1 | {title}")])
    grid(compare,2,(720,514),"compare-v62.png" if REFINEMENT else "compare-v61.png" if REDESIGN else "compare-v6.png")
    if REFINEMENT:
        grid([("v62-massif","WORLD V6.2"),("massif","WORLD V6.3")],2,(580,670),"compare-massif.png")
    elif REDESIGN:
        grid([("v61-massif","WORLD V6.1"),("massif","WORLD V6.2")],2,(580,670),"compare-massif.png")
    grid(SWING+[("fullscreen-foothills","FULLSCREEN | FOOTHILLS"),
                ("fullscreen-summit","FULLSCREEN | SUMMIT")],3,(400,286),"gallery.png")
    encode_gif()
