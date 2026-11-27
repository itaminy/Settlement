"""Bake the two opt-in World V6.5 forest studies."""
import csv
from pathlib import Path
from random import Random
import sys

import numpy as np
from PIL import Image, ImageDraw, ImageFilter

from generate_massive_mountain import edn
from generate_trees import BARK, SHADED_BARK, deciduous, needle_bough, stem, trunk
from relief_raster import smooth

ROOT = Path("resources/images/forest_v6_5")
POLISHED = ROOT.with_name("forest_v6_5_1")
SAMPLE = 32
CONTACT = (25,31,25)
LEAVES = [
    ("#1d2e24","#30442b","#445c32","#5f7639","#83934c"),
    ("#293226","#41472b","#585f32","#777c40","#969655"),
    ("#1a3026","#2c4933","#41613d","#577747","#82934e"),
]
NEEDLES = [
    ("#1b302a","#254339","#345347","#466e56","#67856b"),
    ("#203329","#2a4437","#395943","#537052","#718765"),
]


def tree(species, variant):
    rng = Random(650+variant+100*(species=="pine"))
    image = Image.new("RGBA",(128,128))
    draw = ImageDraw.Draw(image)
    if species == "leaf":
        crowns = [(41,59,27,20),(85,62,29,20),(65,73,27,19),
                  (37,37,21,18),(86,41,24,21),(61,27,28,21)]
        crowns = [(x+rng.randint(-7,7),y+rng.randint(-8,8),
                   r*rng.uniform(.83,1.12),s*rng.uniform(.79,1.15)) for x,y,r,s in crowns]
        crowns = [(x,max(y,round(s*1.15)+4),r,s) for x,y,r,s in crowns]
        deciduous(draw,rng,LEAVES[variant%len(LEAVES)],BARK,crowns)
    else:
        palette = NEEDLES[variant%len(NEEDLES)]
        trunk(draw,SHADED_BARK,rng,width=6)
        lean = rng.randint(-8,8)
        narrow = rng.uniform(.78,1.08)
        draw.polygon([(61,91),(50,56),(61+lean,12),(63+lean,6),
                      (75,50),(76,76),(69,95)],fill=palette[1])
        for y,left,right in [(86,42,29),(75,38,41),(62,34,32),(49,26,29),
                             (38,20,23),(27,16,15),(17,9,10)]:
            x = 63+round(lean*(92-y)/85)
            for span in [-left,right]:
                reach = span*narrow*rng.uniform(.75,1.18)
                ty = y+abs(reach)*rng.uniform(.12,.42)+rng.randint(-4,4)
                if abs(reach)>23:
                    stem(draw,[(x,y+7,2),(x+reach*.57,ty+1,1),(x+reach,ty,0)],
                         SHADED_BARK,rng)
                needle_bough(draw,rng,x,y,x+reach,ty,max(4,abs(reach)*.27),palette,3)
            needle_bough(draw,rng,x,y-1,x+rng.choice((-13,15)),y+13,5,palette,1)
    rgba = np.array(image)
    y,x = np.mgrid[:128,:128]
    shade = 1.10-.12*x/127-.12*y/127
    rgba[:,:,:3] = np.uint8(np.clip(rgba[:,:,:3]*shade[...,None],0,255))
    image = Image.fromarray(rgba)
    # Keep the root anchor fixed. Match the scene's two-pixel material scale.
    width = (176,204,156,192,160,216)[variant] if species=="leaf" else (140,154,126,166,138,150)[variant]
    height = (186,216,166,200,180,218)[variant] if species=="leaf" else (210,228,178,240,190,216)[variant]
    return image.resize((width,height),Image.Resampling.NEAREST)


def shadow(image):
    w,h = image.size
    # Contact and an oblique, southeast silhouette with a short penumbra.
    alpha = image.getchannel("A")
    length = round(h*.29)
    lean = round(h*.34)
    spread = w+lean+16
    extent = length+20
    tex = Image.new("L",(spread,extent))
    projected = alpha.transform((spread,length),Image.Transform.AFFINE,
                                (1,-lean/length,-8,0,-h/length,h-1),
                                resample=Image.Resampling.BILINEAR)
    tex.paste(projected.point(lambda p: p*91//255),(0,7))
    tex = tex.filter(ImageFilter.GaussianBlur(2.1))
    draw = ImageDraw.Draw(tex)
    a = w//2+8
    draw.ellipse((a-13,3,a+14,14),fill=104)
    tex = tex.filter(ImageFilter.GaussianBlur(1.2))
    result = Image.new("RGBA",tex.size,CONTACT)
    result.putalpha(tex)
    return result


def noise(rng, width, height, size):
    lattice = rng.integers(0,256,(size,size),dtype=np.uint8)
    return np.asarray(Image.fromarray(lattice).resize((width,height),Image.Resampling.BILINEAR),
                      dtype=np.float32)/255


def floor(species, rows, origin, size, polish=False):
    ow,oh = size
    width,height = ow*SAMPLE,oh*SAMPLE
    ox,oy = origin
    options = Random(651 if species=="leaf" else 652)
    rng = np.random.default_rng(653 if species=="leaf" else 654)
    coarse = noise(rng,width,height,16)
    cover = noise(rng,width,height,41)
    detail = noise(rng,width,height,196)
    tint = np.array([77,84,62] if species=="leaf" else [65,76,60])
    duff = np.array([111,95,67] if species=="leaf" else [100,86,61])
    moss = smooth((coarse+.27*cover-.40)/.35)
    rgb = duff+(tint-duff)*moss[...,None]
    light = np.round(((detail-.5)*29+(cover-.5)*10)/3)*3
    if polish:
        # Tight palette and small, hard-edged moss islands instead of cloudy duff.
        moss = np.floor(smooth((coarse*.55+cover*.45-.28)/.48)*5)/5
        tint = np.array([83,94,64] if species=="leaf" else [71,85,64])
        duff = np.array([103,91,64] if species=="leaf" else [92,82,61])
        rgb = duff+(tint-duff)*moss[...,None]
        light = np.round(((detail-.5)*17+(rng.random((height,width))-.5)*8)/3)*3
    rgb += light[...,None]
    image = Image.fromarray(np.uint8(np.clip(rgb,0,255))).convert("RGBA")
    draw = ImageDraw.Draw(image)
    zone = Image.new("L",image.size)
    mask = ImageDraw.Draw(zone)
    roads = Image.new("L",image.size)
    path = ImageDraw.Draw(roads)
    plants = {}
    if polish:
        for kind in ("fern","bush"):
            with Image.open(ROOT/f"{species}-{kind}.png") as sprite:
                plants[kind] = sprite.resize((13,11),Image.Resampling.NEAREST)
    for (x,y),(biome,road,obj,prop) in rows.items():
        u,v = (x-ox)*SAMPLE,(y-oy)*SAMPLE
        if road:
            path.rectangle((u,v,u+31,v+31),fill=255)
        if not biome or obj in {"water","bridge","terrain-blocked"}:
            continue
        mask.rectangle((u,v,u+31,v+31),fill=255)
        if road:
            continue
        # Low details use both clustered accents and empty ground.
        for _ in range(options.randint(6,20)):
            px,py = u+options.randrange(3,29),v+options.randrange(3,29)
            if species=="leaf":
                color = options.choice(("#91754a","#7b633f","#645438","#a18352","#546d3e"))
                dx,dy = options.choice(((2,1),(1,-2),(-2,1),(1,1),(-1,-1)))
                draw.line((px,py,px+dx,py+dy),fill=color)
                if options.random()<.27:
                    draw.point((px+dx,py+dy+1),fill="#9b8051")
            else:
                color = options.choice(("#847355","#675741","#5f6548","#78664a"))
                dx,dy = options.choice(((3,-1),(2,2),(-1,3),(2,0)))
                draw.line((px,py,px+dx,py+dy),fill=color)
        if obj=="tree":
            for end in ((6,23),(22,26),(25,20)):
                draw.line([(u+16,v+23),(u+end[0]-1,v+end[1]+2),(u+end[0],v+end[1])],
                          fill="#544736",width=2)
                draw.point((u+end[0]-1,v+end[1]+1),fill="#897456")
        elif prop in {"forest-floor","clear"} and options.random()<.26:
            px,py = u+options.randrange(6,25),v+options.randrange(7,25)
            for dx,dy in [(-4,-3),(-2,-5),(2,-4),(4,-2),(3,1)]:
                draw.line((px,py+3,px+dx,py+dy),fill="#526d40",width=2)
                draw.point((px+dx,py+dy),fill="#819253")
        if polish:
            for _ in range(options.randrange(2,6)):
                px,py = u+options.randrange(4,28),v+options.randrange(4,28)
                draw.polygon([(px-2,py),(px-1,py-2),(px+1,py-1),(px+2,py+1),
                              (px-1,py+2)],fill=options.choice(("#647449","#586940","#6c7c4a")))
                draw.point((px-1,py-1),fill="#7d8852")
            if obj=="clear" and prop in {"forest-floor","clear"}:
                if options.random()<.19:
                    px,py = u+options.randrange(5,18),v+options.randrange(7,18)
                    for dx,dy,kind in [(0,0,"fern"),(8,-3,"bush"),(5,6,"fern")]:
                        draw.ellipse((px+dx+1,py+dy+7,px+dx+11,py+dy+11),fill="#414a35")
                        image.alpha_composite(plants[kind],(px+dx,py+dy))
                if options.random()<.32:
                    px,py = u+options.randrange(4,29),v+options.randrange(5,28)
                    draw.polygon([(px-3,py),(px-1,py-2),(px+2,py-2),(px+4,py),
                                  (px+1,py+2),(px-2,py+2)],fill="#575849")
                    draw.line((px-2,py,px,py-1,px+2,py-1),fill="#92917a")
    rows_y,cols_x = np.mgrid[:height,:width]
    margin = np.minimum.reduce((cols_x,rows_y,width-1-cols_x,height-1-rows_y))
    coverage = np.asarray(zone.filter(ImageFilter.GaussianBlur(11)),dtype=np.float32)/255
    road = np.asarray(roads.filter(ImageFilter.GaussianBlur(7)),dtype=np.float32)/255
    alpha = smooth((coverage-.44)/.48)*smooth(margin/57)*(1-.97*smooth(road/.62))
    alpha *= (np.asarray(zone)>0)
    image.putalpha(Image.fromarray(np.uint8(alpha*255)))
    return image


def polished_trees():
    # Keep V6.5 originals. Derive irregular shoulders from three existing crowns.
    for variant,(width,height) in enumerate([(188,176),(154,214),(227,201)]):
        with Image.open(ROOT/f"leaf-{variant}.png") as src:
            original = np.asarray(src.resize((width,height),Image.Resampling.NEAREST))
        art = original.copy()
        for y in range(int(height*.72)):
            shift = round(np.sin(y*.047+variant*1.9)*min(5,y//3))
            art[y] = np.roll(original[y],shift,axis=0)
        art[:,:,:3] = np.uint8(np.clip(art[:,:,:3].astype(int)+[(3,5,-3),(-5,1,3),(8,4,-2)][variant],0,255))
        out = Image.fromarray(art)
        out.save(POLISHED/f"leaf-ragged-{variant}.png",optimize=True)
        shadow(out).save(POLISHED/f"leaf-ragged-{variant}-shadow.png",optimize=True)
    for name,source,size in [("leaf-young","birch",(84,119)),("pine-young","pine",(88,125)),
                             ("spruce-young","spruce",(76,139))]:
        with Image.open(Path("resources/images/nature_v3",source+".png")) as src:
            out = src.resize(size,Image.Resampling.NEAREST)
        out.save(POLISHED/f"{name}.png",optimize=True)
        shadow(out).save(POLISHED/f"{name}-shadow.png",optimize=True)


def bake(csv_path, polish=False):
    output = POLISHED if polish else ROOT
    output.mkdir(parents=True,exist_ok=True)
    if polish:
        polished_trees()
    sites = {"leaf": {}, "pine": {}}
    with open(csv_path,encoding="utf-8-sig",newline="") as file:
        for name,x,y,biome,road,obj,prop in csv.reader(file):
            sites[name][int(x),int(y)] = (biome=="1",road=="1",obj,prop)
    manifest = {}
    for name,rows in sites.items():
        for prop in (() if polish else ("fern","bush","berry-bush","stump","fallen-tree")):
            path = Path("resources/images/visual_v3/vegetation",prop.replace("-","_")+".png")
            with Image.open(path) as image:
                rgba = np.array(image.convert("RGBA"))
            y,x = np.mgrid[:rgba.shape[0],:rgba.shape[1]]
            shade = 1.12-.17*x/max(1,x.max())-.13*y/max(1,y.max())
            rgb = rgba[:,:,:3]*shade[...,None]+([-5,3,2] if name=="pine" else [2,1,-4])
            rgba[:,:,:3] = np.uint8(np.clip(rgb,0,255))
            Image.fromarray(rgba).save(ROOT/f"{name}-{prop}.png",optimize=True)
        for variant in range(0 if polish else 6):
            image = tree(name,variant)
            image.save(ROOT/f"{name}-{variant}.png",optimize=True)
            shadow(image).save(ROOT/f"{name}-{variant}-shadow.png",optimize=True)
        origin = min(x for x,y in rows),min(y for x,y in rows)
        size = (25,19)
        image = floor(name,rows,origin,size,polish)
        chunks = []
        for y in range(0,image.height,128):
            for x in range(0,image.width,128):
                piece = image.crop((x,y,min(x+128,image.width),min(y+128,image.height)))
                if piece.getchannel("A").getbbox():
                    asset = f"{name}-floor-{x}-{y}"
                    piece.save(output/f"{asset}.png",optimize=True)
                    chunks.append({"asset":asset,"x":(origin[0]*SAMPLE+x)*2,
                                   "y":(origin[1]*SAMPLE+y)*2,
                                   "width":piece.width*2,"height":piece.height*2})
        manifest[name] = {"chunks":chunks,"cells":sorted(p for p,row in rows.items() if row[0])}
    if not polish:
        Path("resources/forest_studies.edn").write_text(edn(manifest)+"\n",encoding="utf-8")


def bake_world_materials():
    """Reuse the polished floor painter as seed-independent ground material."""
    output = ROOT.with_name("forest_v6_5_2")
    output.mkdir(parents=True,exist_ok=True)
    size = 16,16
    rows = {(x,y):(True,False,"clear","clear")
            for y in range(size[1]) for x in range(size[0])}
    for species in ("leaf","pine"):
        image = floor(species,rows,(0,0),size,polish=True).convert("RGB")
        image.save(output/f"{species}-ground.png",optimize=True)


if __name__ == "__main__":
    if sys.argv[1:] == ["--world"]:
        bake_world_materials()
    else:
        bake(sys.argv[1],polish="--polish" in sys.argv[2:])
