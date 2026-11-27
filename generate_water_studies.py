"""Bake deterministic V6.6 water studies from exported server water cells."""
import csv
from pathlib import Path
from random import Random
import sys

import numpy as np
from PIL import Image, ImageDraw
from scipy.ndimage import distance_transform_edt, gaussian_filter

from generate_massive_mountain import edn
from relief_raster import smooth

ROOT = Path("resources/images/water_v6_6")
GRID = 32  # two world pixels per art pixel


def shade_water(site, rows, origin, size, bridge, polish=False, natural=False, river_rows=None):
    ox,oy = origin
    width,height = (n*GRID for n in size)
    shape = (height,width)
    y,x = np.mgrid[:height,:width].astype(np.float32)
    wet = np.zeros(shape,np.float32)
    corners = wet.copy()
    for (u,v),(blocked,corners4,_,_,_) in rows.items():
        a,b = (u-ox)*GRID,(v-oy)*GRID
        patch = np.s_[b:b+GRID,a:a+GRID]
        wet[patch] = blocked
        nw,ne,se,sw = (1-int(c) for c in corners4)
        s = (x[patch]-a+.5)/GRID
        t = (y[patch]-b+.5)/GRID
        corners[patch] = (1-s)*((1-t)*nw+t*sw)+s*((1-t)*ne+t*se)
    field = .80*gaussian_filter(wet,8)+.20*gaussian_filter(corners,4)
    wx,wy = x+ox*GRID,y+oy*GRID
    wobble = .037*np.sin(.102*wx+.087*wy)+.023*np.sin(.193*wx-.142*wy)
    basin = field+wobble>.50
    # Keep every tile centre faithful, including isolated wet cells in tributaries.
    for (u,v),(blocked,*_) in rows.items():
        a,b = (u-ox)*GRID+16,(v-oy)*GRID+16
        if basin[b,a]!=blocked:
            patch = np.s_[b-9:b+10,a-9:a+10]
            brush = (x[patch]-a)**2+(y[patch]-b)**2<8**2
            basin[patch][brush] = blocked
    inside = distance_transform_edt(basin)
    outside = distance_transform_edt(~basin)
    depth = smooth(inside/(52 if site=="river" else 97))
    shallows,deep = {
        "river":((94,146,132),(36,84,104)),
        "lake":((100,148,137),(46,91,117)),
        "oasis":((125,159,133),(44,104,110)),
        "world":((100,148,137),(46,91,117)),
    }[site]
    rgb = np.array(shallows)+(np.array(deep)-shallows)*depth[...,None]
    phase = wy*.105+.3*np.sin(wx*.055)+.3*np.sin(wx*.013+wy*.17)
    wave = (phase%1)
    local = smooth((np.sin(wx*.18+wy*.063)+.39)/1.39)
    light = (np.floor((np.sin(phase*5.2)+1)*2)-2)*1.4
    light += ((wave<.095)*(3+7*local)-(wave>.90)*3)*smooth(inside/7)
    if polish:
        bend = 9*np.sin(wy*.011)+5*np.sin(wy*.03)
        reach = np.sin(wx*.087-bend*.09)+.62*np.sin(wy*.047+wx*.072)
        if site=="river":
            depth = np.clip(depth+reach*.10*smooth(inside/40),0,1)
            rgb = np.array(shallows)+(np.array(deep)-shallows)*depth[...,None]
            vein = (wx*.122-bend*.16+wy*.019)%1
            light = np.floor(reach*3)+((vein<.095)*6-(vein>.91)*3)*smooth(inside/12)
        else:
            # Quiet mottling instead of periodic horizontal wave bands.
            light = np.trunc((np.sin(wx*.13+wy*.067)+np.sin(wy*.12-wx*.055))*2.5)
            if site=="oasis":
                rgb += ((1-depth)*9)[...,None]*[1,.65,.22]
    if natural:
        # Broad broken depth pockets, with independent two-pixel grain and no
        # repeated river stripes or lake wave stripes.
        pocket = np.sin(wx*.043+wy*.077)+.67*np.cos(wx*.081-wy*.036)
        depth = np.clip(depth+pocket*.095*smooth(inside/29),0,1)
        rgb = np.array(shallows)+(np.array(deep)-shallows)*depth[...,None]
        rng_surface = np.random.default_rng(662+sum(map(ord,site)))
        grit = rng_surface.integers(-3,4,shape)
        light = np.trunc(pocket*2.4)+grit*.72
        if site=="oasis":
            rgb += ((1-depth)**2*17)[...,None]*[1,.72,.32]
            light += (grit*.9+np.trunc(3*np.sin(wx*.31-wy*.125)))*(1-depth)
    if river_rows:
        zones = np.zeros((3,*shape),dtype=np.float32)
        for (u,v),(_,_,biome,_,_) in rows.items():
            a,b = (u-ox)*GRID,(v-oy)*GRID
            zone = {"desert":0,"swamp":1,"snow":2}.get(biome)
            if zone is not None:
                zones[zone,b:b+GRID,a:a+GRID] = 1
        for channel,delta in enumerate(([13,13,-2],[-18,-21,-24],[2,4,11])):
            weight = gaussian_filter(zones[channel],20)
            rgb += weight[...,None]*delta
        current = np.maximum(0,1-abs(wx/GRID-np.array(
            [river_rows[v//GRID+oy] for v in range(height)])[:,None])/3.4)
        rgb -= current[...,None]*[7,3,1]
        light += np.trunc(current*4*np.sin(wx*.10+wy*.137))
    rgb += light[...,None]
    sand = np.array((162,132,84) if site=="oasis" else (117,117,87))
    bank = sand+(light*.9)[...,None]
    if natural:
        wet_soil = (1-smooth(outside/(4.4+1.8*np.sin(wx*.081+wy*.132))))
        bank -= wet_soil[...,None]*[29,24,16]
    rgb = np.where(basin[...,None],rgb,bank)
    alpha = np.uint8(np.where(basin,255,(164 if natural else (123 if polish else 165))*(1-smooth(outside/13))))
    image = Image.fromarray(np.dstack([np.uint8(np.clip(rgb,0,255)),alpha]))
    detail = Image.new("RGBA",image.size)
    draw = ImageDraw.Draw(detail)
    rng = Random(660+sum(map(ord,site)))
    reflections = []

    # Reflection fragments trail southeast from trees on banks, entirely within water.
    for (u,v),(_,_,biome,_,tree) in rows.items():
        if not tree:
            continue
        cx,cy = (u-ox)*GRID+16,(v-oy)*GRID+22
        if polish and (site=="oasis" or (river_rows and biome=="desert")):
            # Project reflected fronds toward the local pool, clipped to the water.
            center = min(((90.6,79.5),(75.3,89.4)),key=lambda p:(p[0]-u)**2+(p[1]-v)**2)
            vx,vy = (center[0]-ox)*GRID-cx,(center[1]-oy)*GRID-cy
            distance = max(1,np.hypot(vx,vy))
            for n in range(19,63,2):
                a,b = round(cx+vx*n/distance),round(cy+vy*n/distance)
                reach = 2+max(0,10-abs(n-46)//2)
                if reach<a<width-reach and 1<b<height-1 and basin[b,a]:
                    sway = round(2*np.sin(n*.31)) if natural else 0
                    for dx in range(-reach,reach+1):
                        u = a+dx+sway
                        if 0<=u<width and basin[b,u] and (dx in (-1,0,1) or (n+dx)%5<3):
                            draw.point((u,b),fill=(43,72,60,max(25,(111 if natural else 83)-n)))
                    if natural and n%6==1 and inside[b,a]>12:
                        reflections.append([2*(a+ox*GRID)-8,2*(b+oy*GRID),8,1,n,-min(12,reach+3)])
            continue
        for n in range(26 if polish else 18):
            a,b = cx+4+n//2+rng.randint(-6,6),cy+n*2
            if 8<a<width-16 and 1<b<height-2 and inside[b,a]>4:
                reach = rng.randint(3,12)
                if polish and site=="oasis" and n>9:
                    reach+=4
                if not polish or basin[b:b+2,a-reach//3:a+reach+1].all():
                    draw.line((a-reach//3,b,a+reach,b+1),
                              fill=(42,69,61,max(13,(88 if polish else 70)-n*3)),width=1+n%2)
    # Breaking the edge into clusters keeps gravel and reeds out of paths.
    candidates = np.argwhere((outside>2)&(outside<10))
    for i in rng.sample(range(len(candidates)),min(len(candidates),220 if polish else 360)):
        b,a = map(int,candidates[i])
        if not (9<a<width-10 and 13<b<height-10):
            continue
        if rows[(a//GRID+ox,b//GRID+oy)][3]:
            continue
        if polish and np.sin(a*.108+b*.072)+np.cos(a*.047-b*.133)<.18:
            continue
        tone = rng.choice(((140,133,100),(153,147,117),(126,123,99)))
        radius = rng.choice((1,1,2,2,3))
        draw.polygon([(a-radius,b),(a,b-radius),(a+radius+1,b),(a+1,b+2)],
                     fill=(74,82,67,164))
        draw.polygon([(a-radius,b),(a,b-radius),(a+radius,b-1),(a,b+1)],fill=(*tone,210))
        if rng.random()<.072:
            reed = (105,129,75) if site!="oasis" else (117,135,73)
            for shift in (-4,0,3,6):
                length = rng.randint(5,13)
                draw.line([(a,b+2),(a+shift//2,b-length//2),(a+shift,b-length)],fill=(*reed,230))
                draw.point((a+shift,b-length+1),fill=(164,146,89,240))
    if polish:
        # Pebbles below the surface and short irregular glints share the water mask.
        beds = np.argwhere((inside>6)&(inside<21))
        for i in rng.sample(range(len(beds)),min(len(beds),57 if site=="oasis" else 100)):
            b,a = map(int,beds[i])
            if not (5<a<width-6 and 4<b<height-5):
                continue
            r=rng.choice((1,2,2,3,4))
            draw.polygon([(a-r,b),(a-r//2,b-2),(a+r//2,b-2),(a+r,b+1),(a,b+3)],fill=(74,102,88,108))
            draw.line((a-r//2,b,a+r//2,b-1),fill=(158,163,127,103))
        points=np.argwhere(inside>12)
        for i in rng.sample(range(len(points)),min(len(points),450 if site=="river" else 200)):
            b,a = map(int,points[i])
            length=rng.randrange(2,10)
            cluster = np.sin(a*.045+b*.076)+np.sin(b*.129-a*.03)
            if natural and cluster<-.25:
                continue
            if a+length<width and b+6<height and basin[b:b+6,a:a+length+1].all():
                dy = rng.randrange(2,6) if natural and site=="river" else rng.randrange(2)
                draw.line([(a,b),(a+length//2,b+dy//2),(a+length,b+dy)],
                          fill=(151,200,187,rng.randrange(22,65)))
    if natural and site=="river":
        west,east,row = bridge
        oy_ = (row-oy)*GRID
        for u in range((west-ox)*GRID+22,(east-ox)*GRID+29,27):
            for n in range(2):
                a,b = u+2*n,oy_+27+n*5
                if 7<a<width-8 and basin[b:b+6,a-5:a+6].all():
                    draw.line([(a-5,b+2),(a-1,b+4),(a+3,b+3),(a+5,b)],
                              fill=(155,195,178,73-n*22))
    image.alpha_composite(detail)
    frames = []
    # Each animated dash stays in water over its entire short cycle.
    flow_samples = np.argwhere(inside>21)
    for i in rng.sample(range(len(flow_samples)),min(len(flow_samples),320)):
        b,a = map(int,flow_samples[i])
        dx,dy = (4,9) if site=="river" else ((4,1) if polish else (8,2))
        if polish and site=="river" and 6<b<height-7:
            channels = np.flatnonzero(basin[b-6])
            next_channel = np.flatnonzero(basin[b+6])
            if len(channels) and len(next_channel):
                dx = int(np.clip((np.median(next_channel)-np.median(channels))*.75,-8,8))
        if river_rows:
            stream = abs(a/GRID+ox-river_rows[b//GRID+oy])<2.8
            dy = 9 if stream else 1
            low,high = max(0,b//GRID+oy-1), min(max(river_rows),b//GRID+oy+1)
            dx = int(np.clip(8*(river_rows[high]-river_rows[low]),-8,8)) if stream else 3
        if natural and not river_rows and min(a,b,width-1-a,height-1-b)<39:
            continue
        if a+min(dx,0)<0 or a+max(dx,0)+10>=width or b+dy+3>=height or not basin[b:b+dy+3,a+min(dx,0):a+max(dx,0)+10].all():
            continue
        frames.append([2*(a+ox*GRID),2*(b+oy*GRID),2*dx,2*dy,rng.randrange(32),rng.randrange(7,16)])
    changed = [p for p,(blocked,mask,_,_,_) in rows.items() if blocked or mask!="1111"]
    if site=="river":
        west,east,row = bridge
        changed = sorted(set(changed)|{(x,row) for x in range(west,east+1)})
    if natural and not river_rows:
        edge = np.minimum.reduce((x,y,width-1-x,height-1-y))
        fade = smooth(edge/42)
        image.putalpha(Image.fromarray(np.uint8(np.asarray(image.getchannel("A"))*fade)))
    return image,changed,frames+reflections,basin


def bridge_image(west,east,row,output=ROOT,polish=False,suffix=""):
    width = (east-west+1)*GRID-8
    im = Image.new("RGBA",(width,37))
    d = ImageDraw.Draw(im)
    r = Random(663)
    d.polygon([(4,16),(width-4,15),(width-1,33),(13,36),(4,28)],fill=(36,39,32,108))
    for a in range(12,width-10,27):
        d.polygon([(a-3,14),(a+3,14),(a+4,31),(a-1,34)],fill=(68,48,37))
        d.line((a-2,18,a-1,31),fill=(121,87,55),width=2)
    d.polygon([(1,12),(width-2,12),(width-4,27),(4,28)],fill=(79,53,37))
    for x in range(1,width-4,5):
        off = r.choice((0,0,1))
        tone = r.randrange(-10,11)
        d.polygon([(x,11+off),(x+4,11),(x+3,24+off),(x,26)],
                  fill=(145+tone,104+tone,66+tone))
        d.line((x,12+off,x,22),fill=(183+tone,139+tone,91+tone))
        d.point((x+2,14),fill=(75,57,42))
        d.point((x+2,23),fill=(75,57,42))
        if polish:
            d.line((x+2,16,x+1,21),fill=(114+tone,80+tone,50+tone))
    rail = Image.new("RGBA",im.size) if polish else None
    for y in (8,27):
        if polish and y==27:
            d = ImageDraw.Draw(rail)
        for x in range(10,width-8,22):
            d.rectangle((x,y-6,x+2,y+2),fill=(94,63,43))
            d.line((x,y-6,x,y+1),fill=(174,127,77))
        d.line((9,y,width-9,y-1),fill=(99,70,46),width=2)
        d.line((9,y-1,width-9,y-2),fill=(168,125,80))
    if polish:
        d = ImageDraw.Draw(im)
        for x in (3,width-7):
            d.polygon([(x-2,13),(x+4,13),(x+4,26),(x-2,27)],fill=(133,101,65))
            d.line((x,14,x,24),fill=(174,135,88))
            d.point((x+2,17),fill=(85,67,51))
    im.save(output/f"river-bridge{suffix}.png",optimize=True)
    structure = {"asset":f"river-bridge{suffix}","x":west*64+8,"y":row*64-10,
                 "width":2*width,"height":74}
    if polish:
        rail.save(output/f"river-rail{suffix}.png",optimize=True)
        structure["rail"] = dict(structure,asset=f"river-rail{suffix}",foot_y=row*64+53)
        structure["rail"]["foot-y"] = structure["rail"].pop("foot_y")
    return structure


def waterfall():
    # The upper lip runs along the existing outlet of the mountain lake at [32,68].
    width,height = 67,66
    im = Image.new("RGBA",(width,height))
    d = ImageDraw.Draw(im)
    d.polygon([(8,15),(22,9),(44,12),(59,19),(58,44),(48,54),(10,52),(4,39)],fill=(37,64,63,140))
    for points,tone in [
        ([(0,23),(6,18),(13,10),(21,8),(28,11),(26,18),(17,19),
          (13,36),(7,44),(2,43)],(88,101,101)),
        ([(50,10),(59,13),(63,22),(57,32),(56,43),(48,48),(46,29)],(68,83,83)),
        ([(5,22),(10,15),(17,10),(24,12),(20,15),(11,17),(8,25)],(126,139,137)),
        ([(50,11),(58,14),(61,20),(55,18)],(112,124,120)),
        ([(12,14),(20,10),(45,11),(54,17),(49,29),(49,42),(43,50),(15,48),(11,39)],(60,124,134)),
        ([(20,13),(46,14),(50,18),(44,39),(46,46),(14,46),(18,26)],(91,153,157)),
        ([(17,12),(25,10),(46,12),(52,16),(46,19),(23,16),(15,19)],(153,192,183))]:
        d.polygon(points,fill=tone)
    d.line([(2,41),(6,39),(10,32),(11,26)],fill=(70,82,86),width=2)
    d.line([(53,24),(57,23),(60,21)],fill=(103,115,118),width=2)
    r = Random(664)
    for _ in range(70):
        a = r.randint(17,47)
        b = r.randint(18,40)
        d.line((a,b,a-1,b+r.randint(2,7)),fill=r.choice(((116,169,168),(82,143,152),(151,192,183))))
    for _ in range(125):
        a,b = r.randint(5,60),r.randint(42,62)
        if ((a-33)/29)**2+((b-51)/12)**2<1:
            d.line((a,b,a+r.randint(1,5),b),fill=r.choice(((114,169,167,160),(184,211,196,220),(213,225,208,234))))
    im.save(ROOT/"outlet-fall.png",optimize=True)
    return {"asset":"outlet-fall","x":32*64-8,"y":68*64+29,"width":2*width,"height":2*height}


def bake(path, heights_path=None, natural=False):
    polish = heights_path is not None
    output = ROOT.with_name("water_v6_6_2" if natural else "water_v6_6_1") if polish else ROOT
    output.mkdir(parents=True,exist_ok=True)
    sites,bridge = {},None
    with open(path,encoding="utf-8-sig",newline="") as file:
        for row in csv.reader(file):
            if row[0]=="bridge":
                bridge = tuple(map(int,row[1:]))
                continue
            site,x,y,wet,mask,biome,road,tree = row
            sites.setdefault(site,{})[int(x),int(y)] = (wet=="1",mask,biome,road=="1",tree=="1")
    scenes = {}
    for site,rows in sites.items():
        origin = min(x for x,y in rows),min(y for x,y in rows)
        size = max(x for x,y in rows)+1-origin[0],max(y for x,y in rows)+1-origin[1]
        image,restore,motion,basin = shade_water(site,rows,origin,size,bridge,polish,natural)
        assert all(basin[(y-origin[1])*GRID+16,(x-origin[0])*GRID+16]==cell[0]
                   for (x,y),cell in rows.items()),site
        parts = []
        for y in range(0,image.height,128):
            for x in range(0,image.width,128):
                piece = image.crop((x,y,min(image.width,x+128),min(image.height,y+128)))
                if piece.getchannel("A").getbbox():
                    asset = f"{site}-{x}-{y}"
                    target=output/f"{asset}.png"
                    identical=False
                    if polish and target.exists():
                        with Image.open(target) as previous:
                            identical=previous.tobytes()==piece.tobytes()
                    if not identical:
                        piece.save(target,optimize=True)
                    parts.append({"asset":asset,"x":2*x+64*origin[0],"y":2*y+64*origin[1],
                                  "width":2*piece.width,"height":2*piece.height})
        scenes[site] = {"parts":parts,"restore":restore,"motion":motion,
                        "bounds":{"x":64*origin[0],"y":64*origin[1],"width":64*size[0],"height":64*size[1]}}
    if natural:
        west,east,row = bridge
        deck = {"asset":"river-bridge","x":west*64+8,"y":row*64-10,
                "width":(east-west+1)*64-16,"height":74}
        scenes["river"]["structure"] = deck
        scenes["river"]["foreground"] = dict(deck,asset="river-rail",**{"foot-y":row*64+53})
    else:
        scenes["river"]["structure"] = bridge_image(*bridge,output=output,polish=polish)
    if polish and not natural:
        scenes["river"]["foreground"] = scenes["river"]["structure"].pop("rail")
    if polish:
        from water_outlet import paint
        fall = paint(output,heights_path,natural)
    else:
        fall = waterfall()
    scenes["fall"] = {"structure":fall,"bounds":{k:v for k,v in fall.items() if k!="asset"}}
    manifest = "water_studies_662.edn" if natural else ("water_studies_661.edn" if polish else "water_studies.edn")
    Path("resources",manifest).write_text(edn(scenes)+"\n",encoding="utf-8")


if __name__ == "__main__":
    bake(sys.argv[1],sys.argv[2] if len(sys.argv)>2 else None,"--natural" in sys.argv[3:])
