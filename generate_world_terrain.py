"""Bake World V7's meadow, marsh and desert materials over the existing map."""
import csv
from pathlib import Path
from random import Random
import sys

import numpy as np
from PIL import Image, ImageDraw
from scipy.ndimage import gaussian_filter

from generate_massive_mountain import edn

ROOT = Path("resources/images/world_v7_ground")
PIXELS = 32
SPAN = 8
HALO = 2
SPECIES = {"plains": 1, "swamp": 2, "desert": 3}


def smooth(t):
    u = np.clip(t, 0, 1)
    return u*u*(3-2*u)


def material(region, x, y, grit, broad, patch, left, top, rng):
    """Color and detail follow world-space forms rather than tile boundaries."""
    if region == "plains":
        meadow = smooth(patch*.68+broad*.55+.15)
        base = np.array([115,139,81]) + meadow[...,None]*[13,13,1]
        bare = smooth((.30-broad-.55*patch)*2.4)
        base += bare[...,None]*[18,-13,-13]
        tone = grit*4 + patch*4 + broad*3
    elif region == "swamp":
        hummock = smooth(patch*.74+broad*.48+.16)
        base = np.array([72,78,58])+hummock[...,None]*[26,32,14]
        sediment = smooth((.29-patch*.53-broad*.78)*1.65)
        base += sediment[...,None]*[2,-13,-8]
        tone = grit*4 + patch*3 + broad*3
    else:
        dune = np.sin(x*.018-y*.020+1.1*np.sin(y*.029)+.73*np.sin(x*.008+y*.016))
        base = np.array([187,145,91])+np.trunc(dune*5)[...,None]*[1,.85,.55]
        crests = np.sin(x*.24-y*.238+2.3*dune+.9*np.sin(y*.090))
        wind_rows = ((crests>.83)&(patch+broad>-.12))*2.5
        tone = grit*3 + broad*6 + wind_rows
    rgb = np.uint8(np.clip(base+np.trunc(tone)[...,None],0,255))
    art = Image.fromarray(rgb,mode="RGB")
    d = ImageDraw.Draw(art)

    height,width = patch.shape
    # Scatter small authored motifs, with their density clustered by the same field.
    # A separate per-cell PRNG fixes their anchor even across chunks and seed exports.
    if region == "plains":
        greens = [(78,107,62),(95,126,68),(103,133,72),(144,158,91)]
        flowers = [(228,212,154),(207,153,120),(189,173,209),(237,207,99)]
    elif region == "swamp":
        greens = [(61,77,56),(79,100,61),(105,123,69),(135,143,85)]
    else:
        greens = [(145,114,75),(159,122,81),(176,140,89),(211,170,107)]
    for row in range(height//PIXELS):
        for col in range(width//PIXELS):
            wx,wy = left+col,top+row
            cell = rng(wx,wy)
            for _ in range(15 if region=="plains" else 10):
                u,v = col*PIXELS+cell.randrange(3,30),row*PIXELS+cell.randrange(3,30)
                wet_mud = region=="swamp" and broad[v,u]+patch[v,u]<-.15
                if wet_mud and cell.random()<.8:
                    continue
                if region=="desert":
                    if cell.random()<.65:
                        d.line((u,v,u+cell.randrange(1,4),v+1),fill=greens[cell.randrange(4)])
                        continue
                    d.polygon([(u-1,v),(u,v-2),(u+3,v-1),(u+4,v+1),(u,v+2)],fill=greens[0])
                    d.line((u,v-1,u+2,v-1),fill=greens[3])
                    continue
                n=cell.randrange(1,5 if patch[v,u]>.10 else 3)
                dx=cell.choice((-1,0,1))
                d.line((u+1,v+1,u+dx+1,v-n+1),fill=greens[0])
                d.line((u,v,u+dx,v-n),fill=greens[cell.randrange(1,4)])
                if region=="plains" and patch[v,u]>.04 and cell.random()<.15:
                    d.point((u+dx,v-n-1),fill=cell.choice(flowers))
                    if n>2:
                        d.point((u+dx-1,v-n-1),fill=cell.choice(flowers))
                elif region=="swamp" and not wet_mud and n>=3:
                    d.point((u+dx-1,v-n),fill=greens[3])
    return np.asarray(art)


def existing_variant(source, output, factors, scales):
    with Image.open(source) as img:
        image=img.convert("RGBA")
    # Color unification uses the original pixel silhouettes, masks and anchors.
    for index,(multipliers,stretch) in enumerate(zip(factors,scales)):
        pixels=np.asarray(image).copy()
        pixels[:,:,:3]=np.uint8(np.clip(np.round(pixels[:,:,:3]*multipliers),0,255))
        result=Image.fromarray(pixels)
        if stretch!=(1,1):
            width,height=image.size
            result=result.resize((round(width*stretch[0]),round(height*stretch[1])),
                                 Image.Resampling.NEAREST)
            canvas=Image.new("RGBA",image.size)
            canvas.alpha_composite(result,((width-result.width)//2,height-result.height))
            result=canvas
        result.save(ROOT/f"{output}-{index}.png",optimize=True)


def bake_plants():
    old=Path("resources/images/visual_v3")
    for file,kind in [("palm","palm"),("cactus","cactus"),("flowering_cactus","flowering-cactus"),
                      ("dry_bush","dry-bush")]:
        existing_variant(old/"desert"/f"{file}.png",kind,
                         [(1.00,.99,.96),(.91,.98,.96),(1.04,1.02,.95)],
                         [(1,1),(.83,.90),(.96,.76)])
    for file,kind in [("tall_grass","tall-grass"),("fern","fern"),("reeds","reeds"),
                      ("bush","bush"),("yellow_flowers","yellow-flowers")]:
        existing_variant(old/"vegetation"/f"{file}.png",kind,
                         [(.98,1.01,.96),(.96,.95,.99),(.94,.99,1.02)],
                         [(1,1),(.86,.91),(.92,.81)])
    for index in range(3):
        lily=Image.new("RGBA",(42,34))
        d=ImageDraw.Draw(lily)
        rng=Random(7043+index)
        for _ in range(4):
            x,y=rng.randrange(8,33),rng.randrange(8,26)
            r=rng.randrange(2,5)
            d.line((x-r-1,y+2,x+r+2,y+1),fill=(51,82,61,100))
            polygon=[(x-r,y),(x-r+1,y-2),(x,y-3),(x+r-1,y-1),(x+1,y),(x+r,y),
                     (x+r-1,y+2),(x+1,y+3),(x-r+1,y+2)]
            d.polygon(polygon,fill=(87,117,71,245))
            d.line((x-r+1,y-1,x,y-2,x+r-2,y-1),fill=(132,150,88,246))
        if index!=1:
            d.polygon([(18,14),(20,11),(23,13),(23,15),(20,16)],fill=(181,168,145,250))
            d.point((21,13),fill=(213,191,116,255))
        lily.save(ROOT/f"lily-{index}.png",optimize=True)


def bake(source):
    rows={}
    with open(source,encoding="utf-8-sig",newline="") as file:
        for row in csv.reader(file):
            if row[0]=="seed":
                seed=int(row[1])
            else:
                x,y,kind,road,occupied,decor=row
                rows[int(x),int(y)] = (kind,road=="1",occupied=="1",decor=="1")
    width=max(x for x,y in rows)+1
    height=max(y for x,y in rows)+1
    ROOT.mkdir(parents=True,exist_ok=True)
    bake_plants()
    rng=lambda x,y: Random(seed ^ (((x+1)*19349663)^((y+1)*73856093)))
    chunks=[]
    for top in range(0,height,SPAN):
        for left in range(0,width,SPAN):
            right,bottom=min(left+SPAN,width),min(top+SPAN,height)
            kinds={rows[x,y][0] for y in range(top,bottom) for x in range(left,right)}
            kinds &= SPECIES.keys()
            if not kinds:
                continue
            x0,y0=max(0,left-HALO),max(0,top-HALO)
            x1,y1=min(right+HALO,width),min(bottom+HALO,height)
            shore=np.zeros(((y1-y0)*PIXELS,(x1-x0)*PIXELS),dtype=np.float32)
            species=shore.copy()
            road=shore.copy()
            for cy in range(y0,y1):
                for cx in range(x0,x1):
                    cell=rows[cx,cy]
                    a,b=(cx-x0)*PIXELS,(cy-y0)*PIXELS
                    s=np.s_[b:b+PIXELS,a:a+PIXELS]
                    species[s]=SPECIES.get(cell[0],0)
                    road[s]=1 if cell[1] else 0
                    shore[s]=0 if cell[0] in ("woods","protected") else 1
            y,x=np.mgrid[y0*PIXELS:y1*PIXELS,x0*PIXELS:x1*PIXELS].astype(np.float32)
            broad=(np.sin(x*.009+y*.012+seed%17)+np.sin(x*.020-y*.015))*.48
            patch=(np.sin(x*.046+y*.051)+np.sin(x*.057-y*.035+2*broad))*.44
            raw=np.bitwise_xor(x.astype(np.int32)*19349663,
                               y.astype(np.int32)*83492791)^seed
            grit=np.mod(np.bitwise_xor(raw>>15,raw>>5),15)*(.1333)-1
            outline=gaussian_filter(shore,13)
            carriage=gaussian_filter(road,8)
            # Defer to existing terrain art on either side of a softened ecotone,
            # and leave the exact original road, water and forest center intact.
            soft=smooth((outline-.43)/.52)*(1-smooth(carriage/.62))*shore*(1-road)
            combined=np.zeros((*shore.shape,4),dtype=np.uint8)
            color=np.zeros((*shore.shape,3),dtype=np.float32)
            total=np.zeros(shore.shape,dtype=np.float32)
            for region,number in SPECIES.items():
                if not np.any(species==number):
                    continue
                w=gaussian_filter((species==number).astype(np.float32),14)
                color+=material(region,x,y,grit,broad,patch,x0,y0,rng)*w[...,None]
                total+=w
            combined[:,:,:3]=np.uint8(np.clip(color/np.maximum(total,.00001)[...,None],0,255))
            combined[:,:,3]=np.uint8(np.clip(soft*237,0,255))
            area=np.s_[(top-y0)*PIXELS:(bottom-y0)*PIXELS,
                      (left-x0)*PIXELS:(right-x0)*PIXELS]
            image=Image.fromarray(combined[area],"RGBA")
            asset=f"ground-{left}-{top}"
            image.save(ROOT/f"{asset}.png",optimize=True)
            chunks.append({"asset":asset,"x":left*64,"y":top*64,
                           "width":(right-left)*64,"height":(bottom-top)*64})
    Path("resources/world_terrain.edn").write_text(edn({"seed":seed,"chunks":chunks})+"\n",encoding="utf-8")


if __name__=="__main__":
    bake(sys.argv[1])
