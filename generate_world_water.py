"""Bake the approved V6.6.2 water materials in overlapping world chunks."""
import csv
from pathlib import Path
import sys

from PIL import Image

from generate_water_studies import GRID, ROOT, bridge_image, edn, shade_water

SPAN = 10
HALO = 5
OUTPUT = ROOT.with_name("water_v7")


def save(image, asset):
    target = OUTPUT/f"{asset}.png"
    if target.exists():
        with Image.open(target) as prior:
            if prior.size==image.size and prior.tobytes()==image.tobytes():
                return
    image.save(target,optimize=True)


def bake(path):
    rows,river,bridges = {},{},[]
    with open(path,encoding="utf-8-sig",newline="") as file:
        for line in csv.reader(file):
            if line[0]=="seed":
                seed = int(line[1])
            elif line[0]=="flow":
                river[int(line[1])] = float(line[2])
            elif line[0]=="bridge":
                bridges.append(tuple(map(int,line[1:])))
            else:
                _,x,y,wet,mask,biome,road,tree = line
                rows[int(x),int(y)] = (wet=="1",mask,biome,road=="1",tree=="1")
    width,height = max(x for x,y in rows)+1,max(y for x,y in rows)+1
    OUTPUT.mkdir(parents=True,exist_ok=True)
    chunks = []
    for y in range(0,height,SPAN):
        for x in range(0,width,SPAN):
            r,b = min(x+SPAN,width),min(y+SPAN,height)
            l,t = max(0,x-HALO),max(0,y-HALO)
            u,v = min(r+HALO,width),min(b+HALO,height)
            limits = [(a,c) for c in range(max(0,y-1),min(height,b+1))
                      for a in range(max(0,x-1),min(width,r+1))]
            if not any(rows[p][0] or rows[p][1]!="1111" for p in limits):
                continue
            area = {(a,c):rows[a,c] for c in range(t,v) for a in range(l,u)}
            image,restore,motion,basin = shade_water(
                "world",area,(l,t),(u-l,v-t),None,True,True,river)
            assert all(bool(basin[(c-t)*GRID+16,(a-l)*GRID+16])==rows[a,c][0]
                       for c in range(y,b) for a in range(x,r))
            crop = image.crop(((x-l)*GRID,(y-t)*GRID,(r-l)*GRID,(b-t)*GRID))
            name = f"world-{x}-{y}"
            save(crop,name)
            bounds = {"x":x*64,"y":y*64,"width":(r-x)*64,"height":(b-y)*64}
            chunks.append(dict(bounds,asset=name,
                               restore=[p for p in restore if x<=p[0]<r and y<=p[1]<b],
                               motion=[p for p in motion if x*64<=p[0]<r*64 and y*64<=p[1]<b*64]))
    decks,rails = [],[]
    for west,east,row in bridges:
        if row==36:
            deck = {"asset":"river-bridge","x":west*64+8,"y":row*64-10,
                    "width":(east-west+1)*64-16,"height":74}
            rail = dict(deck,asset="river-rail",**{"foot-y":row*64+53})
        else:
            deck = bridge_image(west,east,row,output=OUTPUT,polish=True,suffix=f"-{row}")
            rail = deck.pop("rail")
        decks.append(deck)
        rails.append(rail)
    Path("resources/water_world.edn").write_text(edn(
        {"seed":seed,"chunks":chunks,"decks":decks,"rails":rails,"bridge-cells":
         [(x,y) for west,east,y in bridges for x in range(west,east+1)]})+"\n",encoding="utf-8")


if __name__=="__main__":
    bake(sys.argv[1])
