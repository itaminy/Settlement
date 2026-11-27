"""Final surface and prop treatments for the two V6.4.7 studies."""
import random

import numpy as np
from PIL import Image, ImageDraw

import relief_walls
from generate_relief_demos import GRID, pixel
from relief_raster import smooth

shape = relief_walls.shape


def material(scene, x, y, z, slope, light, strata, grit, salt):
    color = relief_walls.material(scene,x,y,z,slope,light,strata,grit,salt)
    if scene.name == "canyon":
        flat = (1-smooth(abs(z)/10))*(1-smooth((slope-.17)/.71))
        ripple = np.sin(y*.21+x*.063+.48*np.sin(x*.037))
        grain = (grit-.5)*9+np.select([salt>.89,salt<.11],[4,-5])
        sand = (np.array([181,133,82])+((ripple>.85)*3+grain)[...,None])
        sand *= np.minimum(light,1.13)[...,None]
        color += (sand-color)*flat[...,None]
        bedding = relief_walls.bands(x,y,z)
        wall = smooth((slope-.42)/1)*smooth((abs(z)-4)/15)
        color += (np.where(bedding<.13,-9,np.where(bedding>.83,8,0))*wall)[...,None]
    else:
        edge = smooth((z-15)/20)*(1-smooth((z-60)/35))
        color += ((grit-.53)*12*edge)[...,None]
        color += (edge*(1-smooth((slope-.4)/1.2)))[...,None]*[2,4,3]
    return np.clip(color,0,255)


def tint(color, amount):
    return tuple(int(c) for c in np.clip(color+amount,0,255))


def details(scene, view, bounds, color, light, blend, draw_stairs=True):
    left,top,right,bottom = bounds
    region = np.s_[top:bottom,left:right]
    z, slope, opacity = (values[region] for values in
                         (view.visible_height,view.slope,view.opacity))
    layer = Image.new("RGBA",(right-left,bottom-top))
    draw = ImageDraw.Draw(layer)
    rng = np.random.default_rng(647)
    # Loose fans of stones, with open sand between groups.
    edges = np.argwhere((abs(z)<7)&(slope>.65)&(blend>.6))
    for i in rng.choice(len(edges),min(29,len(edges)),replace=False):
        cy,cx = edges[i]
        if (cx//21+cy//19)%5==1:
            continue
        for _ in range(int(rng.integers(2,6))):
            x,y = cx+rng.integers(-10,11),cy+rng.integers(1,12)
            if not (6<x<layer.width-6 and 6<y<layer.height-6):
                continue
            if abs(z[y,x])>8 or blend[y,x]<.56:
                continue
            stone(draw,x,y,int(rng.integers(1,5)),color[y,x])
    if scene.name == "canyon":
        portal = Image.new("RGBA",layer.size)
        cave(ImageDraw.Draw(portal),scene,view,left,top,color,refine=not draw_stairs)
        alpha = np.asarray(portal.getchannel("A"))
        portal.putalpha(Image.fromarray(np.uint8(alpha*smooth((z-2)/9)*opacity)))
        layer.alpha_composite(portal)
    else:
        ux,uy = pixel(scene,(14,68))
        x,y = ux-left,uy-top+4
        if draw_stairs:
            stairs(draw,x,y,color,light)
        shore_x,_ = pixel(scene,(19.5,68))
        _,shore_y = pixel(scene,(20,68.5))
        for y,x in np.argwhere((opacity>.36)&(opacity<.91)&(abs(z)<4)):
            if not (shore_x<x+left<shore_x+2*GRID and abs(y+top-shore_y)<3*GRID):
                continue
            if x<5 or x>=layer.width-5 or y<5 or y>=layer.height-5 or rng.random()>.033:
                continue
            stone(draw,x,y,int(rng.integers(1,3)),color[y,x]*.72+[21,24,22])
    return layer


def stone(draw,x,y,r,tone):
    draw.polygon([(x-r,y),(x+1,y-r//2),(x+r+1,y),(x+1,y+2)],
                 fill=(*tint(tone,-16),172))
    draw.polygon([(x-r,y),(x-1,y-r//2-1),(x+r,y-1),(x+1,y+1)],
                 fill=(*tint(tone,13),218))


def stairs(draw,x,y,color,light):
    # Centre and heading match the pre-existing treads on the camp trail.
    for i in range(-2,3):
        u = x+i*5
        head = y-10+(2,0,1,-1,1)[i+2]
        tail = y+10-(1,2,0,2,3)[i+2]
        tone = np.array([116,113,98])*min(light[y,u],1.18)
        draw.polygon([(u-3,head+1),(u+1,head),(u+2,tail-1),(u-2,tail)],
                     fill=(*tint(tone,8),255))
        draw.polygon([(u+1,head),(u+3,head+2),(u+3,tail-2),
                      (u,tail+1),(u-1,y+1)],fill=(*tint(tone,-27),255))
        draw.line([(u-2,head+1),(u-1,y-3),(u-2,tail-1)],
                  fill=(*tint(tone,23),255))
        draw.point((u-1,tail),fill=(*tint(color[y,u],-8),228))


def cave(draw,scene,view,left,top,color,refine=False):
    cx,cy = pixel(scene,(82.2,65.6))
    face = (view.slope>.76)&(view.visible_height>7)&(view.opacity>.9)
    score,u,v = max((float(face[y-20:y+20,x-21:x+21].mean()),x,y)
                    for x in range(cx-48,cx+49,4)
                    for y in range(max(24,cy-51),cy+41,3))
    if score<.36:
        return
    x,y = u-left,v-top
    p = color[y,x]
    def poly(points,tone,alpha=255):
        draw.polygon([(x+a,y+b) for a,b in points],fill=(*tone,alpha))
    poly([(-33,19),(-27,9),(-25,-5),(-18,-15),(-7,-23),(6,-19),
          (13,-19),(25,-10),(33,13),(24,22),(-7,23)],tint(p,-8),208)
    poly([(-27,16),(-22,2),(-14,-13),(-7,-17),(0,-15),(8,-16),
          (18,-10),(24,0),(27,13),(17,18),(-9,18)],(60,39,34))
    poly([(-19,13),(-14,1),(-8,-12),(1,-13),(6,-11),(12,-12),
          (19,-5),(22,6),(20,14),(7,17),(-5,14)],(29,25,27))
    poly([(-31,16),(-24,8),(-19,11),(-13,15),(-1,16),(7,15),
          (19,17),(29,12),(25,21),(9,23),(-13,20)],tint(p,-3))
    for a,b,d in [(-18,-14,14),(-7,-20,19),(10,-15,10),(23,-5,-15),(-22,8,9),(9,19,12)]:
        poly([(a-4,b+2),(a,b),(a+5,b+1),(a+1,b+4)],tint(p,d))
    if refine:
        draw.line([(x-18,y+10),(x-12,y+7),(x-5,y+8)],fill=(69,49,39,255))
        draw.line([(x+9,y+10),(x+16,y+7),(x+20,y+8)],fill=(47,34,32,255))
        for a,b in [(-23,16),(-18,18),(10,19),(23,15)]:
            poly([(a-2,b),(a,b-2),(a+2,b),(a+1,b+1)],tint(p,11))


def finish_props(output, integrated=False):
    root = output.parent
    sources = {
        "camp-mountain": root/"visual_v6_3_1/camp1.png",
        "camp-canyon": root/"visual_v6_4/camp0.png",
        "crate": root/"visual_v3/resources/crate.png",
        "barrel": root/"visual_v3/resources/barrel.png",
        "sign": root/"visual_v3/buildings/sign.png",
        "north-deck": root/"relief_v6_4_3/north-deck.png",
    }
    if integrated:
        sources = {"east-deck": root/"relief_v6_4_3/east-deck.png"}
        for kind,folder,count in [("mountain","visual_v6_3_1",5),("canyon","visual_v6_4",3)]:
            for i in range(count):
                sources[f"{kind}-camp-camp{i}"] = root/f"{folder}/camp{i}.png"
    for name,path in sources.items():
        with Image.open(path) as original:
            pixels = np.asarray(original.convert("RGBA"),dtype=np.float32).copy()
        y,x = np.mgrid[:pixels.shape[0],:pixels.shape[1]]
        # A warm upper-left light, short seams and restrained material grain.
        rng = random.Random(647)
        grain = np.asarray([rng.randrange(-2,3) for _ in range(((x.max()//2)+1)*((y.max()//2)+1))])
        grain = grain.reshape(y.max()//2+1,x.max()//2+1)[y//2,x//2]
        level = 1.13-.10*x/max(1,x.max())-.16*y/max(1,y.max())
        rgb = pixels[:,:,:3]
        lit = np.max(rgb,axis=-1)>45
        seam = ((y+x//3)%9==0)&((x//8+y//7)%4!=0)
        rgb *= level[...,None]
        rgb += (grain*2-4*seam)[...,None]*lit[...,None]
        rgb += np.array([4,3,0])*lit[...,None]
        Image.fromarray(np.uint8(np.clip(pixels,0,255))).save(output/f"{name}.png",optimize=True)
