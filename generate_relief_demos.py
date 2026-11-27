"""Author and bake a mountain and a gorge for the opt-in V6.4.1 Swing demo."""
from pathlib import Path
from dataclasses import dataclass

import numpy as np
from PIL import Image, ImageDraw, ImageFilter

from generate_massive_mountain import geometry as mountain_layout, edn
from generate_desert_canyon_layout import geometry as canyon_layout
from relief_raster import PROJECTION, illumination, project, smooth

ROOT = Path("resources/images/relief_v6_4_1")
GRID = 32
MATERIALS = {
    "mountain": [(65,70,73),(115,117,111),(153,153,140),(174,191,196),(218,222,209)],
    "canyon": [(107,65,45),(164,99,65),(195,129,76),(193,142,91),(226,173,111)],
}


@dataclass(frozen=True)
class Scene:
    name: str
    origin: tuple
    size: tuple
    blocks: set
    pits: set
    routes: set
    props: dict


def mountain():
    trail,pads,blocks,props=mountain_layout()
    return Scene("mountain",(7,77),(20,19),blocks,set(),set(trail),props)


def canyon():
    data=canyon_layout()
    return Scene("canyon",(76,62),(21,13),set(data["walls"]),
                 set(data["pits"])|set(data["bridges"]),
                 set(data["trail"]).union(*data["branches"]),{tuple(p):kind for p,kind in data["props"]})


def pixel(scene, point):
    return tuple(round((x-o+.5)*GRID) for x,o in zip(point,scene.origin))


def mask(scene, cells):
    width,height=(t*GRID for t in scene.size)
    image=Image.new("L",(width,height))
    d=ImageDraw.Draw(image)
    for p in cells:
        u,v=pixel(scene,p)
        d.rectangle((u-16,v-16,u+15,v+15),fill=255)
    return image


def grain(rng, width, height, cols, rows):
    samples=rng.random((rows,cols),dtype=np.float32)
    return np.asarray(Image.fromarray(samples).resize((width,height),Image.Resampling.BILINEAR))


def ridge(x,y,cx,cy,rx,ry,lean):
    spread=abs((x-cx-(y-cy)*lean)/rx)
    reach=abs((y-cy)/ry)
    return np.maximum(0,1-spread**1.28-reach**1.63)**1.14


def geometry(scene,x,y,broad,grainage):
    cliff=mask(scene,scene.blocks)
    rock=np.asarray(cliff,dtype=np.float32)/255
    shoulder=np.asarray(cliff.filter(ImageFilter.GaussianBlur(6)),dtype=np.float32)/255
    border=shoulder+.16*(broad-.5)+.07*(grainage-.5)
    relief=smooth((border-.57)/.39)*rock
    if scene.name=="mountain":
        bulk=(100*ridge(x,y,12.3,86.1,5.1,7.0,.18)
              +195*ridge(x,y,19.8,85.8,6.9,7.9,-.41)
              +62*ridge(x,y,22.2,88.3,6.2,6.0,.74))
        erosion=10*(broad-.45)+4*(grainage-.5)
        # Uneven terraces are interpolated over several pixels, with short steep risers.
        levels=(bulk+erosion+6*np.sin(x*.7+y*.39))/19
        stepped=(np.floor(levels)+smooth((levels%1-.10)/.46))*19
        splinters=6*np.sin(x*2.2+y*.81)+3*np.sin(x*4.3-y*1.7)
        return np.float32(np.maximum(0,bulk*.78+stepped*.22+splinters*relief)*relief)
    banks=(75*ridge(x,y,82,65.0,5.8,5.1,.54)
           +68*ridge(x,y,91.4,70.6,5.3,4.9,-.65)
           +51*ridge(x,y,81.4,73.2,4.5,4.5,.51))
    pit=mask(scene,scene.pits)
    basin=np.asarray(pit.filter(ImageFilter.GaussianBlur(5)),dtype=np.float32)/255
    trench=smooth((basin+.12*(broad-.5)-.53)/.43)*(np.asarray(pit)>0)
    axis=87.5+.79*np.sin((y-65)*.62)+.15*np.sin(y*1.36)
    throat=np.exp(-((x-axis)/1.30)**2)
    hollow=31+57*throat+11*broad+5*np.sin(y*1.42+x*2.2)
    return np.float32((banks+11*(broad-.5))*relief-hollow*trench)


def object_heights(scene, height):
    """Small shadow occluders only affect light, leaving the surface geometry intact."""
    y,x=np.mgrid[:height.shape[0],:height.shape[1]]
    blockers=height.copy()
    sizes={"tent":25,"raider-tent":25,"saguaro":27,"campfire":3,"blue-flag":15}
    selected={p:sizes.get(kind,8) for p,kind in scene.props.items()}
    if scene.name=="mountain":
        selected.update({(19,95):32,(23,94):32})
    for p,peak in selected.items():
        cx,cy=pixel(scene,p)
        if -20<=cx<height.shape[1]+20 and -20<=cy<height.shape[0]+20:
            dome=np.maximum(0,1-((x-cx)/12)**2-((y-cy)/8)**2)*peak
            blockers=np.maximum(blockers,height+dome)
    if scene.name=="canyon":
        left,deck=pixel(scene,(85.8,68))
        right,_=pixel(scene,(88.6,68))
        blockers=np.where((x>=left)&(x<=right)&(abs(y-deck)<9),np.maximum(blockers,1),blockers)
    return blockers


def material(scene,x,y,z,slope,illum,noise,stripe,stone):
    rock=np.asarray(MATERIALS[scene.name],dtype=np.float32)
    wall=smooth((slope-.39)/.93)
    if scene.name=="mountain":
        snow=smooth((z-58+18*(noise-.5))/45)
        floor=np.array([100,115,106])+smooth((y-79)/9)[...,None]*[36,43,41]
        crown=rock[2]+snow[...,None]*(rock[4]-rock[2])
        vertical=rock[1]+snow[...,None]*(rock[3]-rock[1])
        weight=smooth((z-3)/25)
    else:
        floor=np.broadcast_to(rock[3],(*z.shape,3))
        crown=np.broadcast_to(rock[2],(*z.shape,3))
        vertical=np.broadcast_to(rock[1],(*z.shape,3))
        weight=smooth((abs(z)-3)/16)
        depth=smooth(-z/98)
        floor=floor-depth[...,None]*[41,32,21]
        vertical=vertical-depth[...,None]*[28,17,11]
    base=floor+(crown-floor)*weight[...,None]
    base=base+(vertical-base)*(wall*.93)[...,None]
    # Sandstone and schist traces follow physical elevation on exposed faces.
    stratum=(z*.112+x*.031+stripe*.74+.23*np.sin(x*.08+stripe*3))%1
    fault=(x*.097+z*.025+stripe*.65)%1
    panels=(np.floor((x+9*np.sin(z*.067)+17*stripe)/17)%4-1.5)*8
    etched=(np.where(stratum<.15,-21,np.where(stratum>.83,21,3))+
             np.where((fault<.14)&(stone>.37),-19,0)+panels)
    granules=np.rint(((noise-.5)*29+(stone-.5)*18)/4)*4
    tone=granules+wall*etched+weight*(stripe-.54)*16
    illum=illum+(1-illum)*wall*.21
    return np.uint8(np.clip((base+tone[...,None])*illum[...,None],0,255))


def painted(scene, x, y, height, shade, opacity, slope, fine, streak, rock, flat=False):
    world_y=y/GRID+scene.origin[1]-.5
    def sample(row, cols, z, face, fraction):
        # Quantized luma bands retain pixel edges without a blur pass.
        light=np.rint((shade[row,cols]+(shade[row+1,cols]-shade[row,cols])*fraction)*23)/23
        if flat:
            face=face*.3
        return material(scene,x[row,cols],world_y[row,cols],z,face,light,
                        fine[row,cols],streak[row,cols],rock[row,cols])
    return project(height,sample,opacity,slope,0 if flat else PROJECTION)


def cave_in_wall(canvas, view, scene):
    # Score a footprint on the *visible* north wall. The opening uses the same
    # projected surface mask as the cliff, so its paint cannot spill onto grade.
    target_x,target_y=pixel(scene,(82.2,65.2))
    face=(view.slope>.75)&(view.visible_height>5)
    score,x,yy=max((np.mean(face[y-23:y+23,u-22:u+23]),u,y)
                   for u in range(target_x-53,target_x+54,4)
                   for y in range(max(24,target_y-65),target_y+42,3))
    if score<.46:
        raise ValueError("The canyon mouth no longer fits the projected rock face")
    portal=Image.new("RGBA",canvas.size)
    d=ImageDraw.Draw(portal)
    arch=[(x-28,yy+22),(x-27,yy-2),(x-20,yy-17),(x-10,yy-27),
          (x+5,yy-31),(x+19,yy-23),(x+26,yy-12),(x+30,yy+23)]
    d.polygon(arch+[(x+16,yy+30),(x-21,yy+27)],fill=(104,62,44))
    opening=[(x-21,yy+23),(x-20,yy+2),(x-14,yy-13),(x-4,yy-19),
             (x+9,yy-18),(x+18,yy-8),(x+23,yy+23)]
    d.polygon(opening,fill=(56,38,34))
    d.polygon([(x-13,yy+21),(x-9,yy+2),(x+1,yy-5),(x+14,yy+6),(x+19,yy+22)],
              fill=(40,31,31))
    d.line(arch[:6],fill=(190,126,80),width=3)
    for p,q in [(2,0),(3,1),(4,2),(5,3),(6,4)]:
        a,b=arch[p],opening[q]
        d.line((a,b),fill=(83,49,39),width=2)
    d.line([(x-29,yy+26),(x-13,yy+28),(x+13,yy+30),(x+31,yy+26)],
           fill=(154,99,66),width=3)
    alpha=np.asarray(portal.getchannel("A"),dtype=np.float32)
    on_rock=smooth((view.visible_height-1)/7)
    portal.putalpha(Image.fromarray(np.uint8(alpha*on_rock)))
    canvas.alpha_composite(portal)


def tread(canvas, scene, view, lit):
    d=ImageDraw.Draw(canvas)
    for cell in scene.routes:
        x,y=pixel(scene,cell)
        if not (16<x<canvas.width-16 and 16<y<canvas.height-16):
            continue
        if cell in scene.pits:
            continue
        for k in range(10):
            u=x+(k*37%25)-12
            v=y+(k*13%13)-4+(k%4)*3
            if view.opacity[v,u]>.5 and abs(view.visible_height[v,u])<4:
                r,g,b,a=canvas.getpixel((u,v))
                step=9 if k%3 else -8
                d.line((u,v,u+2+(k%4),v),fill=(r+step,g+step,b+step,a))
    if scene.name=="mountain":
        x,y=pixel(scene,(12,86))
        for i in range(7):
            a=y-15+i*5
            d.polygon([(x-13,a-2),(x+11+(i%3),a-3),(x+13,a+1),(x-12,a+2)],
                      fill=(119,137,136))
            d.line((x-12,a+2,x+13,a+1),fill=(61,76,80),width=2)
            d.line((x-11,a-2,x+10+(i%3),a-3),fill=(190,198,184) if lit else (147,154,148))


def image(view):
    return Image.fromarray(np.dstack((view.rgb,np.uint8(np.clip(view.opacity,0,1)*255))))


def foreground(scene, canvas, view, mode, root=ROOT):
    anchor=(13,89) if scene.name=="mountain" else (90,72)
    x,y=pixel(scene,anchor)
    columns=np.arange(canvas.width)[None,:]
    rows=view.source_y
    mask=(abs(columns-x)<16)&(rows>=y-16)&(rows<y+16)&(view.visible_height>7)
    if not np.any(mask):
        return None
    selection=Image.fromarray(np.uint8(mask)*255)
    bounds=selection.getbbox()
    cut=canvas.copy()
    cut.putalpha(selection)
    file=f"{scene.name}-{mode}-front"
    cut.crop(bounds).save(root/f"{file}.png")
    u,v,r,b=bounds
    return {"asset":file,"anchor":anchor,"x":2*u,"y":2*v,"width":2*(r-u),
            "height":2*(b-v),"foot-y":2*(y+15)}


def bake(scene):
    rng=np.random.default_rng(64113 if scene.name=="mountain" else 64126)
    w,h=(n*GRID for n in scene.size)
    y,x=np.mgrid[0:h,0:w].astype(np.float32)
    fine=grain(rng,w,h,255,267)
    broad=grain(rng,w,h,18,19)
    streak=grain(rng,w,h,70,49)
    rock=grain(rng,w,h,141,119)
    z=geometry(scene,x/GRID+scene.origin[0]-.5,y/GRID+scene.origin[1]-.5,broad,rock)
    dy,dx=np.gradient(z)
    slope=np.hypot(dx,dy)
    margin=np.minimum.reduce((x,w-1-x,y,h-1-y))
    opacity=smooth((margin-3+(broad-.5)*19)/48)
    z*=smooth(margin/57)
    shadow=illumination(z,object_heights(scene,z))
    views={}
    front={}
    for mode,light in [("lit",shadow),("unlit",np.full_like(z,.96)),("flat",shadow)]:
        view=painted(scene,x,y,z,light,opacity,slope,fine,streak,rock,mode=="flat")
        canvas=image(view)
        if mode!="flat" and scene.name=="canyon":
            cave_in_wall(canvas,view,scene)
        tread(canvas,scene,view,mode=="lit")
        file=f"{scene.name}-{mode}.png"
        canvas.save(ROOT/file)
        views[mode]=file[:-4]
        if mode!="flat":
            part=foreground(scene,canvas,view,mode)
            if part:
                front[mode]=part
    return {"origin":scene.origin,"size":scene.size,"views":views,"front":front}


if __name__=="__main__":
    ROOT.mkdir(parents=True,exist_ok=True)
    scenes={scene.name:bake(scene) for scene in (mountain(),canyon())}
    Path("resources/relief_demos.edn").write_text(edn(scenes)+"\n",encoding="utf-8")

