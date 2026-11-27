"""Full-region terrain demonstrations for V6.4.2. The source world is read-only."""
from dataclasses import replace
from pathlib import Path
import csv
import random
import sys

import numpy as np
from PIL import Image, ImageDraw, ImageFilter

from generate_massive_mountain import ORIGIN as MOUNTAIN_ORIGIN, SIZE as MOUNTAIN_SIZE, edn, geometry as mountain_plan
from generate_desert_canyon_layout import ORIGIN as CANYON_ORIGIN, SIZE as CANYON_SIZE, BRIDGES, geometry as canyon_plan
from generate_relief_demos import canyon, foreground, grain, mask, mountain, pixel, ridge, GRID
from relief_raster import PROJECTION, illumination, project, smooth

ROOT = Path("resources/images/relief_v6_4_2")
ART_ROOT = Path("resources/images/relief_v6_4_3")
COLOR = {
    "mountain": {
        "floor": (112,123,108), "base": (128,129,119), "face": (112,116,116),
        "snow": (215,224,218), "ice": (131,158,171),
    },
    "canyon": {
        "floor": (181,129,82), "base": (177,110,68), "face": (158,92,58),
        "bottom": (90,64,67), "talus": (137,95,73),
    },
}


def full_scenes():
    return (replace(mountain(),origin=(MOUNTAIN_ORIGIN[0],MOUNTAIN_ORIGIN[1]-4),
                    size=(MOUNTAIN_SIZE[0],MOUNTAIN_SIZE[1]+4)),
            replace(canyon(),origin=(CANYON_ORIGIN[0],CANYON_ORIGIN[1]-4),
                    size=(CANYON_SIZE[0],CANYON_SIZE[1]+4)))


def field(scene, x, y, strata, faults):
    blocked=mask(scene,scene.blocks)
    block=np.asarray(blocked,dtype=np.float32)/255
    feather=np.asarray(blocked.filter(ImageFilter.GaussianBlur(7)),dtype=np.float32)/255
    ragged=.24*(strata-.5)+.12*(faults-.5)
    shoulder=smooth((feather+ragged-.54)/.39)*block
    if scene.name=="mountain":
        # Forks of the same ridge connect the upper peak to stony foothills.
        bulk=sum(h*ridge(x,y,cx,cy,rx,ry,lean) for cx,cy,rx,ry,lean,h in [
            (18,63,10,8,.49,65),(11.9,71,8,12,-.4,81),
            (23.5,76,9,12,-.58,124),(13.2,80,8,12,.35,83),
            (20.1,85.5,8.4,9,-.38,162),(24.8,87.3,7.4,6.5,.65,52)])
        slab=bulk+14*(strata-.5)+8*(faults-.45)
        terraces=(np.floor(slab/24)+smooth((slab/24%1-.05)/.63))*24
        height=np.maximum(0,(.81*slab+.19*terraces)*shoulder)
        # Keep the central summit deck behind the lip of the near cliff.
        behind=(y>88.7)*smooth((x-13.8)/2.5)*smooth((20.6-x)/2.5)
        limit=np.maximum(8,(y-88.65)*GRID/PROJECTION)
        height+=(np.minimum(height,limit)-height)*behind
    else:
        bulk=sum(h*ridge(x,y,cx,cy,rx,ry,lean) for cx,cy,rx,ry,lean,h in [
            (82.9,65,9,6,.85,63),(81.9,73.6,6.0,7,.71,44),
            (91.7,71.5,6.9,8,-.71,74),(97.6,78.8,3.7,10,.06,53),
            (85,84.5,6.4,7,.7,42),(87.9,90.1,7,10,-.58,53),
            (95.4,91,5.1,9,.37,61)])
        feasible=mask(scene,scene.pits)
        pit=np.asarray(feasible,dtype=np.float32)/255
        bank=np.asarray(feasible.filter(ImageFilter.GaussianBlur(5)),dtype=np.float32)/255
        bank=smooth((bank+.15*(strata-.5)+.14*(faults-.5)-.49)/.48)*pit
        north=87.23+1.08*np.sin((y-66)*.53)+.21*np.sin(y*1.1)
        east=81+.31*np.sin((x-95)*1.13)
        axis=np.where(y<75,abs(x-north),abs(y-east))
        shelf=np.where(y<75,1.23+.23*np.sin((y-64)*.61),
                       1.30+.27*np.sin((x-95)*1.28))
        bank*=smooth((shelf+.22*(strata-.5)-axis)/.47)
        hollow=48+68*np.exp(-(axis/1.34)**2)+21*strata+8*(faults-.5)
        height=np.maximum(0,bulk+9*(faults-.5))*shoulder-hollow*bank
    return height.astype(np.float32)


def heaviness(kind):
    return {"tree":36,"frost-pine":40,"palm":39,"saguaro":26,"tent":25,
            "raider-tent":26,"barrel":8,"crate":9,"stone":10,"stone-alt":12}.get(kind,0)


def blockers(scene,z,objects):
    y,x=np.ogrid[:z.shape[0],:z.shape[1]]
    surface=z.copy()
    for p,kind in objects.items():
        tall=heaviness(kind)
        if not tall:
            continue
        u,v=pixel(scene,p)
        if not (-15<u<z.shape[1]+15 and -15<v<z.shape[0]+15):
            continue
        a,b=max(0,u-17),min(z.shape[1],u+18)
        c,d=max(0,v-12),min(z.shape[0],v+13)
        mound=np.maximum(0,1-((x[:,a:b]-u)/14)**2-((y[c:d,:]-v)/9)**2)*tall
        surface[c:d,a:b]=np.maximum(surface[c:d,a:b],z[c:d,a:b]+mound)
    if scene.name=="canyon":
        u,v=pixel(scene,(87,68))
        deck=(abs(x-u)<51)&(abs(y-v-4)<10)
        s,t=pixel(scene,(95,81))
        deck|=(abs(x-s)<10)&(abs(y-t-4)<50)
        surface=np.where(deck,np.maximum(surface,2),surface)
    return surface


def pigment(scene, x, y, z, face, light, strata, faults, grit, salt):
    colors=COLOR[scene.name]
    rock=smooth((abs(z)-4)/29)
    wall=smooth((face-.2)/1.12)
    base=np.array(colors["base"])
    vertical=np.array(colors["face"])
    floor=np.array(colors["floor"])
    if scene.name=="mountain":
        snow=smooth((z+2.55*(y-68)-62+22*(strata-.5))/42)
        snow*=1-.72*smooth((face-.59)/1.7)
        rock_color=base+snow[...,None]*(np.array(colors["snow"])-base)
        vertical=vertical+snow[...,None]*(np.array(colors["ice"])-vertical)
        meadow=smooth((71-y)/13)*(1-rock)
        floor=floor+meadow[...,None]*[-14,4,-14]+smooth((y-81)/9)[...,None]*[24,31,27]
        color=floor+(rock_color-floor)*rock[...,None]
    else:
        depth=smooth(-z/111)
        color=floor+(base-floor)*rock[...,None]
        color=color+(np.array(colors["bottom"])-color)*depth[...,None]*.82
        vertical=vertical+depth[...,None]*[ -30,-12,-1]
    color=color+(vertical-color)*(wall*.93)[...,None]
    # Discontinuous strata shear along irregular slabs. The openings break
    # across coarse facets instead of closing into a grid of short bricks.
    plane=np.floor((x+41*(faults-.5)+10*np.sin(z*.029+strata*3.8))/33)
    slump=.27*np.sin(plane*.91+strata*5.1)+.21*np.sin(x*.068)
    bed=(z*.069+x*.013+strata*.85+slump+.16*np.sin(z*.19))%1
    shard=np.where(bed<.08,-23,np.where(bed>.84,20,0))*smooth((strata-.26)/.32)
    seam=(x*.041+z*.061+1.7*faults-.38*np.cos(z*.09))%1
    fracture=np.where((seam<.075)&(grit>.47)&(faults>.50)&(bed<.67),-31,0)
    chips=np.where(salt>.86,14,np.where(salt<.11,-18,0))
    face_detail=(shard+fracture+(np.mod(plane*13,9)-4)*5)*wall
    luma=np.round(((grit-.5)*24+chips+face_detail+(strata-.53)*8)/4)*4
    light=light+(1-light)*wall*.16
    return np.uint8(np.clip((color+luma[...,None])*light[...,None],0,255))


def opening(canvas, view, scene):
    # Locate space for an arch strictly within the positive visible rock face.
    center_x,center_y=pixel(scene,(82.2,65.6))
    face=(view.slope>.76)&(view.visible_height>5)
    best,x,y=max((float(np.mean(face[v-18:v+20,u-18:u+19])),u,v)
                 for u in range(center_x-48,center_x+49,4)
                 for v in range(max(20,center_y-51),center_y+41,3))
    if best<.36:
        return
    portal=Image.new("RGBA",canvas.size)
    d=ImageDraw.Draw(portal)
    rim=[(x-25,y+21),(x-23,y-6),(x-12,y-19),(x+5,y-23),
         (x+18,y-16),(x+25,y+2),(x+26,y+21)]
    d.polygon(rim+[(x+11,y+25),(x-16,y+25)],fill=(122,77,58))
    void=[(x-18,y+20),(x-16,y-1),(x-7,y-12),(x+4,y-15),(x+15,y-6),(x+19,y+20)]
    d.polygon(void,fill=(68,46,46))
    d.polygon([(x-9,y+17),(x-7,y+2),(x+4,y-6),(x+12,y+4),(x+14,y+17)],fill=(43,35,40))
    d.line(rim[:5],fill=(189,124,79),width=3)
    for a,b in zip(rim[1:6],void[:5]):
        d.line((a,b),fill=(104,66,55),width=2)
    alpha=np.asarray(portal.getchannel("A"),dtype=np.float32)
    portal.putalpha(Image.fromarray(np.uint8(alpha*smooth((view.visible_height-2)/9))))
    canvas.alpha_composite(portal)


def surface_details(canvas, view, scene, r):
    d=ImageDraw.Draw(canvas)
    for _ in range(2500):
        x,y=r.randrange(6,canvas.width-7),r.randrange(6,canvas.height-7)
        z=view.visible_height[y,x]
        if view.opacity[y,x]<.86 or (abs(z)<6 and r.random()<.72):
            continue
        r0,g,b,a=canvas.getpixel((x,y))
        size=r.randrange(2,9)
        hue=r.choice((-26,-17,11,23))
        color=tuple(max(0,min(255,c+hue)) for c in (r0,g,b))
        d.polygon([(x-size,y),(x-2,y-max(1,size//3)),(x+size,y),(x+1,y+max(1,size//4))],
                  fill=(*color,a))
    if scene.name=="mountain":
        for point in [(12,86),(11,85),(16,82),(19,60)]:
            x,y=pixel(scene,point)
            for i in range(5):
                v=y-9+i*5
                d.line((x-11,v,x+12,v+1),fill=(75,86,91),width=2)
                d.line((x-10,v-2,x+11,v-1),fill=(190,192,175))
    else:
        for ox,oy in [(82,73),(92,71),(98,85)]:
            u,v=pixel(scene,(ox,oy))
            # Draw debris on the plane that belongs to this blocked rim.
            yy=np.flatnonzero((abs(view.source_y[:,u]-v)<8)&(view.visible_height[:,u]>4))
            if not len(yy):
                continue
            y=int(yy[len(yy)//2])
            d.line([(u-8,y+2),(u+10,y+8),(u+19,y+8)],fill=(89,63,53,138),width=3)
            d.line([(u,y),(u-2,y-12),(u+2,y-26)],fill=(82,54,41),width=3)
            d.line([(u-2,y-12),(u-12,y-18),(u-16,y-24)],fill=(110,73,47),width=2)
            d.line([(u-1,y-17),(u+7,y-22),(u+8,y-29)],fill=(120,79,51),width=2)


def grade_ribbon(canvas, view, scene, y, grit):
    """Reveal the existing walking grade only where the projected ground is visible."""
    planned=canyon_plan() if scene.name=="canyon" else None
    routes=[planned["trail"],*planned["branches"]] if planned else [mountain_plan()[0]]
    brush=Image.new("L",canvas.size)
    d=ImageDraw.Draw(brush)
    for route in routes:
        coords=[(a,b+5) for a,b in (pixel(scene,p) for p in route)]
        d.line(coords,fill=255,width=16,joint="curve")
        for a,b in coords:
            d.ellipse((a-8,b-8,a+8,b+8),fill=255)
    shoulder=np.asarray(brush.filter(ImageFilter.GaussianBlur(2)),dtype=np.float32)/255
    # Material is translucent at the edges and never cuts through a wall silhouette.
    gate=(abs(view.visible_height)<5)&(view.opacity>.3)
    amount=np.float32(smooth((shoulder+.18*(grit-.5)-.16)/.72))*gate*.48
    local_y=y/GRID+scene.origin[1]
    tint=np.array((164,130,87) if planned else (141,145,121),dtype=np.float32)
    if not planned:
        tint=tint+smooth((local_y-75)/15)[...,None]*[25,29,41]
    tone=np.round((grit-.48)*17/3)*3
    rgb=np.asarray(canvas,dtype=np.float32).copy()
    rgb[:,:,:3]+=(tint+tone[...,None]-rgb[:,:,:3])*amount[...,None]
    return Image.fromarray(np.uint8(np.clip(rgb,0,255)))


def bridges(scene, root=ROOT, polish=False):
    result=[]
    r=random.Random(642193)
    for idx,cells in enumerate((BRIDGES[:3],BRIDGES[3:])):
        vertical=idx==1
        length=128*len(cells)+23
        w=116
        image=Image.new("RGBA",(length,w))
        d=ImageDraw.Draw(image)
        # Far rope, structural beams, deck boards, then the near rope.
        d.polygon([(4,27),(length-1,29),(length-7,84),(9,91)],fill=(70,42,29))
        for x in (11,length-26):
            d.polygon([(x,18),(x+8,17),(x+13,96),(x+3,110),(x-2,104)],fill=(82,49,34))
            d.line((x+3,20,x+8,96),fill=(147,99,58),width=3)
        for y in (35,79):
            d.line((5,y,length-1,y+4),fill=(67,42,32),width=9)
            d.line((4,y-4,length-3,y),fill=(139,89,49),width=4)
        for i in range(0,length-9,12):
            k=r.randrange(-9,11)
            edge=r.randrange(-4,5)
            d.polygon([(i+1,33+edge),(i+11,33+edge),(i+10,77+edge),(i+2,82+edge)],
                      fill=(151+k,99+k,56+k),outline=(87,52,33),width=2)
            d.line([(i+4,38+edge),(i+5,55),(i+3,69+edge)],fill=(184+k,127+k,78+k),width=2)
            if i%60==0:
                d.line((i+4,60,i+7,72),fill=(92,58,39),width=2)
        for edge in (25,89):
            posts=list(range(9,length-8,64))+[length-9]
            coords=[]
            for i in range(length-5):
                coords.append((i,edge+round(4*np.sin(np.pi*(i%64)/64))))
            d.line(coords,fill=(74,47,34),width=4)
            d.line([(x,y-1) for x,y in coords],fill=(174,130,83),width=2)
            for x in posts:
                d.rectangle((x-3,edge-11,x+3,edge+7),fill=(99,64,40))
                d.line((x-2,edge-10,x-2,edge+4),fill=(163,114,66),width=2)
                d.line((x-4,edge-2,x+5,edge+1),fill=(210,161,99),width=2)
        if polish:
            for x in (18,length-22):
                for y in (33,80):
                    d.polygon([(x-9,y-8),(x+9,y-7),(x+13,y+7),(x+5,y+17),
                               (x-9,y+15),(x-14,y+5)],fill=(54,37,32,135))
                    d.polygon([(x-7,y-6),(x+7,y-6),(x+10,y+5),(x+4,y+10),
                               (x-7,y+8),(x-11,y+2)],fill=(117,83,59))
                    d.line([(x-10,y+1),(x-6,y-5),(x+7,y-5)],fill=(171,126,84),width=2)
                    d.rectangle((x-4,y-11,x+4,y+6),fill=(79,48,33))
                    d.line((x-3,y-10,x-3,y+4),fill=(153,102,58),width=2)
                    for offset in (-4,0,4):
                        d.line((x-5,y+offset,x+5,y+offset-1),fill=(187,147,96),width=2)
            for x in range(29,length-28,12):
                for y in (40,72):
                    d.point((x,y),fill=(61,48,42))
                    d.point((x-1,y-1),fill=(196,155,109))
                d.line([(x+1,46),(x+2,53),(x,60)],fill=(121,76,44))
        if vertical:
            image=image.transpose(Image.Transpose.ROTATE_90)
            image=image.resize((88,image.height),Image.Resampling.NEAREST)
        u,v=pixel(scene,cells[0])
        # Assets are twice world size. Keep the top deck on the old logical grade.
        u=u*4-(image.width//2 if vertical else 76)
        v=v*4-(77 if vertical else 20)
        name=("east-deck" if vertical else "north-deck")
        image.save(root/f"{name}.png")
        result.append({"asset":name,"x":u//2,"y":v//2,
                       "width":image.width//2,"height":image.height//2})
    return result


def chunk(canvas, name, root=ROOT):
    parts=[]
    for y in range(0,canvas.height,128):
        for x in range(0,canvas.width,128):
            piece=canvas.crop((x,y,min(x+128,canvas.width),min(y+128,canvas.height)))
            if not piece.getbbox(alpha_only=True):
                continue
            asset=f"{name}-{x}-{y}"
            prior=ROOT/f"{asset}.png"
            target=root/f"{asset}.png"
            if target.exists():
                with Image.open(target) as current:
                    same=current.tobytes()==piece.tobytes()
            else:
                same=False
            if not same and root != ROOT and prior.exists():
                with Image.open(prior) as previous:
                    if previous.tobytes()==piece.tobytes():
                        parts.append({"asset":asset,"x":2*x,"y":2*y,
                                      "width":2*piece.width,"height":2*piece.height})
                        continue
            if not same:
                piece.save(target,optimize=True)
            parts.append({"asset":asset,"x":2*x,"y":2*y,
                          "width":2*piece.width,"height":2*piece.height})
    return parts


def bake(scene, objects, art=None, study=None, shapes=None, study_folder=None, integrated=None):
    width,height=(n*GRID for n in scene.size)
    y,x=np.mgrid[:height,:width].astype(np.float32)
    world_x=x/GRID+scene.origin[0]-.5
    world_y=y/GRID+scene.origin[1]-.5
    rng=np.random.default_rng(642113 if scene.name=="mountain" else 642191)
    strata=grain(rng,width,height,35,49)
    faults=grain(rng,width,height,82,81)
    grit=grain(rng,width,height,465,510)
    salt=rng.random(x.shape,dtype=np.float32)
    z=field(scene,world_x,world_y,strata,faults)
    wet=[p for p,kind in objects.items() if kind=="water"]
    # Existing water owns the pixel at the existing shore, even when a nearby
    # cliff projects over it. The halo blends into V6.4's lake rendering.
    wetmask=mask(scene,wet).filter(ImageFilter.MaxFilter(13)).filter(ImageFilter.GaussianBlur(6))
    land=(1-np.asarray(wetmask,dtype=np.float32)/255)
    outer=np.minimum.reduce((x,y,width-1-x,height-1-y))
    alpha=smooth((outer-6+24*(strata-.5))/56)
    if art and scene.name=="canyon":
        # Mix the existing desert floor through the perimeter at a broad,
        # irregular edge while retaining the opacity of nearby rock and pits.
        apron=smooth((outer-10+67*(strata-.5)+30*(faults-.5))/135)
        alpha*=apron+(1-apron)*smooth(abs(z)/12)
    z*=smooth(outer/72)*land
    shape_art=integrated or shapes
    if shape_art:
        z=shape_art.shape(scene,x,y,z)
    if integrated:
        # Fade toward the actual underlying biome only at the outer margin.
        margin=smooth((outer-4+42*(strata-.5))/90)
        alpha*=margin+(1-margin)*smooth(abs(z)/14)
    # Keep the lower rim from swallowing the entire floor in a fixed camera.
    projected=np.where(z<0,z/(1+abs(z)/132),z)
    dy,dx=np.gradient(z)
    slope=np.hypot(dx,dy)
    lighting=illumination(z,blockers(scene,z,objects))
    if study:
        def geometry_only(row, cols, z0, face, frac):
            return 0
        view=project(z,geometry_only,alpha*land,slope,PROJECTION,projected_height=projected)
        study.write_study(scene,view,lighting,strata,grit,salt,ART_ROOT,
                          study_folder,shapes)
        return
    modes={}
    root=study_folder if integrated else (ART_ROOT if art else ROOT)
    for mode in (("lit",) if art else ("lit","unlit")):
        light=lighting if mode=="lit" else np.full_like(z,.98)
        def paint(row, cols, z0, face, frac):
            shade=np.round((light[row,cols]+(light[row+1,cols]-light[row,cols])*frac)*31)/31
            if integrated:
                return np.uint8(integrated.material(
                    scene,x[row,cols],y[row,cols],z0,face,shade,
                    strata[row,cols],grit[row,cols],salt[row,cols]))
            painter=art.pigment if art else pigment
            return painter(scene, x[row,cols], world_y[row,cols], z0, face,shade,
                           strata[row,cols],faults[row,cols],grit[row,cols],salt[row,cols])
        view=project(z,paint,alpha*land,slope,PROJECTION,projected_height=projected)
        output_alpha=np.uint8(smooth((land-.08)/.76)*view.opacity*255)
        canvas=Image.fromarray(np.dstack((view.rgb,output_alpha)))
        canvas=grade_ribbon(canvas,view,scene,y,grit)
        pattern=random.Random(642113 if scene.name=="mountain" else 642191)
        if integrated:
            safe_y=np.maximum(view.source_y,0)
            light=lighting[safe_y,np.arange(width)[None,:]]
            integrated.details(scene,view,canvas,light,land)
        elif art:
            art.details(canvas,view,scene,pattern)
        else:
            if scene.name=="canyon":
                opening(canvas,view,scene)
            surface_details(canvas,view,scene,pattern)
        modes[mode]=chunk(canvas,f"{scene.name}-{mode}",root)
        part=foreground(scene,canvas,view,mode,root=root)
        if part:
            modes[f"{mode}-front"]=part
    result={"origin":scene.origin,"size":scene.size,
            "tiles":{k:modes[k] for k in ("lit","unlit") if k in modes},
            "front":{k:modes[f"{k}-front"] for k in ("lit","unlit") if f"{k}-front" in modes}}
    if scene.name=="canyon":
        result["decks"]=bridges(scene,root,polish=bool(art)) if not integrated else None
    print(scene.name,"samples",width,height,"elevation",float(z.min()),float(z.max()),
          "tiles",len(modes["lit"]))
    return result


if __name__=="__main__":
    study_modes={"--materials","--shapes","--walls","--finish"}
    args=[arg for arg in sys.argv[1:]
          if arg not in study_modes|{"--art","--integrated","--mountain-only"}]
    art=None
    study=None
    shapes=None
    if (study_modes|{"--art"}).intersection(sys.argv):
        import relief_art as art
    if study_modes.intersection(sys.argv):
        import relief_material as study
    if "--finish" in sys.argv:
        import relief_finish as shapes
    elif "--walls" in sys.argv:
        import relief_walls as shapes
    elif "--shapes" in sys.argv:
        import relief_shapes as shapes
    csv_file=Path(args[0] if args else ".relief-v642-world.csv")
    with csv_file.open(encoding="utf-8") as file:
        objects={(int(x),int(y)):kind for x,y,kind in csv.reader(file)}
    if "--integrated" in sys.argv:
        import relief_art
        import relief_integrated
        folder=Path("resources/images/relief_v6_4_8")
        folder.mkdir(parents=True,exist_ok=True)
        if "--mountain-only" in sys.argv:
            bake(full_scenes()[0],objects,art=relief_art,study_folder=folder,
                 integrated=relief_integrated)
            sys.exit(0)
        result={scene.name:bake(scene,objects,art=relief_art,study_folder=folder,
                                integrated=relief_integrated) for scene in full_scenes()}
        result["canyon"].pop("decks")
        Path("resources/relief_integrated.edn").write_text(edn(result)+"\n",encoding="utf-8")
        relief_integrated.relief_finish.finish_props(folder,integrated=True)
        sys.exit(0)
    if study:
        version=7 if "--finish" in sys.argv else (6 if "--walls" in sys.argv else (5 if shapes else 4))
        folder=Path("resources/images",f"relief_v6_4_{version}")
        folder.mkdir(parents=True,exist_ok=True)
        for scene in full_scenes():
            bake(scene,objects,art,study,shapes,folder)
        if version==7:
            shapes.finish_props(folder)
        sys.exit(0)
    (ART_ROOT if art else ROOT).mkdir(parents=True,exist_ok=True)
    scenes={scene.name:bake(scene,objects,art) for scene in full_scenes()}
    manifest="relief_art.edn" if art else "relief_natural.edn"
    Path("resources",manifest).write_text(edn(scenes)+"\n",encoding="utf-8")
