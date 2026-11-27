"""Build the V6.2 mountainside from the unchanged V6.1 gameplay geometry.

All painting works in world-aligned half-resolution pixels. Pillow exports
nearest-neighbour atlas pieces; NumPy evaluates the relief and lighting fields.
"""
import math
import random
from pathlib import Path

import numpy as np
from PIL import Image, ImageChops, ImageDraw, ImageFilter

from generate_massive_mountain import CAMPS, ORIGIN, SIZE, geometry, edn

ROOT = Path("resources/images/visual_v6_2")
PREVIEW = Path("docs/previews/world_v6_2")
W, H = [v * 32 for v in SIZE]
CW = 256
STONE = [(30,35,38),(46,48,48),(65,65,61),(83,80,71),(108,101,85),(136,125,105),(163,150,123)]
FROST = [(38,54,66),(56,75,87),(78,103,116),(110,136,147),(146,171,177),(184,201,201),(222,229,214)]
WOOD = [(42,33,29),(68,46,34),(92,63,42),(124,87,55),(166,122,75)]
Y, X = np.mgrid[0:H,0:W].astype(np.float32)
POSX, POSY = X / 32 + ORIGIN[0], Y / 32 + ORIGIN[1]
RNG = np.random.default_rng(602121)
R = random.Random(602122)


def pixel(cell):
    return tuple((c-o)*32+16 for c,o in zip(cell,ORIGIN))


def smoothed(points, iterations=3):
    result=points
    for _ in range(iterations):
        result=[result[0], *[
            p for a,b in zip(result,result[1:])
            for p in ((.76*a[0]+.24*b[0],.76*a[1]+.24*b[1]),
                      (.24*a[0]+.76*b[0],.24*a[1]+.76*b[1]))
        ],result[-1]]
    return result


def noise(rows, columns):
    values=RNG.random((rows,columns),dtype=np.float32)
    return np.asarray(Image.fromarray(values).resize((W,H),Image.Resampling.BICUBIC))


def geometry_mask(cells):
    img=Image.new("L",(W,H))
    draw=ImageDraw.Draw(img)
    for p in cells:
        x,y=pixel(p)
        draw.rectangle((x-16,y-16,x+15,y+15),fill=255)
    return img


def distance_to(points):
    # The cap equals the search radius. A larger sentinel creates a discontinuity
    # at the edge of each rectangular update, even outside the tread itself.
    dist=np.full((H,W),89.,dtype=np.float32)
    for u,v in points[::3]:
        x,y=int(u),int(v)
        a,b=max(0,x-89),min(W,x+90)
        c,d=max(0,y-89),min(H,y+90)
        local=dist[c:d,a:b]
        np.minimum(local,np.hypot(X[c:d,a:b]-u,Y[c:d,a:b]-v),out=local)
    return dist


def shade(palette, luminosity):
    index=np.clip(luminosity,0,len(palette)-1.001)
    lower=index.astype(np.int16)
    fraction=(index-lower)[...,None]
    colors=np.asarray(palette,dtype=np.float32)
    return colors[lower]*(1-fraction)+colors[lower+1]*fraction


def mix(a,b,t):
    return a*(1-t[...,None])+b*t[...,None]


def rgba(rgb,alpha):
    return Image.fromarray(np.dstack((np.clip(rgb,0,255),np.clip(alpha,0,255))).astype(np.uint8))


def carve(points, rocks, pads):
    macro=noise(12,9)
    medium=noise(47,39)
    strata=noise(88,22)
    gravel=noise(190,146)
    shape=(1-((POSX-18.4)/17.3)**2-((POSY-74.5)/21.4)**2
           + .11*np.sin(POSX*.38+POSY*.38)+.06*np.cos(POSY*.73-POSX*.31))
    alpha=255*np.minimum(np.clip((shape+.16)*4,0,1),np.clip(
        np.minimum.reduce([X/29,(W-1-X)/32,Y/43,(H-1-Y)/54]),0,1))
    dist=distance_to(points)
    ridge_x=680+115*np.sin(Y*.0039)+43*np.cos(Y*.008+macro)
    spine=np.exp(-((X-ridge_x)/(256+53*macro))**2)
    growth=np.clip((Y-24)/1100,0,1)
    # The elevation field supplies lighting, the snowline and transverse bedding.
    rim=np.clip(shape*1.72,0,1)
    flank=rim*(34+177*spine)*(0.27+0.75*growth)+29*macro
    saddle=(1-np.exp(-dist/32))*(flank+21*medium)
    exposed=np.asarray(rocks,dtype=np.float32)/255
    fault_phase=saddle*.178+Y*.062+X*.026+strata*3.1
    bedding=fault_phase-np.floor(fault_phase)
    light_y,light_x=np.gradient(saddle)
    level=3.42+(.69*light_x+.83*light_y)+1.36*(medium-.5)+.63*(gravel-.5)
    seams=np.where(bedding<.13,-.36,np.where(bedding>.83,.23,0))
    level += seams*exposed*np.clip((strata-.26)*2.5,0,1)
    level+=np.clip((dist-9)/19,0,1)*(-.18+.56*macro)
    snow=np.clip((growth-.61)*4.8+spine*.16+(macro-.46)*.52-abs(light_y)*.085,0,1)
    snow*=np.clip((Y-612)/148,0,1)
    moss=np.clip((.49-growth)*3.1+macro*.22-abs(light_x)*.06,0,.8)*(1-.75*spine)
    rgb=mix(shade(STONE,level),shade(FROST,level+.75),snow)
    rgb=mix(rgb,shade([(45,57,39),(64,74,48),(77,86,52),(104,108,67),
                        (126,126,77),(140,137,89),(155,148,103)],level),moss)
    rgb+=RNG.integers(-5,6,(H,W,1))
    terrain=rgba(rgb,alpha)
    trail=trail_surface(points,dist,snow,medium,gravel,pads)
    return terrain,trail,snow,macro


def trail_surface(points,dist,snow,medium,gravel,pads):
    # The tread is a material change in the shelf, with gaps in the scree edge.
    width=7.6+2.55*np.sin(Y*.021+X*.011)+1.6*np.cos(X*.034-Y*.024)+2.0*(medium-.5)
    edge=width+gravel*3.4-dist
    pad=np.asarray(pads.filter(ImageFilter.GaussianBlur(6)),dtype=np.float32)/255
    shelves=np.zeros((H,W),dtype=np.float32)
    for cell in (*CAMPS,geometry()[0][-1]):
        u,v=pixel(cell)
        shelves=np.maximum(shelves,np.clip(1.3-np.hypot((X-u)/80,(Y-v)/42)+(medium-.5)*.32,0,1))
    openness=np.maximum(np.clip(edge/4.2,0,1)*pad,shelves*pad*.92)
    grit=(medium-.5)*1.16+(gravel-.5)*.65
    dry=shade([(69,64,53),(87,78,61),(103,90,70),(120,106,84),(142,127,99)],2.65+grit)
    frozen=shade([(69,89,105),(95,119,135),(123,149,160),(151,174,179),
                  (188,204,200)],2.34+grit)
    img=rgba(mix(dry,frozen,snow),openness*247)
    d=ImageDraw.Draw(img)
    for i in range(2800):
        x,y=R.randrange(7,W-8),R.randrange(7,H-7)
        if openness[y,x]>.57:
            s=R.randrange(1,4)
            shade_color=tuple(int(c) for c in (frozen if snow[y,x]>.65 else dry)[y,x]*R.uniform(.69,.99))
            d.polygon([(x-s,y),(x,y-1),(x+s,y),(x+1,y+1)],fill=(*shade_color,218))
    for i,(u,v) in enumerate(points[::10]):
        x,y=int(u),int(v)
        if snow[y,x]>.7 and openness[y,x]>.52:
            offset=-2 if i%2 else 2
            color=tuple(round(c*.84) for c in frozen[y,x])
            d.line([(x+offset-1,y-1),(x+offset+1,y),(x+offset+2,y+1)],
                   fill=(*color,189),width=1)
    for u,v in [pixel(p) for p in [(18,61),(8,66),(17,72),(11,86)]]:
        for y in range(v-13,v+15,5):
            extent=R.randrange(7,13)
            p=FROST if y>960 else STONE
            d.line((u-extent,y,u+extent,y+R.randrange(-1,2)),fill=(*p[1],230),width=2)
            d.line((u-extent+1,y-1,u+extent-1,y),fill=(*p[5],227))
    for px,py in ((15,68),(9,79)):
        # Only the central tread crosses these short planked cuts.
        u,v=pixel((px,py))
        for x in range(u-17,u+18,4):
            t=R.randrange(-2,3)
            d.rectangle((x,v-8+t,x+3,v+9+t),fill=WOOD[R.randrange(2,5)])
            d.line((x,v-7+t,x,v+8+t),fill=WOOD[1])
        d.line((u-17,v-9,u+16,v-9),fill=WOOD[4],width=2)
    return img


def wall(points, depth, icy):
    """Extrude a continuous rim down the screen with visible strata and faults."""
    p=FROST if icy else STONE
    left,top=min(x for x,_ in points)-5,min(y for _,y in points)-7
    right,bottom=max(x for x,_ in points)+6,max(y for _,y in points)+int(depth*1.4)+12
    image=Image.new("RGBA",(right-left,bottom-top))
    dr=ImageDraw.Draw(image)
    upper=[(x-left,y-top) for x,y in points]
    lower=[(x+int(9*math.sin(x/31)),y+int(depth*(.95+.24*math.sin(x/71)+.13*math.sin(x/18))))
           for x,y in upper]
    profile=upper+lower[::-1]
    dr.polygon(profile,fill=p[1])
    for i in range(len(upper)-1):
        x,y=upper[i]
        rx,ry=upper[i+1]
        u,b=lower[i]
        ux,by=lower[i+1]
        q=1.0+R.uniform(-.55,.73)
        if i%4==0:
            q+=.63
        f=max(1,min(5,int(2+q*(math.sin(i*.84)+.2))))
        dr.polygon([(x,y),(rx+4,ry),(ux,by),(u,b)],fill=p[f])
        kink=(x+R.randrange(0,12), (y+b)//2)
        dr.polygon([(x,y),(rx,ry),(kink[0],kink[1]),(u,b)],fill=p[min(6,f+1)])
        for line in range(y+8,b-3,R.randrange(6,13)):
            shift=(line-y)/max(1,b-y)
            bx=x+round((u-x)*shift)
            dr.line([(bx,line),(bx+(rx-x)//3,line+1),(bx+rx-x,line+R.randrange(1,6))],
                    fill=p[R.choice((0,1,2,4))])
        dr.line([(x,y),(x+6,y+8),(kink[0],kink[1]),(u,b-2)],fill=p[max(0,f-2)])
    mask=Image.new("L",image.size)
    ImageDraw.Draw(mask).polygon(profile,fill=255)
    dr.line(upper,fill=p[6 if icy else 5],width=2)
    image.putalpha(ImageChops.multiply(image.getchannel("A"),mask))
    return image,(left,top)


def shelf_edges(terrain,rocks,pads,trail):
    shadow=Image.new("RGBA",(W,H))
    faces=Image.new("RGBA",(W,H))
    shade_draw=ImageDraw.Draw(shadow)
    curtains=[
        (130,175,808,31,0.42),(224,145,965,45,1.7),(321,106,1002,56,2.8),
        (434,95,1018,60,3.7),(551,75,1018,63,4.5),(676,82,998,68,5.4),
        (812,125,975,72,6.1),(930,156,944,79,7.0),(1083,207,881,94,8.3),
    ]
    for cy,a,b,depth,phase in curtains:
        xs=[a]
        while xs[-1]<b-9:
            xs.append(min(b,xs[-1]+R.randint(8,23)))
        points=[]
        for x in xs:
            bend=21*math.sin(x/82+phase)+14*math.sin(x/34+phase*.58)
            y=cy+int(bend+abs((x-(a+b)*.52)/(b-a))*61+R.randrange(-5,6))
            points.append((x,y))
        face,offset=wall(points,depth,cy>=675)
        faces.alpha_composite(face,offset)
        shade_draw.line([(x,y+int(depth*1.04)) for x,y in points],
                        fill=(25,30,32,107),width=max(16,depth//3))
    limit=rocks.filter(ImageFilter.GaussianBlur(2))
    # The continuous escarpment can touch, but cannot spill across, the tread.
    faces.putalpha(ImageChops.multiply(faces.getchannel("A"),limit))
    shadow.putalpha(ImageChops.multiply(shadow.getchannel("A").filter(ImageFilter.GaussianBlur(9)),
                                       terrain.getchannel("A")))
    terrain.alpha_composite(shadow)
    terrain.alpha_composite(faces)
    # Build paired front walls along the sheltered edges of the platforms.
    for i,cell in enumerate((*CAMPS,trail[-1])):
        u,v=pixel(cell)
        if i == 4:
            points=[(u+dx,v+45-int(23*(dx/101)**2)+int(6*math.sin(dx/11)))
                    for dx in range(-101,103,8)]
        else:
            points=[(u+dx,v-32+int(7*math.sin(dx/18+i)+.034*dx))
                    for dx in range(-79,81,10)]
        face,offset=wall(points,54 if i == 4 else 19+i*4,i>=3)
        cut=Image.new("RGBA",(W,H))
        cut.alpha_composite(face,offset)
        mask=ImageChops.invert(pads.filter(ImageFilter.GaussianBlur(1)))
        cut.putalpha(ImageChops.multiply(cut.getchannel("A"),mask))
        terrain.alpha_composite(cut)


def scatter(terrain,rocks,snow,macro):
    d=ImageDraw.Draw(terrain)
    mask=rocks.load()
    source=terrain.getpixel
    for y in range(19,H-10,3):
        for x in range(12,W-16,3):
            if not mask[x,y] or R.random()>.11:
                continue
            r,g,b,a=source((x,y))
            if a<180:
                continue
            span=R.randint(2,12)
            shade=R.choice((-29,-19,-13,8,16))
            color=tuple(max(0,min(255,c+shade)) for c in (r,g,b))+(a,)
            d.line([(x,y),(x+span//2,y+1),(x+span,y+R.choice((1,2,4)))],fill=color)
    for _ in range(1250):
        x,y=R.randrange(8,W-10),R.randrange(20,H-10)
        if not mask[x,y] or source((x,y))[3]<220:
            continue
        rock=FROST if snow[y,x]>.62 else STONE
        s=R.randrange(2,9)
        if y<260 and macro[y,x]>.57:
            for dx in (-3,0,2):
                d.line((x,y,x+dx,y-R.randrange(2,7)),fill=(98,111,60))
        elif R.random()<.52:
            d.polygon([(x-s,y+3),(x-1,y-3),(x+s,y+1),(x+1,y+4)],fill=rock[R.randrange(1,4)])
            d.line((x-s,y+2,x-1,y-3,x+2,y-1),fill=rock[4])



def foothill_brush(terrain, rocks):
    draw=ImageDraw.Draw(terrain)
    mask=rocks.load()
    for cx,cy in [(184,129),(282,86),(427,143),(633,79),(789,166),
                  (245,243),(855,270),(113,293)]:
        for _ in range(29):
            x,y=cx+R.randrange(-52,53),cy+R.randrange(-34,35)
            if not mask[x,y] or terrain.getpixel((x,y))[3]<245:
                continue
            w,h=R.randrange(4,9),R.randrange(6,14)
            draw.ellipse((x-7,y-2,x+10,y+4),fill=(56,56,38))
            draw.polygon([(x,y-h),(x-w,y-2),(x+w,y),(x+4,y-4)],
                         fill=(49,69,45))
            draw.polygon([(x,y-h),(x-w+2,y-2),(x+1,y-3)],fill=(76,95,58))
            draw.line((x,y-2,x,y+2),fill=(85,69,46))


def sheet(x,y,kind,asset,foot,image):
    name=f"{asset}.png"
    doubled=image.resize((image.width*2,image.height*2),Image.Resampling.NEAREST)
    doubled.save(ROOT/name)
    return {"x":x*2,"y":y*2,"width":doubled.width,"height":doubled.height,
            "foot-y":foot*2,"kind":kind,"asset":asset}


def foreground():
    result=[]
    for i,(x,y) in enumerate([(9,63),(12,69),(16,71),(5,76),(11,80),(18,83),(13,89)]):
        u,v=pixel((x,y))
        points=[(u+q,v-16-int(9*math.sin((q+24)/46*math.pi))+R.randrange(-3,5))
                for q in range(-25,28,5)]
        out,(a,b)=wall(points,20,i>=4)
        result.append(sheet(a,b,"mountain-face",f"front{i}",v+13,out))
    return result


def supplies(draw, x, y, barrel):
    if barrel:
        draw.rectangle((x,y-13,x+10,y),fill=WOOD[2])
        draw.ellipse((x,y-16,x+10,y-11),fill=WOOD[4],outline=WOOD[0])
        for v in (-10,-3):
            draw.line((x,y+v,x+10,y+v),fill=(49,47,42),width=2)
    else:
        draw.rectangle((x,y-12,x+12,y),fill=WOOD[2],outline=WOOD[0])
        draw.polygon([(x,y-12),(x+3,y-16),(x+14,y-16),(x+12,y-12)],fill=WOOD[4])
        draw.line((x+2,y-2,x+10,y-10),fill=WOOD[4])


def camp_objects(trail):
    result=[]
    styles=[((129,102,63),False),((108,113,75),True),((101,107,123),False),
            ((75,101,108),True),((123,112,92),True)]
    for i,(p,(canvas,lean_to)) in enumerate(zip([*CAMPS,trail[-1]],styles)):
        u,v=pixel(p)
        # Forward of its shelter rim, but behind the walking grade.
        image=Image.new("RGBA",(94,58))
        d=ImageDraw.Draw(image)
        d.ellipse((6,41,90,54),fill=(34,31,27,91))
        peak=32 if lean_to else 25
        for x in (17,52):
            d.line((x,peak,x,47),fill=WOOD[1],width=3)
        if lean_to:
            d.polygon([(12,31),(27,15),(70,20),(62,35)],fill=canvas,outline=WOOD[0])
            d.polygon([(16,31),(30,24),(37,35),(12,39)],fill=WOOD[2])
        else:
            d.polygon([(12,46),(30,15),(65,20),(74,43),(49,47)],fill=canvas,outline=WOOD[0])
            d.polygon([(12,46),(30,15),(41,45)],fill=tuple(int(c*.68) for c in canvas),outline=WOOD[0])
            d.polygon([(22,46),(30,29),(35,45)],fill=(38,37,35))
        for x in range(18,66,4):
            d.line((x,34+(x%5),x+3,34+(x%5)),fill=tuple(max(0,c-19) for c in canvas))
        if i>=2:
            d.line([(14,31),(27,17),(41,19),(51,21)],fill=(204,215,210),width=2)
        supplies(d,73,47,i%2==0)
        if i in (1,3):
            supplies(d,4,51,False)
        if i in (0,2,3,4):
            palette=FROST if i>=3 else STONE
            for px in range(8,82,7):
                py=53+R.randrange(-2,2)
                d.polygon([(px,py),(px+2,py-4),(px+7,py-2),(px+5,py+2)],
                          fill=palette[R.randrange(2,5)])
        for x in range(10,64,7):
            d.line((x,51,x+3,49),fill=WOOD[3],width=2)
        a,b=u-(94 if i==4 else 63),v-79
        result.append(sheet(a,b,"mountain-camp",f"camp{i}",b+54,image))
    return result


def write_chunks(image,layer):
    doubled=image.resize((W*2,H*2),Image.Resampling.NEAREST)
    chunks=[]
    for y in range(0,doubled.height,CW):
        for x in range(0,doubled.width,CW):
            piece=doubled.crop((x,y,min(x+CW,doubled.width),min(y+CW,doubled.height)))
            if piece.getbbox():
                piece.save(ROOT/f"{layer}_{x}_{y}.png")
                chunks.append([x,y,*piece.size])
    return chunks


def main():
    ROOT.mkdir(parents=True,exist_ok=True)
    PREVIEW.mkdir(parents=True,exist_ok=True)
    trail,pads,blocks,_=geometry()
    route=smoothed([pixel(c) for c in trail])
    cut=geometry_mask(blocks)
    safe=geometry_mask(pads)
    base,tread,snow,macro=carve(route,cut,safe)
    shelf_edges(base,cut,safe,trail)
    scatter(base,cut,snow,macro)
    foothill_brush(base,cut)
    fronts=foreground()+camp_objects(trail)
    data={"base":write_chunks(base,"base"),"tread":write_chunks(tread,"tread"),
          "front":fronts}
    (Path("resources")/"massive_mountain_art_v6_2.edn").write_text(edn(data)+"\n",encoding="utf-8")
    picture=Image.new("RGBA",(W*2,H*2),(104,119,75,255))
    for im in (base,tread):
        picture.alpha_composite(im.resize(picture.size,Image.Resampling.NEAREST))
    for part in sorted(fronts,key=lambda el:el["foot-y"]):
        with Image.open(ROOT/f"{part['asset']}.png") as art:
            picture.alpha_composite(art,(part["x"],part["y"]))
    picture.convert("RGB").save(PREVIEW/"massif.png")
    print({key:len(value) for key,value in data.items()})


if __name__=="__main__":
    main()
