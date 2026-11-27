"""Pillow sandstone, canyon floor and foreground atlas at twice game resolution."""
from pathlib import Path
import random

import numpy as np
from PIL import Image, ImageChops, ImageDraw, ImageFilter

from generate_desert_canyon_layout import ORIGIN, SIZE, geometry
from generate_massive_mountain import edn

ROOT = Path("resources/images/visual_v6_4")
PREVIEW = Path("docs/previews/world_v6_4")
W,H = (d*128 for d in SIZE)
R = random.Random(640177)
RNG = np.random.default_rng(640611)
RIDGE = np.array([(64,34,28),(86,42,32),(110,55,37),(134,68,43),(158,83,49),
                  (179,100,58),(197,121,72),(218,147,86),(232,172,112)])
SAND = np.array([(112,73,46),(137,92,57),(156,109,67),(176,125,75),(193,143,91),
                 (207,159,101),(222,178,115),(232,197,139),(241,212,157)])
PIT = np.array([(37,26,30),(48,29,32),(61,34,34),(72,38,35),(88,46,35),
                (105,54,41),(127,64,45),(146,78,51),(168,92,59)])
WOOD = [(49,30,24),(67,41,28),(93,58,35),(133,84,48),(162,109,64),(190,137,83)]


def point(x,y):
    return round((x-ORIGIN[0]+.5)*128), round((y-ORIGIN[1]+.5)*128)


def block_mask(cells):
    result=Image.new("L",(W,H))
    d=ImageDraw.Draw(result)
    for x,y in cells:
        u,v=point(x,y)
        d.rectangle((u-64,v-64,u+63,v+63),fill=255)
    return result


def variation(cols,rows):
    samples=RNG.random((rows,cols),dtype=np.float32)
    return Image.fromarray(samples).resize((W,H),Image.Resampling.BILINEAR)


def palette(colors,tone):
    return colors[np.clip(np.rint(tone),0,len(colors)-1).astype(np.uint8)]


def curve(route):
    return [point(x,y) for x,y in route]


def atlas(plan):
    macro=variation(18,26)
    grain=variation(735,798)
    detail=variation(112,153)
    walls=block_mask(plan["walls"])
    face=walls.filter(ImageFilter.GaussianBlur(37))
    ravine=block_mask([*plan["pits"],*plan["bridges"]]).filter(ImageFilter.GaussianBlur(35))
    road=Image.new("L",(W,H))
    brush=ImageDraw.Draw(road)
    for route in [plan["trail"],*plan["branches"]]:
        points=curve(route)
        brush.line([(x,y+23) for x,y in points],fill=255,width=83,joint="curve")
        for x,y in points:
            brush.ellipse((x-40,y-17,x+40,y+63),fill=255)
    road=road.filter(ImageFilter.GaussianBlur(9))
    land=Image.new("RGBA",(W,H))
    tread=Image.new("RGBA",(W,H))
    x=np.arange(W)[None,:]/128+ORIGIN[0]-.5
    for top in range(0,H,96):
        stop=min(H,top+96)
        box=(0,top,W,stop)
        y=np.arange(top,stop)[:,None]/128+ORIGIN[1]-.5
        n=np.asarray(macro.crop(box))
        grainage=np.asarray(grain.crop(box))
        flecks=np.asarray(detail.crop(box))
        mesa=np.asarray(face.crop(box))/255
        chasm=np.asarray(ravine.crop(box))/255
        north_axis=87.25+1.04*np.sin((y-66)*.53)+.17*np.sin(y*1.8)
        east_axis=81.1+.31*np.sin((x-94)*.82)
        depth_profile=np.maximum(np.exp(-((x-north_axis)/1.09)**2)*(y<73),
                                 np.exp(-((y-east_axis)/1.14)**2)*(x>93))
        chasm=np.clip(chasm*(.52+.65*depth_profile)+.07*(n-.49),0,1)
        trail=np.asarray(road.crop(box))/255
        oval=((x-86-(y-80)*.105)/14.4)**2+((y-81)/17.9)**2
        alpha=np.clip((1.13-oval+.19*(n-.5)+.15*(grainage-.5))*5.3,0,1)
        edge=np.clip(np.minimum(np.minimum((x-72.5)*2.0,(98.95-x)*2.4),
                                np.minimum((y-63.53)*1.5,(99.1-y)*2.4)),0,1)
        # Broad shadows occupy entire sandstone arms; short ripple crests run across the sandy fans.
        slope=(mesa-np.roll(mesa,4,axis=1))*18
        ripple=(x*.81+y*.74+2.5*n+2.4*np.sin(y*.57-x*.22))%1
        dune=3.9+2.2*(n-.5)+2.5*(flecks-.5)+.66*(grainage-.5)
        dune+=(ripple>.83)*.62*(1-mesa)
        for cx,cy,span,tilt in [(75.8,68.9,2.1,.27),(79.4,81.4,2.6,-.28),
                                (82,92,2.9,.23),(95.9,95.8,2.4,-.3)]:
            u=x-cx
            slip=y-cy-.33*u*u-tilt*u
            dune+=np.maximum(0,1-abs(u)/span)*np.exp(-(slip/.76)**2)*np.where(slip<0,1.05,-1.45)*(1-.8*trail)
        sand=palette(SAND,dune)
        seam=(y*3.1+x*.82+1.4*n+mesa*.74)%1
        rock=palette(RIDGE,3.9+2.5*(n-.5)+2.2*(flecks-.5)+1.1*(grainage-.5)
                     +np.clip(slope,-2.2,1.7) +(.43*(seam>.76)-.52*(seam<.18))*mesa)
        t=mesa[...,None]
        rgb=sand+(rock-sand)*t
        abyss=palette(PIT,.9+4.0*(1-chasm)+(flecks-.5)*2+.4*(seam>.7))
        rgb=rgb+(abyss-rgb)*chasm[...,None]
        oasis=np.minimum(((x-90)/5.6)**2+((y-79)/4.45)**2,
                         ((x-75)/6.4)**2+((y-89)/5.35)**2)
        portal_fade=np.clip((oasis-1)/.57,0,1)
        alpha=np.maximum(alpha*edge*portal_fade,chasm*.86)
        land.paste(Image.fromarray(np.uint8(np.dstack((rgb,np.rint(alpha*7)*255/7)))),(0,top))
        worn=palette(SAND,4.8+(flecks-.5)*1.7+(grainage-.5)*1.4)
        eroded=np.clip((trail-.28+.17*(grainage-.5))*1.8,0,1)*(.82+.16*n)
        eroded*=np.clip(1-chasm*1.4,0,1)*portal_fade
        tread.paste(Image.fromarray(np.uint8(np.dstack((worn,np.rint(eroded*6)*39)))),(0,top))
    return land,tread,walls


def cliff(crest,depth):
    xs=[point(*p) for p in crest]
    upper=[]
    for a,b in zip(xs,xs[1:]):
        count=max(2,round(np.hypot(b[0]-a[0],b[1]-a[1])/44))
        for i in range(count):
            t=i/count
            upper.append((round(a[0]+t*(b[0]-a[0])+R.randint(-12,12)),
                          round(a[1]+t*(b[1]-a[1])+R.randint(-15,15))))
    upper.append(xs[-1])
    lower=[(x+R.randint(12,46),
            y+round(depth*(.48+.53*min(1,min(i,len(upper)-1-i)/3)+R.uniform(-.17,.21))))
           for i,(x,y) in enumerate(upper)]
    left=min(x for x,y in upper+lower)-30
    top=min(y for x,y in upper+lower)-15
    bounds=(max(x for x,y in upper+lower)-left+33,max(y for x,y in upper+lower)-top+33)
    image=Image.new("RGBA",bounds)
    d=ImageDraw.Draw(image)
    upper=[(x-left,y-top) for x,y in upper]
    lower=[(x-left,y-top) for x,y in lower]
    shape=upper+lower[::-1]
    d.polygon(shape,fill=tuple(RIDGE[2]))
    for (a,b),(c,e),(f,g),(h,j) in zip(upper,upper[1:],lower,lower[1:]):
        shade=R.choice((3,4,4,5,6))
        d.polygon([(a,b),(c,e),(h,j),(f,g)],fill=tuple(RIDGE[shade]))
        m=(round((a+f)*.5)+R.randint(-8,12),round((b+g)*.5))
        d.polygon([(a,b),(c,e),m,(f,g)],fill=tuple(RIDGE[min(8,shade+1)]))
        d.line([(a,b),m,(f,g)],fill=tuple(RIDGE[shade-2]),width=R.randint(4,13))
        for k in range(10,min(g-b,j-e)-5,R.randrange(14,25)):
            p=a+(f-a)*k/max(1,g-b)
            q=c+(h-c)*k/max(1,j-e)
            tone=R.choice((shade-2,shade-1,shade+1,shade+2))
            d.line([(p,b+k),((p+q)/2,(b+e)/2+k+R.randint(-7,8)),(q,e+k)],
                   fill=tuple(RIDGE[min(8,tone)]),width=R.randint(2,8))
        d.line([(a,b),(c,e)],fill=tuple(RIDGE[min(8,shade+2)]),width=R.randint(4,11))
    m=Image.new("L",bounds)
    ImageDraw.Draw(m).polygon(shape,fill=255)
    image.putalpha(ImageChops.multiply(m,image.getchannel("A")))
    return image,(left,top),upper


def faces(land,walls):
    layer=Image.new("RGBA",(W,H))
    shade=Image.new("RGBA",(W,H))
    shadow=ImageDraw.Draw(shade)
    lines=[
        ([(76,65.6),(79,64.8),(82,65.4),(85,66.4),(87,67)],145),
        ([(79,72.8),(80.6,71.1),(82,71.6),(83.2,72.8),(85.3,73.2),(87,74.2)],191),
        ([(89.1,70.7),(90,69.8),(91.5,70.5),(93,72.1),(95.1,73)],184),
        ([(94.9,68.3),(96.1,68.9),(97.6,69.7),(98.4,72.1),(97.8,75.3),(97.5,78.9)],207),
        ([(80.6,84.2),(82.2,83.1),(85.0,83.6),(86.3,85.3),(88.1,86)],200),
        ([(84.6,89.2),(86.1,88),(88.6,88.5),(90.2,89.8),(91.6,90.8)],176),
        ([(96.9,85.1),(97.5,87.3),(96.1,88.9),(95.8,91.7),(97.1,93.4)],214),
        ([(80.7,97.7),(83.6,96.2),(86.1,96.6),(88.8,95.2),(90.9,96.6),(93.0,97.1)],225),
    ]
    for line,depth in lines:
        image,(x,y),top=cliff(line,depth)
        layer.alpha_composite(image,(x,y))
        contact=[(x+a+8,y+b+depth*.83) for a,b in top]
        for width,alpha in [(57,26),(32,60),(13,95)]:
            shadow.line(contact,fill=(60,35,34,alpha),width=width,joint="curve")
    limit=walls.filter(ImageFilter.GaussianBlur(5))
    layer.putalpha(ImageChops.multiply(layer.getchannel("A"),limit))
    shade.putalpha(ImageChops.multiply(shade.getchannel("A"),limit))
    land.alpha_composite(shade)
    land.alpha_composite(layer)
    draw=ImageDraw.Draw(land)
    for _ in range(7200):
        x,y=R.randrange(30,W-30),R.randrange(30,H-30)
        p=land.getpixel((x,y))
        if p[3]<220:
            continue
        size=R.randrange(3,27)
        delta=R.choice((-19,-13,-7,9,18))
        shade=tuple(max(0,min(255,c+delta)) for c in p[:3])
        draw.polygon([(x-size,y),(x-2,y-size//3),(x+size,y+2),(x+size//2,y+size//4)],
                     fill=(*shade,p[3]))


def sheet(image,x,y,foot,kind,name):
    if image.width%2 or image.height%2:
        image=image.crop((0,0,image.width-image.width%2,image.height-image.height%2))
    image.save(ROOT/f"{name}.png")
    return {"asset":name,"kind":kind,"x":x//2,"y":y//2,"foot-y":foot//2,
            "width":image.width//2,"height":image.height//2}


def make_bridge(length,vertical):
    im=Image.new("RGBA",(length,128))
    d=ImageDraw.Draw(im)
    d.polygon([(4,98),(length-14,99),(length-4,118),(14,117)],fill=(43,26,25,165))
    for i in range(0,length-8,12):
        offset=R.randrange(-5,6)
        d.polygon([(i,33+offset),(i+11,31+offset),(i+9,98+offset),(i+1,99+offset)],
                  fill=WOOD[R.choice((2,3,4))],outline=WOOD[1],width=2)
        d.line((i+3,41+offset,i+7,88+offset),fill=WOOD[5],width=2)
    for y in (29,101):
        d.line((3,y,length-9,y),fill=WOOD[3],width=6)
        for x in range(4,length-6,75):
            d.rectangle((x,y-11,x+8,y+5),fill=WOOD[1])
            d.line((x+2,y-10,x+2,y+4),fill=WOOD[5],width=2)
    return im.transpose(Image.Transpose.ROTATE_90) if vertical else im


def cave(style):
    im=Image.new("RGBA",(220,208))
    d=ImageDraw.Draw(im)
    arch=[(17,159),(24,99),(51,43),(85,24),(121,17),(161,33),(185,57),(201,111),(202,158)]
    d.ellipse((14,141,215,184),fill=(61,37,32,133))
    d.polygon(arch+[(175,178),(45,176)],fill=tuple(RIDGE[3]))
    d.polygon([(37,158),(44,109),(61,69),(93,53),(126,47),(153,66),
               (171,98),(182,160)],fill=(47,29,30))
    d.polygon([(53,153),(66,116),(100,103),(136,111),(169,153)],fill=(33,24,28))
    for index,(a,b) in enumerate(zip(arch,arch[1:])):
        x,y=a
        xx,yy=b
        d.polygon([(x,y),(xx,yy),(xx+8,yy+19),(x+16,y+22)],fill=tuple(RIDGE[5+index%3]))
        d.line([(x,y),(x+16,y+22)],fill=tuple(RIDGE[2]),width=4)
    for _ in range(38):
        x=R.randrange(19,205)
        y=R.randrange(148,179)
        if x<46 or x>175 or (not style and y>162):
            d.line((x,y,x+R.randrange(3,19),y+2),fill=tuple(RIDGE[R.randrange(2,7)]),width=3)
    return im


def ruins():
    im=Image.new("RGBA",(342,260))
    d=ImageDraw.Draw(im)
    d.polygon([(13,183),(262,168),(329,202),(104,237)],fill=(70,43,33,135))
    for x,y,rows in [(26,160,5),(87,163,3),(139,159,5),(235,170,4),(286,181,2)]:
        for row in range(rows):
            v=y-row*17
            u=x+R.randrange(-5,6)
            a=R.randrange(2,5)
            d.polygon([(u,v-14),(u+41,v-15),(u+46,v),(u,v+3)],fill=tuple(RIDGE[a+1]))
            d.polygon([(u,v-14),(u+12,v-23),(u+47,v-20),(u+41,v-15)],fill=tuple(RIDGE[a+3]))
            d.line((u+13,v-13,u+11,v+1),fill=tuple(RIDGE[a-1]),width=4)
    for _ in range(55):
        x,y=R.randrange(12,324),R.randrange(171,225)
        r=R.randrange(3,13)
        d.polygon([(x-r,y),(x-1,y-r//2),(x+r,y),(x+4,y+4)],fill=tuple(RIDGE[R.randrange(2,7)]))
    return im


def stone_arch():
    im=Image.new("RGBA",(330,248))
    d=ImageDraw.Draw(im)
    d.ellipse((28,187,319,239),fill=(51,28,28,109))
    crest=[(18,187),(33,114),(58,59),(100,35),(139,25),(195,38),
           (247,31),(284,60),(313,113),(315,207)]
    d.polygon(crest+[(258,219),(229,150),(225,102),(207,83),(160,97),
                     (121,91),(89,128),(83,183),(52,210)],fill=tuple(RIDGE[3]))
    for i,((x,y),(u,v)) in enumerate(zip(crest,crest[1:])):
        d.polygon([(x,y),(u,v),(u-16,v+21),(x+11,y+25)],fill=tuple(RIDGE[6+i%3]))
        d.line((x+11,y+25,u-16,v+21),fill=tuple(RIDGE[2]),width=4)
    for x,y in [(44,122),(49,145),(55,166),(272,101),(281,129),(290,155),(285,175)]:
        d.line([(x-17,y),(x-6,y-4),(x+20,y+2)],fill=tuple(RIDGE[5]),width=5)
    return im


def shelter(i):
    im=Image.new("RGBA",(264,192))
    d=ImageDraw.Draw(im)
    d.polygon([(15,141),(184,126),(248,158),(41,174)],fill=(71,43,33,127))
    for x in (52,145,177):
        d.line((x,67,x-5,148),fill=WOOD[1],width=7)
    canvas=[(168,115,71),(190,151,97),(128,117,86)][i]
    ridge=45+12*i
    d.polygon([(31,92),(78,ridge),(153,ridge+13),(184,89),(119,100)],fill=canvas,outline=WOOD[1],width=4)
    d.polygon([(31,92),(79,ridge),(74,102),(33,128)],fill=tuple(int(.72*c) for c in canvas))
    d.line((78,ridge,87,90,119,100),fill=WOOD[5],width=5)
    for x in range(94,156,12):
        d.line((x,ridge+9,x+20,90),fill=tuple(max(0,c-26) for c in canvas),width=2)
    d.polygon([(130,94),(148,84),(173,96),(161,99),(151,109)],fill=(0,0,0,0))
    for x,y in [(15,145),(185,144),(219,153)]:
        d.rectangle((x,y-24,x+24,y),fill=WOOD[3],outline=WOOD[0],width=3)
        d.line((x+3,y-4,x+21,y-22),fill=WOOD[5],width=3)
    return im


def foreground(plan):
    items=[]
    for x,y,variant in [(81.3,72.1,0),(97.0,77.1,1),(96.7,89.8,0)]:
        u,v=point(x,y)
        image=cave(variant)
        items.append(sheet(image,u-110,v-150,v+37,"canyon-face",f"portal{len(items)}"))
    for x,y in [(85.5,85.1),(90.7,96.1)]:
        u,v=point(x,y)
        items.append(sheet(ruins(),u-171,v-169,v+62,"canyon-face",f"ruins{len(items)}"))
    u,v=point(94.4,72.2)
    items.append(sheet(stone_arch(),u-162,v-188,v+44,"canyon-face","sandstone-arch"))
    for i,(x,y) in enumerate(plan["camps"]):
        u,v=point(x,y)
        items.append(sheet(shelter(i),u-(181 if i==1 else 244),v-156,v+13,"canyon-camp",f"camp{i}"))
    for name,cells in [("west-bridge",plan["bridges"][:3]),("east-bridge",plan["bridges"][3:])]:
        vertical=cells[0][0]==cells[-1][0]
        u,v=point(*cells[0])
        image=make_bridge(len(cells)*128+30,vertical)
        x=u-(64 if vertical else 79)
        y=v-(80 if vertical else 42)
        items.append(sheet(image,x,y,y+image.height,"canyon-bridge",name))
    return items


def export(image,tag):
    chunks=[]
    for y in range(0,H,512):
        for x in range(0,W,512):
            tile=image.crop((x,y,min(W,x+512),min(H,y+512)))
            if tile.getbbox(alpha_only=True):
                tile.save(ROOT/f"{tag}_{x//2}_{y//2}.png")
                chunks.append([x//2,y//2,tile.width//2,tile.height//2])
    return chunks


def route_preview(picture,plan):
    frame=picture.resize((W//4,H//4),Image.Resampling.NEAREST).convert("RGBA")
    overlay=Image.new("RGBA",frame.size)
    d=ImageDraw.Draw(overlay)
    for key,color in [("walls",(125,43,47,130)),("pits",(59,33,61,196)),("bridges",(79,197,198,212))]:
        for cell in plan[key]:
            x,y=(axis//4 for axis in point(*cell))
            d.rectangle((x-16,y-16,x+15,y+15),outline=color,width=1)
    for idx,route in enumerate([plan["trail"],*plan["branches"]]):
        d.line([(x//4,y//4) for x,y in curve(route)],
               fill=(255,223,148,250) if idx==0 else (170,205,182,235),width=4)
    for i,cell in enumerate(plan["camps"]):
        x,y=(axis//4 for axis in point(*cell))
        d.text((x+5,y),f"CAMP {i+1}",fill="white",stroke_width=2,stroke_fill=(55,36,31))
    d.rectangle((14,H//4-38,670,H//4-13),fill=(39,34,33,236))
    d.text((22,H//4-33),"GOLD: MAIN ROUTE | GREEN: BRANCHES | TEAL: BRIDGES | RED/PURPLE: BLOCKED",fill="white")
    Image.alpha_composite(frame,overlay).convert("RGB").save(PREVIEW/"canyon-route.png")


def main():
    ROOT.mkdir(parents=True,exist_ok=True)
    PREVIEW.mkdir(parents=True,exist_ok=True)
    plan=geometry()
    land,tread,walls=atlas(plan)
    faces(land,walls)
    parts=foreground(plan)
    manifest={"base":export(land,"base"),"floor":export(tread,"floor"),"front":parts}
    Path("resources/desert_canyon_art.edn").write_text(edn(manifest)+"\n",encoding="utf-8")
    picture=Image.new("RGBA",(W,H),(193,146,88))
    picture.alpha_composite(land)
    picture.alpha_composite(tread)
    for part in sorted(parts,key=lambda p:p["foot-y"]):
        with Image.open(ROOT/f"{part['asset']}.png") as image:
            picture.alpha_composite(image,(part["x"]*2,part["y"]*2))
    picture.convert("RGB").save(PREVIEW/"canyon.png")
    route_preview(picture,plan)
    print({key:len(value) for key,value in manifest.items()})


if __name__=="__main__":
    main()
