"""Draw a single continuous mountainside and its shared server geometry."""
import math
import random
from pathlib import Path

from PIL import Image, ImageChops, ImageDraw, ImageFilter

ROOT = Path("resources")
ART = ROOT / "images/visual_v6_1"
PREVIEWS = Path("docs/previews/world_v6_1")
ORIGIN = (2, 55)
SIZE = (34, 39)
CONTROLS = [(20,55),(20,59),(18,61),(13,62),(9,64),(8,66),(10,68),
            (15,68),(18,70),(17,72),(12,74),(8,75),(7,77),(9,79),
            (14,79),(17,81),(16,83),(12,84),(11,86),(13,88),(17,88)]
CAMPS = [(13,62),(15,68),(8,75),(14,79)]
ROCK = [(51,50,49),(70,68,65),(90,87,79),(111,106,92),(131,122,101)]
SNOW = [(80,98,111),(117,140,151),(153,174,182),(196,208,207),(225,228,216)]
MOSS = [(60,72,46),(73,82,51),(87,94,59),(103,105,67),(120,115,79)]
TILE = 32
CHUNK = 256


def path_cells(controls):
    result = [controls[0]]
    for (a,b),(x,y) in zip(controls,controls[1:]):
        steps = []
        for axis, delta in enumerate((x-a,y-b)):
            for i in range(abs(delta)):
                steps.append(((i+0.5)/abs(delta), axis, 1 if delta>0 else -1))
        for _,axis,step in sorted(steps):
            p = list(result[-1])
            p[axis] += step
            result.append(tuple(p))
    return result


def shape(x,y):
    return 1-((x-18.4)/17.3)**2-((y-74.5)/21.4)**2 + .11*math.sin(x*.38+y*.38)+.06*math.cos(y*.73-x*.31)


def geometry():
    trail = path_cells(CONTROLS)
    pads = set(trail)
    pads.update((x,y) for cx,cy in [*CAMPS,trail[-1]] for y in range(cy-1,cy+2) for x in range(cx-2,cx+3))
    blocks = {(x,y) for y in range(56,94) for x in range(2,35)
              if shape(x+.5,y+.5)>.10 and (x,y) not in pads}
    props = {}
    styles = [["tent","campfire","crate"],["raider-tent","lantern","log-bundle"],
              ["tent","campfire","barrel"],["tent","campfire","sign"],
              ["tent","blue-flag","cairn"]]
    for (cx,cy),kinds in zip([*CAMPS,trail[-1]],styles):
        available = [(cx+dx,cy+dy) for dx,dy in [(-2,-1),(0,-1),(2,-1),(-2,1),(0,1),(2,1),(-1,-1),(1,1)]
                     if (cx+dx,cy+dy) not in trail]
        props.update(zip(available,kinds))
    return trail,pads,blocks,props


def blend(a,b,t):
    return tuple(round(u+(v-u)*t) for u,v in zip(a,b))


def paint(trail,pads,blocks):
    w,h=[n*TILE for n in SIZE]
    ox,oy=ORIGIN
    image=Image.new("RGBA",(w,h))
    px=image.load()
    rng=random.Random(6120321)
    # Distance to the climb is feathered before the rock profile is shaded.
    open_mask=Image.new("L",(w,h))
    m=ImageDraw.Draw(open_mask)
    for x,y in pads:
        a,b=(x-ox)*TILE,(y-oy)*TILE
        m.rectangle((a,b,a+TILE-1,b+TILE-1),fill=255)
    soft=open_mask.filter(ImageFilter.GaussianBlur(12)).load()
    for j in range(h):
        y=oy+j/TILE
        for i in range(w):
            x=ox+i/TILE
            z=shape(x,y)
            if z<-.17:
                continue
            elevation=max(0,min(1,(y-56)/35 + .11*math.cos((x-13)*.19)))
            veins=math.sin((i+j*.28)/31+math.sin(j/54)*2)+.45*math.sin(i/11-j/36)
            stratum=(j*0.116+i*0.026+4*math.sin(i/108)+2*math.sin(i/41))%8
            is_rock=(int(x),int(y)) in blocks
            shelf=soft[i,j]/255
            snow=max(0,min(1,(elevation-.59)*4 + veins*.18))
            edge=max(0,min(1,(z+.17)*6,
                           min(i,w-1-i)/24, j/44, (h-1-j)/34))
            tone=2 + .40*veins + .21*math.sin(i*.067-j*.031)
            if is_rock and shelf<.42:
                if stratum<.67 and veins>.0: tone-=.46
                elif stratum>7.46 and veins<.2: tone+=.48
            elif shelf>.34:
                tone=2.13+.17*veins
            lo=max(0,min(3,math.floor(tone)))
            f=max(0,min(1,tone-lo))
            moss=blend(MOSS[lo],MOSS[lo+1],f)
            rock=blend(ROCK[lo],ROCK[lo+1],f)
            ice=blend(SNOW[lo],SNOW[lo+1],f)
            color=blend(blend(moss,rock,min(1,elevation*3+.12)),ice,snow)
            grain=rng.choice((-7,-4,-2,0,0,0,2,3,5))
            px[i,j]=(*[max(0,min(255,c+grain)) for c in color],int(255*edge))
    draw=ImageDraw.Draw(image)
    for j in range(6800):
        x,y=rng.randrange(10,w-12),rng.randrange(10,h-12)
        if (int(ox+x/TILE),int(oy+y/TILE)) not in blocks:
            continue
        c=px[x,y]
        if c[3]<200: continue
        length=rng.randint(2,15)
        ink=(*[max(0,k-rng.randrange(8,27)) for k in c[:3]],c[3])
        draw.line([(x,y),(x+length//2,y+1),(x+length,y+rng.randint(0,5))],fill=ink)
    for x,y in sorted(blocks):
        # The shelves above the trail cast short shadows onto the scree.
        if (x,y+1) not in pads:
            continue
        a,b=(x-ox)*TILE,(y+1-oy)*TILE
        for i in range(TILE):
            top=b-int(8+5*math.sin((a+i)/11)+rng.randrange(4))
            color=px[a+i,top]
            draw.line((a+i,top,a+i,b-1),fill=(*[int(ch*.69) for ch in color[:3]],color[3]))
        for _ in range(7):
            u,v=a+rng.randrange(2,29),b+rng.randrange(-10,0)
            draw.polygon([(u,v-3),(u+3,v),(u+1,v+2),(u-2,v+1)],fill=ROCK[rng.randint(0,3)])
    # The main spine ties the exposures into a single formation. Its
    # shoulders are shaded continuously so that terraces belong to the same rock.
    flank=Image.new("RGBA",image.size)
    fl=flank.load()
    spine=Image.new("L",image.size)
    ImageDraw.Draw(spine).polygon([
        (556,171),(689,216),(779,316),(821,386),(776,474),(839,587),
        (801,725),(867,831),(823,980),(758,1081),(637,1120),(560,1077),
        (489,975),(475,876),(515,804),(458,715),(504,601),(454,523),
        (506,429),(454,334),(469,251)
    ],fill=255)
    shoulder=spine.filter(ImageFilter.GaussianBlur(19)).load()
    for y in range(125,1169):
        bend=612+math.sin(y/159)*57+math.sin(y/311)*28
        for x in range(355,915):
            strength=shoulder[x,y]
            if strength<3 or (ox+x//TILE,oy+y//TILE) not in blocks:
                continue
            t=(x-bend)/215
            seam=(y*.081+x*.13+4.2*math.sin(x/55)+5.4*math.sin(y/102))%8
            light=2.1-1.28*t + .22*math.sin(x/8-y/31)
            if seam<.50:
                light-=.38
            elif seam>7.38:
                light+=.39
            lo=max(0,min(3,math.floor(light)))
            f=max(0,min(1,light-lo))
            cold=max(0,min(1,(y-645)/190+.07*math.sin(x/20)))
            rgb=blend(blend(ROCK[lo],ROCK[lo+1],f),
                      blend(SNOW[lo],SNOW[lo+1],f),cold)
            ink=rng.choice((-5,-2,0,0,2,3))
            fl[x,y]=(*[max(0,min(255,c+ink)) for c in rgb],strength*3//4)
    image.alpha_composite(flank)
    # Interlocking faces along the main ridge. The top ridge is continuous,
    # while its weathered foothills split into shallower ribs.
    faces=Image.new("RGBA",image.size)
    f=ImageDraw.Draw(faces)
    for k,(cx,cy,span,rise) in enumerate([
        (555,251,262,116),(753,371,292,184),(513,493,225,132),
        (722,590,315,201),(660,765,334,212),(607,891,300,238),
        (720,1030,257,142),(303,963,239,125),(348,364,210,93),
    ]):
        left,right=max(10,cx-span//2),min(w-11,cx+span//2)
        ceiling=[]
        lip=[]
        p=SNOW if cy>725 else ROCK
        for x in range(left,right+1,4):
            t=(x-left)/(right-left)
            envelope=math.sin(math.pi*t)**.58
            roof=cy-int(envelope*(rise*.69+rise*.20*math.cos(t*8+k)
                                  +rise*.11*math.sin(t*23+k)))-rng.randrange(4)
            ceiling.append((x,roof))
            lip.append((x,cy+int(13*math.sin(x/35+k))-int(13*abs(t-.46))))
        polygon=ceiling+lip[::-1]
        local=Image.new("RGBA",image.size)
        l=ImageDraw.Draw(local)
        l.polygon(polygon,fill=(*p[1],220))
        i=0
        while i<len(ceiling)-1:
            j=min(len(ceiling)-1,i+rng.randrange(4,13))
            x,top=ceiling[i]
            xe,end=ceiling[j]
            floor=lip[i][1]
            elbow=x+rng.randrange(3,16)
            shoulder=(top+floor)//2
            l.polygon(ceiling[i:j+1]+[(xe+9,floor),(elbow,floor+2),(x-5,floor-6)],
                      fill=(*p[rng.choice((0,1,2))],228))
            l.polygon(ceiling[i:j+1]+[(elbow,shoulder),(elbow-3,shoulder+16),(x-1,floor-5)],
                      fill=(*p[rng.choice((2,3,3,4))],237))
            l.polygon(ceiling[i:j+1]+[(xe-8,end+10),(elbow,top+18),(x-2,top+13)],
                      fill=(*p[4],241))
            for y in range(top+25,floor-8,rng.randrange(7,16)):
                l.line((x,y,elbow,y+2,xe-4,y+7),fill=(*p[rng.choice((1,2,3))],190))
            i=j
        mask=Image.new("L",image.size)
        ImageDraw.Draw(mask).polygon(polygon,fill=255)
        local.putalpha(ImageChops.multiply(mask,local.getchannel("A")))
        faces.alpha_composite(local)
    rock_mask=Image.new("L",image.size)
    md=ImageDraw.Draw(rock_mask)
    for x,y in blocks:
        a,b=(x-ox)*TILE,(y-oy)*TILE
        md.rectangle((a,b,a+TILE-1,b+TILE-1),fill=255)
    rock_mask=rock_mask.filter(ImageFilter.GaussianBlur(3))
    faces.putalpha(ImageChops.multiply(rock_mask,faces.getchannel("A")))
    image.alpha_composite(faces)
    # A shelf reads as a vertical drop immediately above the walking grade.
    # Connected runs share a cliff profile and irregular strata.
    cliff=Image.new("RGBA", image.size)
    c=ImageDraw.Draw(cliff)
    for row in range(57,93):
        strip=[x for x in range(2,35) if (x,row) in blocks and (x,row+1) in pads]
        runs=[]
        for x in strip:
            if not runs or x != runs[-1][-1]+1:
                runs.append([x])
            else:
                runs[-1].append(x)
        for run in runs:
            a=(run[0]-ox)*TILE
            b=(run[-1]+1-ox)*TILE
            foot=(row+1-oy)*TILE
            rise=min(31,19+int((row-57)/3))
            p=SNOW if row>=78 else ROCK
            top=[(x,foot-rise+int(5*math.sin(x/14+row)+3*math.sin(x/7)))
                 for x in range(a,b+1,3)]
            bottom=[(x,foot-1-int(3+2*math.sin(x/8))) for x,_ in top]
            mask=Image.new("L",image.size)
            ImageDraw.Draw(mask).polygon(top+bottom[::-1],fill=255)
            wall=Image.new("RGBA",image.size)
            f=ImageDraw.Draw(wall)
            f.polygon(top+bottom[::-1],fill=p[0])
            j=0
            while j<len(top)-1:
                end=min(len(top)-1,j+rng.randrange(3,8))
                x,y=top[j]
                rx,ry=top[end]
                base=bottom[j][1]
                f.polygon(top[j:end+1]+[(rx-3,base),(x+2,base-1),(x-3,base-5)],
                          fill=p[rng.choice((1,1,2,2,3))])
                f.polygon(top[j:end+1]+[(rx-6,ry+4),(x+4,y+10),(x-2,y+5)],
                          fill=p[4] if row>=78 else p[3])
                for hline in range(y+11,base-5,7):
                    f.line((x,hline,rx-5,hline+3),fill=p[rng.choice((0,1,3))])
                j=end
            wall.putalpha(ImageChops.multiply(mask,wall.getchannel("A")))
            cliff.alpha_composite(wall)
            for _ in range(3*len(run)):
                x,y=rng.randrange(a+4,b-4),foot+rng.randrange(-5,8)
                c.polygon([(x-2,y+1),(x,y-3),(x+4,y+1),(x+2,y+3)],fill=p[rng.choice((0,1,2))])
    image.alpha_composite(cliff)
    route=Image.new("RGBA",image.size)
    d=ImageDraw.Draw(route)
    points=[((x-ox)*TILE+16,(y-oy)*TILE+16) for x,y in trail]
    smooth=points
    for _ in range(3):
        smooth=[smooth[0], *[p for a,b in zip(smooth,smooth[1:])
                for p in ((.75*a[0]+.25*b[0],.75*a[1]+.25*b[1]),
                          (.25*a[0]+.75*b[0],.25*a[1]+.75*b[1]))], smooth[-1]]
    def banks(offset):
        left,right=[],[]
        for i,(x,y) in enumerate(smooth):
            ax,ay=smooth[max(0,i-1)]
            bx,by=smooth[min(len(smooth)-1,i+1)]
            length=max(.01,math.hypot(bx-ax,by-ay))
            dx,dy=(by-ay)/length,(ax-bx)/length
            width=5.5 + offset + 1.8*math.sin(i*.059) + .75*math.sin(i*.38)
            fade=min(1, (len(smooth)-i+1)/13)
            left.append((x+dx*width*fade,y+dy*width*fade))
            right.append((x-dx*width*fade,y-dy*width*fade))
        return left+right[::-1]
    for extent,color in [(4,(64,54,38,82)),(2,(103,87,65,143)),(0,(155,132,94,236))]:
        d.polygon(banks(extent),fill=color)
    for x,y in smooth[::2]:
        for _ in range(5):
            u,v=x+rng.randint(-4,4),y+rng.randint(-4,4)
            d.point((u,v),fill=rng.choice([(164,137,101,243),(111,96,76,220),(189,157,113,242)]))
    image.alpha_composite(route)
    return image


def edn(v):
    if isinstance(v,dict): return "{"+ " ".join(edn(k)+" "+edn(value) for k,value in v.items())+"}"
    if isinstance(v,str): return ":"+v
    if isinstance(v,int): return str(v)
    return "["+" ".join(map(edn,v))+"]"


def main():
    ART.mkdir(parents=True,exist_ok=True)
    PREVIEWS.mkdir(parents=True,exist_ok=True)
    trail,pads,blocks,props=geometry()
    image=paint(trail,pads,blocks)
    full=image.resize((image.width*2,image.height*2),Image.Resampling.NEAREST)
    chunks=[]
    for y in range(0,full.height,CHUNK):
        for x in range(0,full.width,CHUNK):
            part=full.crop((x,y,min(x+CHUNK,full.width),min(y+CHUNK,full.height)))
            if part.getbbox():
                part.save(ART/f"{x}_{y}.png")
                chunks.append([x,y,part.width,part.height])
    (ROOT/"massive_mountain.edn").write_text(edn({
        "origin": ORIGIN, "size": SIZE,"trail": trail,"platforms":sorted(pads),
        "blocks":sorted(blocks),"props":props,"camps":CAMPS,"chunks":chunks
    })+"\n",encoding="utf-8")
    background=Image.new("RGBA",full.size,(104,119,75,255))
    background.alpha_composite(full)
    background.convert("RGB").save(PREVIEWS/"massif.png")
    chart=background.copy()
    d=ImageDraw.Draw(chart)
    points=[((x-ORIGIN[0])*64+32,(y-ORIGIN[1])*64+32) for x,y in trail]
    d.line(points,fill=(253,210,92),width=6)
    for k,cell in enumerate([trail[0],*CAMPS,trail[-1]]):
        x,y=points[trail.index(cell)]
        d.ellipse((x-17,y-17,x+17,y+17),fill=(33,48,53),outline=(253,210,92),width=3)
        d.text((x-3,y-5),str(k),fill=(253,225,149))
    chart.convert("RGB").save(PREVIEWS/"route.png")
    print(f"{full.width}x{full.height}, {len(chunks)} chunks, {len(trail)} trail cells, {len(blocks)} rocks")


if __name__=="__main__":
    main()
