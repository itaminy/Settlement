"""Paint an oversampled, world-aligned mountain atlas using the shared collision map."""
from pathlib import Path
import random

import numpy as np
from PIL import Image, ImageDraw, ImageChops

from generate_massive_mountain import CAMPS, ORIGIN, SIZE, geometry, edn
from generate_massive_mountain_v6_2 import camp_objects, smoothed
from generate_massive_mountain_v6_3 import route_map
import generate_massive_mountain_v6_2 as shelters

ROOT = Path("resources/images/visual_v6_3_1")
PREVIEW = Path("docs/previews/world_v6_3_1")
SCALE = 2
CELL = 64 * SCALE
W,H = (n*CELL for n in SIZE)
R = random.Random(631181)
RNG = np.random.default_rng(631122)
ROCK = [(31,36,40),(42,47,50),(57,59,59),(72,72,67),(89,87,79),(107,102,91),
        (125,117,102),(148,136,118),(171,155,133)]
ICE = [(32,53,67),(44,69,86),(59,87,104),(78,108,123),(101,131,143),
       (128,156,165),(157,181,185),(183,201,201),(216,224,217)]
GREEN = [(36,47,36),(44,59,42),(55,70,45),(67,80,48),(80,92,52),(98,104,63),
         (116,116,76),(132,129,91),(150,145,103)]
DIRT = [(60,53,46),(77,64,52),(95,78,61),(115,95,74),(134,113,87),
        (152,132,105),(170,148,119),(190,169,138),(205,185,153)]
PACKED_SNOW = [(91,119,137),(112,138,153),(130,156,169),(148,171,179),
               (162,183,188),(176,195,196),(194,208,204),(212,221,212),(225,231,220)]


def point(cell):
    return tuple((c-o)*CELL+CELL//2 for c,o in zip(cell,ORIGIN))


def mask(cells):
    image = Image.new("L",(W,H))
    d = ImageDraw.Draw(image)
    for cell in cells:
        x,y = point(cell)
        d.rectangle((x-CELL//2,y-CELL//2,x+CELL//2-1,y+CELL//2-1),fill=255)
    return image


def noise(columns, rows):
    values = RNG.random((rows,columns),dtype=np.float32)
    return Image.fromarray(values).resize((W,H),Image.Resampling.BILINEAR)


def route_curve(trail):
    """Round corners within a small radius of the authoritative cell centers."""
    # Feet are anchored toward the south edge of a tile in Swing. Offset the
    # walking grade slightly so the feet and the cell center both stay on it.
    points=[(x,y+26) for x,y in map(point,trail)]
    route=[points[0]]
    for a,b,c in zip(points,points[1:],points[2:]):
        if a[0]==c[0] or a[1]==c[1]:
            route.append(b)
            continue
        entry=tuple(v+(u-v)*.23 for u,v in zip(a,b))
        exit=tuple(v+(u-v)*.23 for u,v in zip(c,b))
        for step in range(9):
            t=step/8
            route.append(tuple((1-t)**2*u+2*t*(1-t)*v+t*t*w for u,v,w in zip(entry,b,exit)))
    route.append(points[-1])
    return route


def distance_field(route):
    """Exact point-to-segment distance on the source pixel grid."""
    radius = 256
    distance = np.full((H,W),radius,dtype=np.float32)
    for (x,y),(u,v) in zip(route,route[1:]):
        l,r = max(0,int(min(x,u))-radius),min(W,int(max(x,u))+radius+2)
        t,b = max(0,int(min(y,v))-radius),min(H,int(max(y,v))+radius+2)
        dx = np.arange(l,r,dtype=np.float32)[None,:]-x
        dy = np.arange(t,b,dtype=np.float32)[:,None]-y
        run,rise = u-x,v-y
        along = np.clip((dx*run+dy*rise)/(run*run+rise*rise),0,1)
        target = distance[t:b,l:r]
        np.minimum(target,np.hypot(dx-along*run,dy-along*rise),out=target)
    return distance


def buttress(x,y,cx,cy,span,reach,tilt):
    cross = abs(x-cx-(y-cy)*tilt)/span
    longitudinal = abs(y-cy)/reach
    return np.maximum(0,1-(cross**1.16+longitudinal**1.43))**1.28


def paint(relief_distance, cell_distance, blocked, platforms, summit):
    broad = noise(15,17)
    chips = noise(948,1000)
    mineral = noise(170,194)
    base = Image.new("RGBA",(W,H))
    tread = Image.new("RGBA",(W,H))
    x = np.arange(W,dtype=np.float32)[None,:]/4
    for top in range(0,H,128):
        start,end = max(0,top-1), min(H,top+129)
        box = (0,start,W,end)
        y = np.arange(start,end,dtype=np.float32)[:,None]/4
        n = np.asarray(broad.crop(box))
        chip = np.asarray(chips.crop(box))
        fleck = np.asarray(mineral.crop(box))
        exposed = np.asarray(blocked.crop(box))>0
        walk = np.asarray(platforms.crop(box))>0
        distance = relief_distance[start:end]/4
        # Diagonal arms intersect to give one continuous, asymmetric spine.
        mass = (
            97*buttress(x,y,515,233,445,260,-.55)
            + 154*buttress(x,y,658,420,488,316,-.83)
            + 177*buttress(x,y,405,587,403,352,.74)
            + 191*buttress(x,y,701,705,382,389,-.57)
            + 198*buttress(x,y,412,886,329,342,-.42)
            + 287*buttress(x,y,594,962,246,278,-.91)
            + 141*buttress(x,y,695,1030,323,229,.63))
        ravines = (69*buttress(x,y,548,505,72,259,-.78)
                   + 78*buttress(x,y,656,756,54,194,.85))
        margin = np.clip(np.minimum(np.minimum(x/28,(1087-x)/32),
                                    np.minimum(y/45,(1247-y)/52)),0,1)
        opacity = np.clip((mass+21*n-24+(chip-.5)*13)/39,0,1)*margin
        shoulder = np.clip((distance-10)/51,0,1)
        rock_wall = shoulder*shoulder*(3-2*shoulder)
        bedding = (x*.117+y*.188+8*n)
        rock = mass-ravines+14*(n-.5)+3*(fleck-.5)
        height = y*.082+rock*rock_wall
        dy,dx = np.gradient(height)
        light = np.clip(-1.44*dx-1.14*dy,-2.5,2.5)
        face = np.maximum(abs(dx),abs(dy))
        # Hairline faults vanish on level ground and are interrupted by mineral patches.
        cracks = ((bedding+height*.19)%2.9 < .21) & (fleck>.63) & (face>.55) & exposed
        level = 4.28+light*1.12 + (fleck-.5)*1.38 + (chip-.5)*1.1 - .66*cracks
        level -= np.clip((26-distance)/18,0,1)*np.clip(mass/170,0,1)*.53
        snow_edge = 754-(-.27*(x-450))+52*(n-.5)
        frost = np.clip((y-snow_edge+height*.21)/128,0,1)
        frost *= np.clip(1-face*.13,.52,1)
        moss = np.clip((435-y)/235+(n-.52)*1.4,0,.88)*np.clip(1-face*.3,0,1)
        # Small sets of precomputed tone steps keep hard pixel edges.
        def colors(palette, tones):
            return np.asarray(palette,dtype=np.float32)[np.uint8(np.clip(np.rint(tones),0,8))]
        def blend(a,b,t):
            return a+(b-a)*t[...,None]
        rgb = blend(colors(ROCK,level),colors(GREEN,level),(np.rint(moss*4)/4))
        rgb = blend(rgb,colors(ICE,level+1.05),np.rint(frost*5)/5)
        base_rgb = np.dstack((rgb,np.round(opacity*5)*51))
        width = 11+1.55*np.sin(y*.034+x*.022)+1.65*(n-.5)+1.5*(fleck-.5)
        tread_edge = np.clip((width-distance+(chip-.5)*2.7)/3.1,0,1)*walk
        # Keep a narrow shoulder through the tile centers behind each rounded bend.
        shoulder_edge = np.clip((6.5-cell_distance[start:end]/4+(chip-.5)*2)/3.1,0,1)*walk
        tread_edge = np.maximum(tread_edge,shoulder_edge)
        for camp in (*CAMPS, summit):
            u,v = (p/4 for p in point(camp))
            apron = np.clip(1.05-np.hypot((x-u)/74,(y-v)/39)+(fleck-.5)*.23,0,1)*walk
            tread_edge = np.maximum(tread_edge,apron)
        path_tone = 4.35 +(chip-.5)*1.11+(fleck-.5)*1.12
        packed_snow = np.clip((y-795+(n-.5)*39)/92,0,1)
        snow_tone = path_tone+.72-np.clip((distance-5)/6,0,1)*1.2
        tread_rgb = blend(colors(DIRT,path_tone),colors(PACKED_SNOW,snow_tone),np.rint(packed_snow*5)/5)
        path_rgb = np.dstack((tread_rgb,np.rint(tread_edge*5)*48))
        for output,array in ((base,base_rgb),(tread,path_rgb)):
            output.paste(Image.fromarray(array[top-start:top-start+128].astype(np.uint8)),(0,top))
    return base,tread


def crag(line, depth, palette):
    """Irregular projecting faces with slanted joints, broken crowns and bedding."""
    knots = smoothed(line,2)
    left = int(min(x for x,_ in knots)-22)
    top = int(min(y for _,y in knots)-12)
    w = int(max(x for x,_ in knots)-left+44)
    h = int(max(y for _,y in knots)-top+depth*1.64+48)
    image = Image.new("RGBA",(w*4,h*4))
    d = ImageDraw.Draw(image)
    upper = [(round((x-left)*4),round((y-top+R.uniform(-2.1,2.1))*4)) for x,y in knots]
    lower=[]
    for i,(x,y) in enumerate(upper):
        taper = min(1,.21+min(i,len(upper)-1-i)*.22)
        drop = round(depth*4*taper*(.78+.27*np.sin(i*.36)+R.uniform(-.12,.19)))
        lower.append((x+int((27*np.sin(i*.43)+12)*taper)+drop//8,y+drop))
    d.polygon(upper+lower[::-1],fill=palette[2])
    for i,((x,y),(u,v)) in enumerate(zip(upper,upper[1:])):
        a,b = lower[i]
        c,e = lower[i+1]
        tone = 3 + int(v-y < 0) + R.choice((-1,0,0,1))
        mid = (int((x+2*a)/3)+R.randint(-11,16),int((y+b)/2))
        d.polygon([(x,y),(u,v),(c,e),(a,b)],fill=palette[tone])
        d.polygon([(x,y),(u,v),mid,(a,b)],fill=palette[tone+1])
        d.line([(x,y+5),mid,(a,b-2)],fill=palette[max(0,tone-2)],width=R.randint(3,7))
        # Bedding breaks into plates. Follow both edge joints to keep marks in perspective.
        for j in range(max(1,abs(u-x)*max(0,b-y)//360)):
            t,s = R.uniform(.06,.94),R.uniform(.07,.95)
            px=(x+(u-x)*s)*(1-t)+(a+(c-a)*s)*t
            py=(y+(v-y)*s)*(1-t)+(b+(e-b)*s)*t
            length=R.randint(4,29)
            d.line([(px-length,py-2),(px,py),(px+length,py+R.randint(2,7))],
                   fill=palette[R.choice((tone-1,tone,tone+1,tone+2))],width=R.randint(2,5))
        for k in range(18,min(b-y,e-v)-10,R.randint(19,43)):
            p = x+int((a-x)*k/max(1,b-y))
            q = u+int((c-u)*k/max(1,e-v))
            d.line([(p,y+k),((p+q)//2,y+k+(v-y)//4+R.randint(-9,10)),(q,v+k)],
                   fill=palette[R.choice((tone-1,tone,tone+1,tone+2))],width=R.randint(2,6))
        if i%3 != 1:
            d.line([(x,y),(u,v)],fill=palette[6],width=R.randint(4,10))
    area=Image.new("L",image.size)
    ImageDraw.Draw(area).polygon(upper+lower[::-1],fill=255)
    image.putalpha(ImageChops.multiply(area,image.getchannel("A")))
    return image,(left*4,top*4)


def rock_faces(base, blocked):
    faces = Image.new("RGBA",(W,H))
    shade = Image.new("RGBA",(W,H))
    ground = ImageDraw.Draw(shade)
    band = [
        ([(172,209),(218,162),(280,155),(331,171),(391,218)],30,ROCK),
        ([(285,350),(359,283),(421,267),(485,288),(525,343),(575,383),(672,422)],53,ROCK),
        ([(516,326),(569,300),(643,321),(710,370),(776,415),(855,436)],64,ROCK),
        ([(248,599),(333,565),(392,494),(447,479),(487,441),(553,430),(627,447)],70,ROCK),
        ([(559,441),(646,447),(689,493),(724,565),(788,587),(868,618)],89,ROCK),
        ([(261,767),(336,717),(404,685),(483,646),(530,581),(604,546)],84,ROCK),
        ([(688,575),(662,666),(649,713),(608,762),(568,847)],74,ICE),
        ([(681,688),(729,722),(779,764),(849,785)],86,ICE),
        ([(315,951),(390,929),(444,867),(486,843),(542,858),(591,881)],92,ICE),
        ([(507,916),(542,870),(578,832),(600,843),(631,897),(653,938),(718,957),(786,1019)],120,ICE),
        ([(630,904),(670,932),(670,1028),(704,1081),(752,1110)],97,ICE),
        ([(405,1144),(456,1112),(505,1118),(556,1082),(601,1056),(666,1048)],59,ICE),
    ]
    for line,depth,palette in band:
        face,offset = crag(line,depth,palette)
        faces.alpha_composite(face,offset)
        profile=[(int(x*4),int((y+depth*.85)*4)) for x,y in smoothed(line,2)]
        for width,alpha in [(53,32),(31,65),(15,111)]:
            ground.line(profile,fill=(28,33,35,alpha),width=width,joint="curve")
    allowed = ImageChops.multiply(blocked,base.getchannel("A"))
    shade.putalpha(ImageChops.multiply(shade.getchannel("A"),allowed))
    faces.putalpha(ImageChops.multiply(faces.getchannel("A"),allowed))
    base.alpha_composite(shade)
    base.alpha_composite(faces)


def surface_details(base, tread, blocked, trail):
    d = ImageDraw.Draw(base)
    detail = ImageDraw.Draw(tread)
    for _ in range(7400):
        x,y = R.randrange(20,W-30),R.randrange(25,H-35)
        r,g,b,a = base.getpixel((x,y))
        if a<225 or not blocked.getpixel((x,y)):
            continue
        size = R.randrange(3,26)
        delta = R.choice((-23,-14,-9,8,14,23))
        color = tuple(max(0,min(255,c+delta)) for c in (r,g,b))
        d.polygon([(x-size,y),(x-size//3,y-size//3),(x+size//2,y-size//5),
                   (x+size,y+size//7),(x+size//4,y+size//3)],fill=(*color,a))
        if size>15:
            d.line((x-size,y,x-size//3,y-size//3,x+size//2,y-size//5),
                   fill=tuple(min(255,c+21) for c in color),width=2)

    for i,cell in enumerate(trail):
        x,y = point(cell)
        for _ in range(16):
            u,v = x+R.randrange(-47,48),y+R.randrange(-56,57)
            r,g,b,a = tread.getpixel((u,v))
            if a<120:
                continue
            color = tuple(max(0,c-R.randrange(12,25)) for c in (r,g,b))
            detail.line((u,v,u+R.randrange(2,11),v+R.randrange(-1,4)),fill=(*color,a),width=2)
        if i>=60:
            for dx,dy in ((-10,15),(9,29),(-7,43)):
                detail.polygon([(x+dx-4,y+dy-4),(x+dx+4,y+dy-4),
                                (x+dx+3,y+dy+5),(x+dx-3,y+dy+5)],
                               fill=(106,139,156,219))

    for i in (18,28,48,64,82):
        x,y = point(trail[i])
        y += 26
        horizontal = trail[i-1][1] == trail[i+1][1]
        palette = ICE if i>60 else ROCK
        for n in range(-37,39,14):
            coords = [(x+n,y-31),(x+n+2,y+31)] if horizontal else [(x-34,y+n),(x+32,y+n+3)]
            detail.line(coords,fill=palette[2],width=7)
            detail.line([(u-2,v-3) for u,v in coords],fill=palette[6],width=4)
    for i in (35,58):
        x,y = point(trail[i])
        y += 26
        horizontal = trail[i-1][1] == trail[i+1][1]
        for n in range(-42,43,11):
            wear = R.randrange(0,18)
            coords = [(x+n,y-27),(x+n+2,y+28)] if horizontal else [(x-28,y+n),(x+28,y+n+2)]
            detail.line(coords,fill=(105+wear,75+wear,52+wear),width=10)
            detail.line([(u-1,v-3) for u,v in coords],fill=(156+wear,119+wear,82+wear),width=3)


def trees(base, blocked):
    d = ImageDraw.Draw(base)
    for kind,limit,count in [("pine",420,29),("spruce",432,24),("birch",249,10)]:
        tree = Image.open(Path("resources/images/nature_v3")/f"{kind}.png").convert("RGBA")
        tree = tree.crop(tree.getbbox())
        stamps=[]
        for _ in range(count*9):
            x,y = R.randrange(220,W-220),R.randrange(245,limit*4)
            if blocked.getpixel((x,y)) and base.getpixel((x,y))[3]==255:
                stamps.append((y,x))
            if len(stamps)==count:
                break
        for y,x in sorted(stamps):
            height = R.randrange(48,115)
            sprite = tree.resize((height*tree.width//tree.height,height),Image.Resampling.NEAREST)
            d.polygon([(x-24,y-5),(x+33,y-9),(x+46,y+4),(x+7,y+11),(x-29,y+5)],
                      fill=(53,60,41,255))
            base.alpha_composite(sprite,(x-sprite.width//2,y-sprite.height))


def export(image, layer):
    """Store only visible atlas fragments, with coordinates in logical pixels."""
    chunks=[]
    span = 256*SCALE
    for y in range(0,H,span):
        for x in range(0,W,span):
            image_piece = image.crop((x,y,min(W,x+span),min(H,y+span)))
            if image_piece.getbbox(alpha_only=True):
                image_piece.save(ROOT/f"{layer}_{x//SCALE}_{y//SCALE}.png")
                chunks.append([x//SCALE,y//SCALE,image_piece.width//SCALE,image_piece.height//SCALE])
    return chunks


def fronts(blocked, trail):
    shelters.ROOT = ROOT
    objects = camp_objects(trail)
    # Equipment is part of the shelter sheet so the whole assembly shares its depth.
    for i,part in enumerate(objects):
        path=ROOT/f"{part['asset']}.png"
        with Image.open(path) as source:
            sprite=source.convert("RGBA")
        d=ImageDraw.Draw(sprite)
        d.line([(27,101),(65,102),(105,102),(144,98)],fill=(37,34,31,130),width=3)
        for dx in range(0,18,5):
            x,y=119+dx,93-dx//3
            if i in (0,1):
                d.line((x-11,y+5,x,y),fill=(102,70,43),width=5)
                d.rectangle((x-13,y+4,x-10,y+7),fill=(173,122,73))
            else:
                d.polygon([(x-5,y+6),(x,y),(x+5,y),(x+7,y+6)],fill=ICE[3+i%3])
        if i in (2,3,4):
            d.line([(30,61),(52,35),(75,36)],fill=(210,221,214),width=3)
        sprite.save(path)
    for i,(cell,depth) in enumerate([((9,63),18),((12,71),26),((15,76),31),((19,89),35)]):
        x,y = (p/4 for p in point(cell))
        # Foreground silhouettes are limited to blocked pixels.
        face,(u,v) = crag([(x-20,y+4),(x-12,y-11),(x+2,y-16),(x+14,y-9),(x+24,y+9)],
                          depth,ICE if i==3 else ROCK)
        allowed = blocked.crop((u,v,u+face.width,v+face.height))
        face.putalpha(ImageChops.multiply(face.getchannel("A"),allowed))
        if face.getbbox(alpha_only=True):
            name = f"front{i}"
            sprite = face.resize((face.width//2,face.height//2),Image.Resampling.NEAREST)
            sprite.save(ROOT/f"{name}.png")
            objects.append({"x":u//2,"y":v//2,"width":sprite.width,"height":sprite.height,
                            "foot-y":int((y+13)*2),"kind":"mountain-face","asset":name})
    return objects


if __name__ == "__main__":
    ROOT.mkdir(parents=True,exist_ok=True)
    PREVIEW.mkdir(parents=True,exist_ok=True)
    trail,pads,blocks,_ = geometry()
    blocked = mask(blocks)
    route = route_curve(trail)
    base,tread = paint(distance_field(route),distance_field([point(cell) for cell in trail]),
                       blocked,mask(pads),trail[-1])
    rock_faces(base,blocked)
    surface_details(base,tread,blocked,trail)
    trees(base,blocked)
    objects = fronts(blocked,trail)
    data={"scale":SCALE,"base":export(base,"base"),"tread":export(tread,"tread"),"front":objects}
    Path("resources/massive_mountain_art_v6_3_1.edn").write_text(edn(data)+"\n",encoding="utf-8")
    picture = Image.new("RGBA",(W,H),(104,119,75,255))
    picture.alpha_composite(base)
    picture.alpha_composite(tread)
    for part in sorted(objects,key=lambda p:p["foot-y"]):
        with Image.open(ROOT/f"{part['asset']}.png") as sprite:
            picture.alpha_composite(sprite.resize((part["width"]*2,part["height"]*2),Image.Resampling.NEAREST),
                                    (part["x"]*2,part["y"]*2))
    picture.convert("RGB").save(PREVIEW/"massif.png")
    # The collision overlay is intentionally shown on the smaller logical map.
    import generate_massive_mountain_v6_3 as previous
    previous.PREVIEW = PREVIEW
    route_map(picture,trail,pads,blocks)
    print({k:len(v) for k,v in data.items() if k!="scale"})
