"""Paint V6.3 relief over the common Massive Mountain gameplay geometry."""
from pathlib import Path

import numpy as np
from PIL import Image, ImageChops, ImageDraw, ImageFilter

import generate_massive_mountain_v6_2 as kit
from generate_massive_mountain import CAMPS, geometry, edn

ROOT = Path("resources/images/visual_v6_3")
PREVIEW = Path("docs/previews/world_v6_3")
X, Y, W, H = kit.X, kit.Y, kit.W, kit.H
STONE, FROST = kit.STONE, kit.FROST


def lobe(x, y, sx, sy, tilt):
    return np.exp(-(((X-x-(Y-y)*tilt)/sx)**2 + ((Y-y)/sy)**2))


def path_distance(route):
    radius = 96
    distance = np.full((H,W),radius,dtype=np.float32)
    for (x,y),(u,v) in zip(route,route[1:]):
        left,right = max(0,int(min(x,u))-radius), min(W,int(max(x,u))+radius+2)
        top,bottom = max(0,int(min(y,v))-radius), min(H,int(max(y,v))+radius+2)
        rows,columns = slice(top,bottom),slice(left,right)
        dx,dy = X[rows,columns]-x,Y[rows,columns]-y
        run,rise = u-x,v-y
        along = np.clip((dx*run+dy*rise)/(run*run+rise*rise),0,1)
        region = distance[rows,columns]
        np.minimum(region,np.hypot(dx-along*run,dy-along*rise),out=region)
    return distance


def relief(distance, blocked):
    broad = kit.noise(10, 12)
    facet = kit.noise(53, 47)
    grain = kit.noise(238, 191)
    banks = (
        67*lobe(612,205,370,160,-.6) + 110*lobe(307,332,213,235,.56)
        + 148*lobe(702,460,283,258,-.6) + 143*lobe(394,578,236,217,-.74)
        + 196*lobe(635,758,255,228,.72) + 185*lobe(376,904,195,225,-.42)
    )
    crest = 265*lobe(582,989,131,174,-.62) + 82*lobe(775,1065,178,139,.48)
    gullies = 91*lobe(550,541,64,190,.67) + 68*lobe(467,827,52,199,-.66)
    outline = np.clip((banks+crest-24)/47,0,1)
    border = np.clip(np.minimum.reduce((X/20,(W-1-X)/24,Y/22,(H-1-Y)/39)),0,1)
    alpha = np.floor(np.minimum(outline,border)*15)/15*255
    grade = Y*.105 + 14*broad
    bed = (Y*.43+X*.31+72*broad+12*facet)
    shelves = (np.floor(bed/24)*24+np.minimum(bed%24,5))/24-bed/24
    shoulder = np.clip((distance-12)/85,0,1)
    carve = shoulder*shoulder*(3-2*shoulder)
    rock = np.asarray(blocked.filter(ImageFilter.MaxFilter(5)),dtype=np.float32)/255
    height = grade + (banks+crest-gullies+48*(broad-.5)+8*shelves+13*(facet-.5))*carve
    dy,dx = np.gradient(height)
    light = (-dx*1.25-dy*.73)/np.sqrt(1+.18*(dx*dx+dy*dy))
    bedding = (height*.46+X*.064+Y*.091+3*facet)%7
    fault = (X*.096-Y*.051+9*broad+1.1*facet)%6
    value = 3.22 + np.clip(light,-2.1,2.0)*.88 + (facet-.5)*1.46
    value += rock*(np.where(bedding<.37,-.42,np.where(bedding>6.25,.31,0))
                   - (fault<.19)*.67)
    value -= np.clip(1-distance/29,0,1)*np.clip((banks+crest)/200,0,1)*.23
    value = np.round((value+(grain-.5)*.48)*5)/5
    snow = np.clip((height-158+(Y-680)*.16+20*facet)/47,0,1)
    snow *= np.clip((Y-615)/205,0,1)*np.clip(1-(np.abs(dx)+np.abs(dy))*.055,.35,1)
    snow = np.round(snow*8)/8
    moss = np.clip((.54-Y/H)*2.7+(broad-.44)*1.5,0,.82)*np.clip(1-abs(dy)*.24,0,1)
    green = [(40,50,34),(55,67,41),(71,83,49),(89,99,57),(110,117,67),(129,131,82),(152,147,98)]
    rgb = kit.mix(kit.shade(STONE,value),kit.shade(green,value),moss)
    rgb = kit.mix(rgb,kit.shade(FROST,value+.92),snow)
    return kit.rgba(rgb,alpha), snow, broad, facet, grain


def escarpments(base, blocked, distance):
    """Add forked rock faces to the shoulders and expose the summit's cornice."""
    faces = Image.new("RGBA",(W,H))
    shadow = Image.new("RGBA",(W,H))
    d = ImageDraw.Draw(shadow)
    spurs = [
        ([(165,251),(215,209),(263,199),(310,166),(352,152)],24,False),
        ([(195,390),(250,333),(337,317),(386,277),(438,264)],37,False),
        ([(434,264),(523,284),(573,325),(599,378),(672,401),(723,451)],43,False),
        ([(701,354),(757,342),(796,382),(819,425),(878,449)],31,False),
        ([(301,581),(344,541),(466,509),(524,464),(602,462),(675,421)],50,False),
        ([(602,462),(644,488),(681,572),(757,596),(816,652)],36,False),
        ([(248,760),(323,722),(379,679),(422,614),(481,595)],47,False),
        ([(651,748),(694,667),(757,596)],55,True),
        ([(586,943),(608,870),(637,809),(651,748),(694,720)],63,True),
        ([(305,1030),(376,986),(455,970),(500,920),(551,910),(586,943)],69,True),
        ([(586,943),(622,957),(685,970),(724,1016),(779,1031)],82,True),
        ([(622,957),(626,1052),(689,1120),(735,1137)],52,True),
        ([(449,1130),(491,1110),(527,1114),(565,1083),(626,1052)],34,True),
    ]
    for line,depth,icy in spurs:
        points = [(round(x),round(y+kit.R.uniform(-3,3))) for x,y in kit.smoothed(line,2)]
        wall,offset = kit.wall(points,depth,icy)
        faces.alpha_composite(wall,offset)
        d.line([(x+2,y+int(depth*.89)) for x,y in points],fill=(24,30,34,93),width=8)
    safe = Image.fromarray(np.uint8(np.clip((distance-19)/8,0,1)*255))
    allowed = ImageChops.multiply(safe,blocked.filter(ImageFilter.GaussianBlur(2)))
    allowed = ImageChops.multiply(allowed,base.getchannel("A"))
    shadow.putalpha(ImageChops.multiply(shadow.getchannel("A").filter(ImageFilter.GaussianBlur(3)),allowed))
    faces.putalpha(ImageChops.multiply(faces.getchannel("A"),allowed))
    base.alpha_composite(shadow)
    base.alpha_composite(faces)


def rim_features(base, blocked, distance):
    """Paint crevasses and gullies along the same oblique ridges as the relief."""
    details = Image.new("RGBA",(W,H))
    d = ImageDraw.Draw(details)
    r = kit.R
    for cx,cy,length,width,frozen,direction in [
        (692,388,126,9,False,-1),(490,557,115,12,False,1),
        (751,727,182,19,True,-1),(289,860,134,15,True,1),
        (648,1008,104,17,True,1),(546,1165,51,10,True,-1)
    ]:
        points = [(int(cx+direction*i*.43+11*np.sin(i/24)),
                   int(cy+i+3*np.cos(i/13))) for i in range(0,length,4)]
        shadow = (24,37,46,190) if frozen else (35,40,39,162)
        d.line(points,fill=shadow,width=width)
        d.line([(x-3,y-2) for x,y in points],fill=(96,137,155,208),width=max(2,width//3))
        if frozen:
            for x,y in points[::3]:
                drop = r.randint(6,19)
                d.line([(x-4,y-2),(x-3,y+drop),(x-1,y+1)],fill=(162,198,204,212))
        else:
            for x,y in points[::2]:
                d.line((x-1,y-3,x+1,y+6),fill=(169,196,189,205),width=2)
    keep = Image.fromarray(np.uint8(np.clip((distance-23)/16,0,1)*255))
    opacity = ImageChops.multiply(keep,blocked.filter(ImageFilter.MinFilter(5)))
    opacity = ImageChops.multiply(opacity,base.getchannel("A"))
    details.putalpha(ImageChops.multiply(details.getchannel("A"),opacity))
    base.alpha_composite(details)


def tread_material(route, distance, snow, facet, grain, safe):
    lay = kit.trail_surface(route,distance,snow,facet,grain,safe)
    pixels = np.asarray(lay).astype(np.int16)
    opacity = pixels[...,3]
    # Firm edges break into loose gravel rather than a translucent airbrush halo.
    chips = kit.RNG.integers(-13,14,(H,W),dtype=np.int16)
    ragged = 20*(grain-.5)+chips
    opacity[:] = np.clip(np.round((opacity*1.21-19+ragged)/26)*26,0,255)
    opacity[np.asarray(lay)[...,3] == 0] = 0
    treads = (distance<13) & (opacity>75)
    mineral = np.round(((grain-.5)*19+chips*.33)/3)*3
    pixels[...,:3] += np.where(treads,mineral,0).astype(np.int16)[...,None]
    return Image.fromarray(np.clip(pixels,0,255).astype(np.uint8))


def vegetation(base,blocked):
    r = kit.R
    for name, count, limit in [("pine",25,405),("spruce",16,490),("birch",10,235)]:
        tree = Image.open(Path("resources/images/nature_v3") / f"{name}.png").convert("RGBA")
        tree = tree.crop(tree.getbbox())
        stamps = []
        for _ in range(count*8):
            x,y = r.randrange(54,W-55),r.randrange(46,limit)
            if blocked.getpixel((x,y)) and base.getpixel((x,y))[3]>246:
                stamps.append((y,x))
                if len(stamps)==count:
                    break
        for y,x in sorted(stamps):
            height = r.randrange(18,33)
            stamp = tree.resize((max(1,tree.width*height//tree.height),height),Image.Resampling.NEAREST)
            base.alpha_composite(stamp,(x-stamp.width//2,y-height))


def foreground(blocked):
    """Small independent rock lips on blocked cells, never across the walking grade."""
    result = []
    for i,(cx,cy,depth) in enumerate([(19,61,16),(10,71,27),(15,76,24),(12,81,26),(19,89,33)]):
        u,v = kit.pixel((cx,cy))
        points = [(u+x,v-14+int(x*.28)+kit.R.randrange(-4,5)) for x in range(-22,23,5)]
        face,(a,b) = kit.wall(points,depth,i>=3)
        mask = blocked.crop((a,b,a+face.width,b+face.height))
        face.putalpha(ImageChops.multiply(face.getchannel("A"),mask))
        if face.getbbox():
            result.append(kit.sheet(a,b,"mountain-face",f"front{i}",v+16,face))
    return result


def route_map(picture, trail, pads, blocks):
    image = picture.convert("RGB").resize((W,H),Image.Resampling.NEAREST)
    overlay = Image.new("RGBA",image.size)
    draw = ImageDraw.Draw(overlay)
    for cell in blocks:
        x,y = kit.pixel(cell)
        draw.rectangle((x-16,y-16,x+15,y+15),fill=(54,17,21,61),outline=(208,105,91,87))
    for cell in pads:
        x,y = kit.pixel(cell)
        draw.rectangle((x-16,y-16,x+15,y+15),outline=(117,206,180,200))
    draw.line([kit.pixel(c) for c in trail],fill=(255,223,136,255),width=4)
    for label,cell in [("START",trail[0]),*((f"CAMP {i+1}",p) for i,p in enumerate(CAMPS)),
                       ("SUMMIT",trail[-1])]:
        x,y = kit.pixel(cell)
        draw.ellipse((x-5,y-5,x+5,y+5),fill=(250,204,103,255),outline=(29,34,38,255))
        draw.text((x+8,y-14),label,fill="white",stroke_width=2,stroke_fill=(28,34,38,255))
    draw.rectangle((10,H-35,545,H-9),fill=(27,35,37,240))
    draw.text((19,H-29),"88 STEPS   |   GOLD: TRAIL   |   TEAL: PLATFORMS   |   RED: ROCK COLLISION",fill="white")
    Image.alpha_composite(image.convert("RGBA"),overlay).convert("RGB").save(PREVIEW/"route.png")


def main():
    kit.ROOT = ROOT
    ROOT.mkdir(parents=True,exist_ok=True)
    PREVIEW.mkdir(parents=True,exist_ok=True)
    trail,pads,blocks,_ = geometry()
    route = kit.smoothed([kit.pixel(c) for c in trail],2)
    blocked = kit.geometry_mask(blocks)
    safe = kit.geometry_mask(pads)
    distance = path_distance(route)
    base,snow,broad,facet,grain = relief(distance,blocked)
    escarpments(base,blocked,distance)
    kit.scatter(base,blocked,snow,broad)
    rim_features(base,blocked,distance)
    vegetation(base,blocked)
    tread = tread_material(route,distance,np.maximum(snow,np.clip((Y-760)/190,0,1)*.87),
                           facet,grain,safe)
    fronts = foreground(blocked)+kit.camp_objects(trail)
    data = {"base":kit.write_chunks(base,"base"),"tread":kit.write_chunks(tread,"tread"),"front":fronts}
    Path("resources/massive_mountain_art_v6_3.edn").write_text(edn(data)+"\n",encoding="utf-8")
    picture = Image.new("RGBA",(W*2,H*2),(104,119,75))
    for layer in (base,tread):
        picture.alpha_composite(layer.resize(picture.size,Image.Resampling.NEAREST))
    for part in sorted(fronts,key=lambda el:el["foot-y"]):
        with Image.open(ROOT/f"{part['asset']}.png") as sprite:
            picture.alpha_composite(sprite,(part["x"],part["y"]))
    picture.convert("RGB").save(PREVIEW/"massif.png")
    route_map(picture,trail,pads,blocks)
    print({key:len(value) for key,value in data.items()})


if __name__ == "__main__":
    main()
