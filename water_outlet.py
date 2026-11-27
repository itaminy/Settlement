"""Paint a short, tiered fall against the existing V6.4.8 mountain profile."""
import csv
from random import Random

import numpy as np
from PIL import Image, ImageDraw, ImageFilter

from generate_natural_relief import field, full_scenes, grain, GRID, mask, pixel
from relief_integrated import shape
from relief_raster import PROJECTION, smooth


def outlet_heights(path):
    scene = full_scenes()[0]
    width,height = (n*GRID for n in scene.size)
    y,x = np.mgrid[:height,:width].astype(np.float32)
    wx = x/GRID+scene.origin[0]-.5
    wy = y/GRID+scene.origin[1]-.5
    rng = np.random.default_rng(642113)
    z = field(scene,wx,wy,grain(rng,width,height,35,49),grain(rng,width,height,82,81))
    with open(path,encoding="utf-8-sig",newline="") as src:
        wet = [(int(u),int(v)) for u,v,kind in csv.reader(src) if kind=="water"]
    water = mask(scene,wet).filter(ImageFilter.MaxFilter(13)).filter(ImageFilter.GaussianBlur(6))
    outer = np.minimum.reduce((x,y,width-1-x,height-1-y))
    z *= smooth(outer/72)*(1-np.asarray(water,dtype=np.float32)/255)
    z = shape(scene,x,y,z)
    points = [(30.5,67),(31,67),(31.5,67),(32,68.5)]
    return [float(z[v,u]) for u,v in (pixel(scene,p) for p in points)]


def paint(output, heights_path, natural=False):
    heights = outlet_heights(heights_path)
    drop = round((max(heights[:3])-heights[3])*PROJECTION)
    assert drop>=12,(heights,drop)
    if natural:
        return paint_natural(output,drop)
    x0,y0 = 30*64,67*64-38
    width,height = 94,86
    splash_y = 33+drop
    im = Image.new("RGBA",(width,height))
    d = ImageDraw.Draw(im)
    rng = Random(661)
    # Upper water is tucked behind the rock shoulders, then arcs to the lower pool.
    d.polygon([(12,20),(23,13),(29,15),(36,10),(49,11),(59,17),(62,24),
               (56,34),(29,34),(14,27)],fill=(51,91,108,238))
    d.polygon([(22,17),(35,14),(41,15),(48,14),(55,19),(59,23),(55,28),(34,26),(20,23)],
              fill=(76,134,147))
    d.line([(22,20),(31,18),(35,19),(44,17),(51,21),(55,22)],fill=(111,163,164),width=2)
    for points,color in [
        ([(1,31),(11,21),(22,19),(22,27),(17,42),(22,54),(12,52),(5,45)],(80,92,90,234)),
        ([(2,32),(12,24),(21,22),(18,29),(13,34),(6,37)],(117,128,118,245)),
        ([(57,15),(67,21),(71,33),(67,46),(75,51),(68,56),(55,47),(58,34)],(84,97,99,240)),
        ([(58,17),(66,21),(70,30),(65,29),(60,25)],(130,141,133,237)),
    ]:
        d.polygon(points,fill=color)
    d.line([(9,46),(13,41),(17,36)],fill=(63,76,78),width=2)
    d.line([(66,33),(64,41),(68,48)],fill=(71,83,85),width=2)
    rock = np.array(im)
    for v in range(15,57):
        for u in [*range(2,21),*range(59,75)]:
            r,g,b,a = map(int,rock[v,u])
            if a>190 and max(r,g,b)-min(r,g,b)<35:
                detail = rng.choice((-9,-4,0,3,8))
                d.point((u,v),fill=(r+detail,g+detail,b+detail,a))
    lower = Image.new("RGBA",im.size)
    lp = ImageDraw.Draw(lower)
    lp.ellipse((13,splash_y-9,81,splash_y+26),fill=(34,71,84,120))
    lp.ellipse((20,splash_y-4,69,splash_y+10),fill=(87,144,149,131))
    im.alpha_composite(lower.filter(ImageFilter.GaussianBlur(2)))
    d = ImageDraw.Draw(im)
    d.polygon([(23,24),(33,22),(45,23),(58,24),(57,32),(53,36),
               (55,splash_y-2),(48,splash_y+4),(27,splash_y+1),(21,splash_y-4),
               (24,36)],fill=(63,128,146))
    for a,b,k,col in [
        (26,31,-3,(104,161,168)),(34,39,1,(136,183,178)),
        (43,49,-3,(100,157,166)),(51,54,1,(76,139,156))]:
        d.polygon([(a,25),(b,26),(b-2,34),(b+k,splash_y-1),
                   (a+k-1,splash_y+1),(a+1,35)],fill=col)
    d.line([(23,25),(34,23),(46,24),(58,24)],fill=(164,198,185),width=2)
    d.line([(24,38),(30,41),(41,40),(48,42),(53,39)],fill=(114,176,174),width=2)
    for _ in range(75):
        a,b = rng.randrange(25,54),rng.randrange(28,splash_y)
        d.line((a,b,a-1,min(splash_y,b+rng.randrange(2,8))),
               fill=rng.choice(((113,173,172),(129,185,178),(87,146,156))))
    for _ in range(112):
        a,b = rng.randrange(15,74),rng.randrange(splash_y-5,splash_y+19)
        if ((a-42)/29)**2+((b-splash_y-4)/12)**2<1:
            d.line((a,b,a+rng.randrange(1,6),b),fill=rng.choice(
                ((132,181,179,192),(183,211,199,231),(214,226,212,248))))
    for diameter in (68,91):
        d.arc((41-diameter//2,splash_y-4,41+diameter//2,splash_y+diameter//3),
              13,162,fill=(132,184,181,132))
    im.save(output/"outlet-fall.png",optimize=True)
    return {"asset":"outlet-fall","x":x0,"y":y0,"width":2*width,"height":2*height,
            "flow":[x0+52,y0+54,49,2*drop+2],"pool":[x0+84,y0+2*(splash_y+5)],
            "drop":2*drop}


def paint_natural(output, drop):
    """A shallow feeder cuts through a rock cleft into the projected drop."""
    x0,y0 = 30*64-20,67*64-96
    width,height = 130,122
    crest = 54
    pool = crest+drop+13
    im = Image.new("RGBA",(width,height))
    draw = ImageDraw.Draw(im)
    rng = Random(662)
    # The creek begins behind the dark upper stones and broadens toward the lip.
    draw.polygon([(26,3),(34,4),(43,8),(47,19),(51,23),(65,25),(78,33),(81,46),
                  (78,54),(46,57),(43,44),(36,40),(34,33),(31,24),(30,13)],
                 fill=(52,99,118,245))
    draw.polygon([(31,8),(39,11),(43,22),(54,29),(67,30),(74,37),(75,49),
                  (72,54),(52,54),(48,44),(41,38),(38,27)],fill=(74,133,147))
    for points in [[(34,13),(37,22),(42,29)],[(44,30),(50,33),(61,33)],
                   [(55,39),(65,39),(72,43),(71,46)]]:
        draw.line(points,fill=(116,169,167,220),width=2)
    rocks = [
        ([(20,6),(29,4),(34,9),(30,15),(33,26),(23,30),(15,21)],(96,107,101)),
        ([(38,4),(50,6),(56,18),(49,24),(45,21),(41,12)],(103,110,103)),
        ([(39,24),(45,24),(48,31),(43,38),(51,49),(47,61),(38,62),(30,51),(30,38)],(92,103,98)),
        ([(74,25),(85,29),(93,39),(90,48),(86,60),(83,70),(74,68),(73,58),(77,49),(78,40)],(88,101,100)),
        ([(26,60),(37,57),(43,63),(42,75),(37,85),(24,80),(21,73)],(82,95,94)),
        ([(82,72),(93,71),(102,79),(98,87),(88,90),(80,83)],(79,93,92)),
    ]
    for points,palette in rocks:
        draw.polygon(points,fill=(*palette,245))
        draw.line(points[:3],fill=tuple(c+20 for c in palette),width=2)
        draw.line(points[3:6],fill=tuple(c-17 for c in palette),width=2)
    for _ in range(1300):
        x,y=rng.randrange(14,104),rng.randrange(4,92)
        r,g,b,a=im.getpixel((x,y))
        if a>=230 and max(r,g,b)-min(r,g,b)<26:
            n=rng.choice((-13,-8,-4,0,5,9,14))
            draw.point((x,y),fill=(max(0,r+n),max(0,g+n),max(0,b+n),a))
    shadow = Image.new("RGBA",im.size)
    s = ImageDraw.Draw(shadow)
    s.polygon([(34,pool-11),(48,pool-16),(78,pool-13),(102,pool-4),(98,pool+12),
               (75,pool+16),(42,pool+9),(29,pool)],fill=(28,60,68,125))
    im.alpha_composite(shadow.filter(ImageFilter.GaussianBlur(2)))
    draw = ImageDraw.Draw(im)
    draw.polygon([(47,51),(55,49),(66,51),(73,50),(78,54),(75,56),(73,64),
                  (77,pool-13),(74,pool-5),(67,pool+3),(53,pool-1),
                  (47,pool-6),(42,pool-10),(44,65)],fill=(61,127,145))
    for left,right,bottom,color in [
        (46,52,56,(100,163,174)),(52,59,62,(137,185,183)),
        (61,66,63,(91,154,168)),(68,72,69,(122,180,181))]:
        draw.polygon([(left,53),(right,53),(right-1,63),
                      (bottom+2,pool-6),(bottom-3,pool),(left,65)],fill=color)
    draw.line([(48,54),(56,51),(64,53),(70,52),(76,54)], fill=(161,201,191), width=2)
    draw.line([(42,59),(40,64),(42,72),(38,77)],fill=(110,161,165),width=2)
    draw.line([(77,59),(80,65),(79,72),(84,77),(87,81)],fill=(111,169,168),width=2)
    for _ in range(90):
        x,y=rng.randrange(48,74),rng.randrange(57,pool-7)
        draw.line((x,y,x+rng.choice((-1,0,1)),min(pool-6,y+rng.randrange(2,7))),
                  fill=rng.choice(((106,164,171),(148,193,187),(81,142,158))))
    for _ in range(106):
        x,y=rng.randrange(31,97),rng.randrange(pool-8,pool+18)
        if ((x-63)/29)**2+((y-pool-3-x*.05)/9)**2<1:
            draw.line((x,y,x+rng.randrange(1,7),y+rng.choice((0,0,1))),
                      fill=rng.choice(((112,169,172,180),(173,207,199,222),(203,222,210,241))))
    for points in [[(35,pool+13),(42,pool+16),(55,pool+17),(67,pool+16)],
                   [(74,pool+15),(88,pool+13),(99,pool+10)],
                   [(46,pool+23),(58,pool+24),(73,pool+23),(84,pool+21)]]:
        draw.line(points,fill=(139,185,181,127))
    im.save(output/"outlet-fall.png",optimize=True)
    return {"asset":"outlet-fall","x":x0,"y":y0,"width":2*width,"height":2*height,
            "flow":[x0+98,y0+113,45,drop*2+13],"pool":[x0+126,y0+2*(pool+4)],
            "drop":2*drop,"broken":1}
