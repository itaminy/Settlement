"""V6.4.3 surface art for the existing relief projection."""
import numpy as np
from PIL import Image, ImageDraw

from generate_massive_mountain import geometry as mountain_plan
from generate_relief_demos import GRID, pixel
from relief_raster import smooth


def pigment(scene, x, y, z, face, light, strata, faults, grit, salt):
    wall = smooth((face-.26)/1.2)
    rock = smooth((abs(z)-3)/24)
    # Bend the underlying bedding with the same faults at all scales.
    warped = z + 9*(strata-.5) + 4*np.sin(x*.013+strata*3)
    bed = (warped*.084 + x*.009 + .19*np.sin(x*.041+z*.026)) % 1
    layers = np.select([bed < .11, bed < .30, bed > .86], [-22, -9, 15], default=3)
    bare_bands = smooth((strata+.29*faults-.43)/.34)
    profile = abs(np.sin(x*.053+2.8*faults+z*.024+.23*np.sin(z*.21)))
    splits = np.where((profile < .095) & (faults > .44), -32, 0)
    splinters = np.where((profile < .16) & (faults > .44), 9, 0)
    tonal_planes = np.round((strata-.5)*8)*6 + (faults-.5)*19
    weathering = (layers*bare_bands + splits + splinters)*(.27+.73*wall) + tonal_planes
    if scene.name == "mountain":
        floor = np.array([100,117,91]) + smooth((y-65)/17)[...,None]*[35,29,34]
        stone = np.array([125,126,118]) + wall[...,None]*[-19,-15,-11]
        snowline = z + 2.55*(y-68) - 58 + 34*(strata-.5) + 12*(faults-.5) + 8*(grit-.5)
        snow = smooth(snowline/5)*smooth((1.65-face)/.87)
        snow_color = np.array([197,211,214]) + (faults-.52)[...,None]*[32,26,19]
        frost_edge = (snowline > -3) & (snowline <= 2) & (face < 1.25)
        stone = stone + (snow_color-stone)*snow[...,None]
        color = floor+(stone-floor)*rock[...,None]
        detail = weathering*rock*(1-.87*snow) + frost_edge*15 + (grit-.5)*10
        detail += np.where((salt>.93) & (snow>.9), 7, 0)
        shade = np.minimum(1.13,light + (1-light)*wall*.13)
    else:
        depth = smooth(-z/109)
        floor = np.array([188,138,87])
        stone = np.array([178,108,64])+wall[...,None]*[-15,-13,-5]
        color = floor+(stone-floor)*rock[...,None]
        color += (np.array([62,49,55])-color)*(depth*.87)[...,None]
        detail = weathering*rock + (grit-.5)*(3+5*rock)
        shade = (light+(1-light)*wall*.13)*(1-.19*depth)
    luma = np.round(detail/3)*3
    return np.uint8(np.clip((color+luma[...,None])*shade[...,None],0,255))


def steps(canvas, view, scene):
    trail = mountain_plan()[0]
    draw = ImageDraw.Draw(canvas)
    for point in [(13,63),(14,68),(8,74),(14,80),(16,82),(11,85),(12,87)]:
        if point not in trail:
            continue
        i = trail.index(point)
        ax, ay = trail[i-1]
        bx, by = trail[i+1]
        if ax != bx and ay != by:
            continue
        x, y = pixel(scene,point)
        horizontal = ax != bx
        for k in range(-2,3):
            u, v = (x+k*5,y+4) if horizontal else (x,y+4+k*5)
            if view.opacity[v,u]<.9 or abs(view.visible_height[v,u])>2:
                continue
            length = (9,11,10,12,8)[k+2]
            color = canvas.getpixel((u,v))
            light = tuple(min(255,c+30) for c in color[:3])
            dark = tuple(max(0,c-43) for c in color[:3])
            if horizontal:
                lip = [(u-2,v-length),(u,v-2),(u-1,v+length)]
                edge = [(a+1,b) for a,b in lip]
            else:
                lip = [(u-length,v+1),(u-1,v),(u+length,v+1)]
                edge = [(a,b-1) for a,b in lip]
            draw.line(lip,fill=(*dark,color[3]),width=3)
            draw.line(edge,fill=(*light,color[3]),width=2)


def cave(canvas, view, scene, overlay=None, offset=(0,0)):
    # Find a face with room for the aperture and surrounding broken courses.
    cx,cy = pixel(scene,(82.2,65.6))
    face = (view.slope>.76) & (view.visible_height>7) & (view.opacity>.9)
    score,x,y = max((float(face[v-21:v+21,u-22:u+22].mean()),u,v)
                    for u in range(cx-48,cx+49,4)
                    for v in range(max(24,cy-51),cy+41,3))
    if score < .36:
        return
    portal = Image.new("RGBA",canvas.size)
    draw = ImageDraw.Draw(portal)
    palette = canvas.getpixel((x,y))[:3]

    def tint(delta):
        return tuple(max(0,min(255,c+delta)) for c in palette)

    def polygon(points,color):
        draw.polygon([(x+a,y+b) for a,b in points],fill=color)

    polygon([(-37,16),(-29,3),(-23,-17),(-10,-24),(12,-21),
             (27,-13),(35,15),(18,22),(-16,21)],tint(-17))
    polygon([(-36,11),(-27,-1),(-22,-18),(-11,-23),(10,-21),(22,-15),
             (15,-15),(5,-17),(-9,-15),(-16,-8),(-26,11)],tint(19))
    polygon([(-23,15),(-18,-4),(-11,-13),(2,-16),(13,-12),
             (22,-4),(26,14),(13,18),(-9,18)],(45,30,30))
    polygon([(-14,12),(-11,-4),(-3,-10),(7,-11),(15,-4),(20,13),
             (8,15)],(29,24,28))
    polygon([(-25,15),(-10,13),(10,14),(24,12),(34,17),(21,23),
             (-16,22),(-31,20)],tint(-3))
    polygon([(-30,18),(-19,16),(-10,17),(-22,19)],tint(15))
    polygon([(18,-14),(27,-10),(32,4),(28,2),(24,-6)],tint(-25))
    alpha = np.asarray(portal.getchannel("A"),dtype=np.float32)
    portal.putalpha(Image.fromarray(np.uint8(alpha*smooth((view.visible_height-2)/8)*view.opacity)))
    if overlay is None:
        canvas.alpha_composite(portal)
    else:
        u,v = offset
        overlay.alpha_composite(portal.crop((u,v,u+overlay.width,v+overlay.height)))


def details(canvas, view, scene, rng):
    draw = ImageDraw.Draw(canvas)
    mountain = scene.name == "mountain"
    # Cluster angular talus at low cliff contacts, with quiet sand between walls.
    candidates = np.argwhere((view.opacity>.96) &
                             (abs(view.visible_height)<7) & (view.slope>.24))
    if len(candidates):
        for _ in range(480 if mountain else 340):
            y,x = map(int,candidates[rng.randrange(len(candidates))])
            x += rng.randrange(-7,8)
            y += rng.randrange(0,9)
            size = rng.choice((1,2,2,3,3,4,5))
            if not (size<x<canvas.width-size and size<y<canvas.height-size):
                continue
            area = np.s_[y-size:y+size+1,x-size:x+size+1]
            if np.min(view.opacity[area])<.94 or np.max(abs(view.visible_height[area]))>10:
                continue
            r,g,b,a = canvas.getpixel((x,y))
            dark = tuple(max(0,c-28) for c in (r,g,b))
            light = tuple(min(255,c+rng.randrange(9,31)) for c in (r,g,b))
            draw.polygon([(x-size,y),(x-1,y-size//2),(x+size,y-1),
                          (x+size,y+size//2),(x,y+size//2+1)],fill=(*dark,a))
            draw.polygon([(x-size,y),(x-1,y-size//2),(x+size,y-1),(x,y+1)],fill=(*light,a))
    if mountain:
        steps(canvas,view,scene)
    else:
        cave(canvas,view,scene)
