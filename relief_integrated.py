"""V6.4.8 finishing details shared across the existing relief regions."""
import numpy as np
from PIL import Image, ImageDraw

import relief_finish
from generate_massive_mountain import geometry as mountain_plan
from generate_relief_demos import pixel
from relief_raster import smooth

shape = relief_finish.shape
material = relief_finish.material


def details(scene, view, canvas, light, land):
    height,width = view.opacity.shape
    shaded = np.asarray(canvas,dtype=np.float32)[:,:,:3]
    overlay = relief_finish.details(scene,view,(0,0,width,height),shaded,light,
                                    smooth((view.opacity-.33)/.57),draw_stairs=False)
    if scene.name == "mountain":
        draw = ImageDraw.Draw(overlay)
        trail = mountain_plan()[0]
        for point in [(14,68),(8,74),(14,80),(16,82),(11,85),(12,87)]:
            if point not in trail or point in (trail[0],trail[-1]):
                continue
            i = trail.index(point)
            a,b = trail[i-1],trail[i+1]
            if a[0]!=b[0] and a[1]!=b[1]:
                continue
            x,y = pixel(scene,point)
            stairs(draw,x,y+5,a[0]!=b[0],light[y,x])
    overlay.putalpha(Image.fromarray(
        np.uint8(np.asarray(overlay.getchannel("A"))*smooth((land-.12)/.78))))
    canvas.alpha_composite(overlay)


def stairs(draw, x, y, horizontal, light):
    """Draw contiguous slabs with a visible tread and a lower riser."""
    def place(u,v):
        return (x+u,y+v) if horizontal else (x+v,y+u)

    def slab(points, color):
        draw.polygon([place(a,b) for a,b in points],fill=(*color,255))

    stone = np.array([128,125,112])*min(float(light),1.14)
    for n,(near,far) in enumerate([(7,7),(8,8),(9,9),(9,8),(8,6)]):
        u = 12-n*6
        rise = n*2-4
        tone = stone+(n%3-1)*6
        # Exposed southern edge and dark east-facing riser under each tread.
        slab([(u-5,near+rise-3),(u+3,near+rise-5),(u+4,near+rise+1),
              (u-3,near+rise+3)],relief_finish.tint(tone,-30))
        slab([(u+1,-far+rise),(u+4,-far+rise+3),(u+4,near+rise+1),
              (u+2,near+rise)],relief_finish.tint(tone,-17))
        slab([(u-5,-far+rise+2),(u+1,-far+rise),(u+2,near+rise-2),
              (u-4,near+rise)],relief_finish.tint(tone,12))
        draw.line([place(u-4,near+rise-1),place(u,near+rise-2),place(u+2,near+rise-3)],
                  fill=(*relief_finish.tint(tone,26),255))
        draw.point(place(u-2,rise+(n*7)%9-4),fill=(*relief_finish.tint(tone,-8),255))
