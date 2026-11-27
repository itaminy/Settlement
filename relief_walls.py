"""Local V6.4.6 wall treatments on the V6.4.5 height field."""
import numpy as np
from PIL import Image, ImageDraw

import relief_art
import relief_shapes
from generate_relief_demos import pixel
from relief_raster import smooth


def bands(x, y, z):
    return (z*.075+x*.018+y*.009+.29*np.sin(x*.061+y*.034))%1


def shape(scene, x, y, height):
    base = relief_shapes.shape(scene,x,y,height)
    dy,dx = np.gradient(height)
    wall = smooth((np.hypot(dx,dy)-.47)/1.25)
    radius = relief_shapes.extent(scene,x,y)
    local = smooth((.94-radius)/.34)
    # Low shelves add several projected pixels without changing the ridge tops.
    phase = bands(x,y,base)
    ledge = np.maximum(0,1-abs(phase-.32)/.25)
    breakup = smooth((np.sin(x*.098+y*.055)+.38)/1.45)
    wall *= np.where(base>0,1-smooth((base-44)/39),1)
    relief = (ledge*(3+6*breakup)-2.9)*wall*local*smooth(abs(base)/8)
    raised = np.where(base>0,np.maximum(.001,base+relief),
                      np.where(base<0,np.minimum(-.001,base+relief),0))
    return np.where(relief!=0,raised,base)


def material(scene, x, y, z, slope, light, strata, grit, salt):
    color = relief_shapes.material(scene,x,y,z,slope,light,strata,grit,salt)
    slope_weight = smooth((slope-.45)/1.2)*smooth((abs(z)-3)/17)
    phase = bands(x,y,z)
    chipped = smooth((strata+.28*np.sin(x*.122+y*.048)-.28)/.48)
    layers = np.select([phase<.13,phase<.28,phase>.81],[-18,-7,16],default=3)
    color += (layers*chipped*slope_weight*light)[...,None]
    if scene.name == "mountain":
        bright = smooth((color[...,1]-132)/57)*smooth((z-22)/41)
        matte = np.stack([color[...,0]*.72+47,color[...,1]*.72+47,
                          color[...,2]*.69+47],axis=-1)
        matte += ((np.floor(grit*6)-2.5)*2.6)[...,None]
        color += (matte-color)*bright[...,None]
    return np.clip(color,0,255)


def details(scene, view, bounds, color, light, blend):
    """Return transparent face details, talus and tread nosings at atlas scale."""
    left,top,right,bottom = bounds
    layer = Image.new("RGBA",(right-left,bottom-top))
    draw = ImageDraw.Draw(layer)
    region = np.s_[top:bottom,left:right]
    height = view.visible_height[region]
    steep = view.slope[region]
    # Track the contact line rather than scattering debris across paths.
    edge = (abs(height)<8) & (steep>.45) & (blend>.64)
    points = np.argwhere(edge)
    rng = np.random.default_rng(646)
    if len(points):
        for i in rng.choice(len(points),min(320,len(points)),replace=False):
            y,x = points[i]
            y += rng.integers(0,5)
            r = int(rng.choice((1,2,2,3,4)))
            if not (r+1<x<layer.width-r-1 and r+1<y<layer.height-r-1):
                continue
            if blend[y,x]<.64 or abs(height[y,x])>10:
                continue
            tone = color[y,x]
            shadow = tuple(np.uint8(np.clip(tone-32,0,255)))
            face = tuple(np.uint8(np.clip(tone+12,0,255)))
            draw.polygon([(x-r,y),(x-r//2,y-r//2),(x+r,y-1),
                          (x+r,y+1),(x,y+2)],fill=(*shadow,180))
            draw.polygon([(x-r,y),(x-r//2,y-r//2),(x+r,y-1),(x,y+1)],fill=(*face,220))
    if scene.name == "canyon":
        # Reapply the V6.4.3 opening to the newly projected wall, using its palette.
        wall = Image.new("RGBA",view.opacity.shape[::-1])
        wall.paste(Image.fromarray(np.uint8(color)),(left,top))
        relief_art.cave(wall,view,scene,overlay=layer,offset=(left,top))
    else:
        x,y = pixel(scene,(14,68))
        x,y = x-left,y-top+4
        for n in range(-2,3):
            u = x+n*5
            if not (14<u<layer.width-14 and 14<y<layer.height-14 and abs(height[y,u])<3):
                continue
            half = (9,10,11,10,8)[n+2]
            shade = light[y,u]
            lip = tuple(np.uint8(np.clip(np.array([157,153,138])*shade,0,255)))
            riser = tuple(np.uint8(np.clip(np.array([82,83,77])*shade,0,255)))
            draw.polygon([(u-2,y-half),(u+2,y-half+1),(u+2,y+half),
                          (u-1,y+half+1),(u,y+1)],fill=(*riser,255))
            draw.line([(u-2,y-half),(u-1,y),(u-2,y+half)],fill=(*lip,255),width=2)
            draw.point((u,y+n),fill=(*lip,180))
    return layer
