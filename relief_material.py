"""Local V6.4.4 material studies over the V6.4.3 relief tiles."""
import numpy as np
from PIL import Image

from generate_relief_demos import GRID, pixel
from relief_raster import smooth

SITES = {"mountain": ((15, 70), (8, 7)), "canyon": ((81, 66), (8, 6))}


def facets(u, v, scale, seed):
    """Return irregular planes and their distances to the nearest fault."""
    u, v = u/scale, v/scale
    i, j = np.floor(u), np.floor(v)
    near = np.full_like(u, np.inf)
    second = near.copy()
    tone = np.zeros_like(u)
    for dy in (-1, 0, 1):
        for dx in (-1, 0, 1):
            k, l = i+dx, j+dy
            a = np.sin(17.83*k+41.73*l+seed)*43758.5453
            b = np.sin(37.91*k-19.81*l+seed)*8533.235
            px, py = k+.12+.76*(a%1), l+.12+.76*(b%1)
            distance = (u-px)**2+(v-py)**2
            wins = distance < near
            second = np.where(wins, near, np.minimum(distance, second))
            near = np.minimum(near, distance)
            tone = np.where(wins, (a+b)%1, tone)
    return tone, np.sqrt(second)-np.sqrt(near)


def material(scene, x, y, z, slope, light, strata, grit, salt):
    rock = smooth((abs(z)-2)/19)
    steep = smooth((slope-.32)/1.15)
    u = x+z*.29+(strata-.5)*6
    v = y*.44+z*.69
    slab, edge = facets(u, v, 30, 62)
    chips, chip_edge = facets(u-v*.28, v, 9, 43)
    weathered = smooth((strata+.25*slab-.43)/.33)
    bedding = (v*.11 + u*.024 + .64*slab + .31*np.sin(u*.072)) % 1
    seam = np.where(bedding < .085, -21, np.where(bedding > .89, 11, 0))
    fracture = np.where((edge < .053) & (slab>.33), -32, 0)
    flake = np.where((chip_edge < .089) & (chips>.63), -16, 0)
    contrast = (slab-.5)*35 + (chips-.5)*15 + fracture + flake + seam*weathered
    grit_detail = np.where(salt>.87, 11, np.where(salt<.16, -13, 0))
    if scene.name == "mountain":
        floor = np.array([105,119,98])
        stone = np.array([126,129,124])+steep[...,None]*[-13,-10,-6]
        drift = z+2.5*(y/GRID+scene.origin[1]-68)-57+30*(strata-.5)
        crust = drift+24*(slab-.5)+8*(chips-.5)-21*steep
        snow = smooth(crust/11)*(1-.91*smooth((slope-.84)/1.05))
        snow_color = np.array([198,214,218])+(chips-.5)[...,None]*[19,17,13]
        stone = stone+(snow_color-stone)*snow[...,None]
        detail = contrast*rock*(1-.77*snow)+grit_detail*.55
        detail += np.where((crust>0)&(crust<5)&(snow>.1), 10, 0)
    else:
        depth = smooth(-z/108)
        floor = np.array([179,133,78])
        stone = np.array([177,108,67])+steep[...,None]*[-22,-13,-6]
        stone += (np.array([62,48,52])-stone)*(depth*.85)[...,None]
        ripple = np.sin(.24*y+.062*x+3.2*strata+2*grit)
        sand = (grit-.5)*13+grit_detail*.62+np.where(ripple>.77, 5, 0)
        detail = contrast*rock+sand*(1-rock)+(grit-.5)*7*rock
    color = floor+(stone-floor)*rock[...,None]
    shade = np.round(np.minimum(light,1.16)*20)/20
    return np.clip((color+(np.round(detail/3)*3)[...,None])*shade[...,None],0,255)


def write_study(scene, view, lighting, strata, grit, salt, prior, output, shapes=None):
    """Write only intersecting tiles and reuse cached pixels at the edges."""
    point, size = SITES[scene.name]
    cx, cy = pixel(scene, point)
    rx, ry = (n*GRID for n in size)
    height,width = view.opacity.shape
    left, top, right, bottom = max(0,cx-rx),max(0,cy-ry),min(width,cx+rx),min(height,cy+ry)
    y,x = np.mgrid[top:bottom,left:right].astype(np.float32)
    region = np.s_[top:bottom,left:right]
    sy = np.maximum(view.source_y[region],0)
    sx = x.astype(int)
    z = view.visible_height[region]
    slope = view.slope[region]
    radius = np.hypot((x-cx)/rx,(y-cy)/ry)
    blend = smooth((1-radius)/(.18 if shapes else .28))
    blend *= smooth((view.opacity[region]-.04)/.24)
    if scene.name == "mountain":
        # Keep the camp, treads and exposed walking surface from V6.4.3.
        blend *= smooth((abs(z)-3)/9)
    painter = shapes.material if shapes else material
    light = lighting[sy,sx]
    color = painter(scene,x,sy,z,slope,light,strata[sy,sx],grit[sy,sx],salt[sy,sx])
    finish = output.name == "relief_v6_4_7"
    wall_study = output.name in {"relief_v6_4_6","relief_v6_4_7"}
    overlay = (shapes.details(scene,view,(left,top,right,bottom),color,light,blend)
               if wall_study else None)
    opacity = np.ones_like(blend)
    if scene.name == "canyon":
        # Let the original dappled desert through where there are no rock or
        # contact shadows. This follows the actual material and local light.
        ground = (1-smooth(abs(z)/8))*smooth((lighting[sy,sx]-.94)/.22)
        if finish:
            # Re-establish a common fine-grained material before feathering.
            flat = (1-smooth(abs(z)/11))*(1-smooth((slope-.19)/.95))
            sand_alpha = view.opacity[region]*(1-.19*flat)*255
        elif wall_study:
            # Keep a common sand substrate, including under soft shadows.
            taper = smooth((1-radius)/.53)
            flat = (1-smooth(abs(z)/9))*(1-smooth((slope-.12)/.86))
            opacity -= flat*taper*(.24+.70*smooth((light-.87)/.26))
            blend *= 1-.91*ground
        elif shapes:
            # In unshadowed sand keep the cached grain and colour. Changes
            # to slopes and their shadows taper separately into that sand.
            blend *= 1-.91*ground
        else:
            opacity -= .53*ground*blend
    for ty in range(top//128*128,bottom,128):
        for tx in range(left//128*128,right,128):
            name = f"{scene.name}-lit-{tx}-{ty}.png"
            path = prior/name
            if shapes:
                versions = ("relief_v6_4_6","relief_v6_4_5","relief_v6_4_4") if finish else (
                    ("relief_v6_4_5","relief_v6_4_4") if wall_study else ("relief_v6_4_4",))
                for version in versions:
                    cached = prior.with_name(version)/name
                    if cached.exists():
                        path = cached
                        break
            if not path.exists():
                continue
            with Image.open(path) as old:
                tile = old.copy()
                a,b,c,d = max(left,tx),max(top,ty),min(right,tx+tile.width),min(bottom,ty+tile.height)
                if a>=c or b>=d:
                    continue
                section = np.s_[b-top:d-top,a-left:c-left]
                patch = np.asarray(tile.crop((a-tx,b-ty,c-tx,d-ty)),dtype=np.float32).copy()
                patch[:,:,:3] += (color[section]-patch[:,:,:3])*blend[section][...,None]
                if finish and scene.name == "canyon":
                    patch[:,:,3] += (sand_alpha[section]-patch[:,:,3])*blend[section]
                else:
                    patch[:,:,3] *= opacity[section]
                updated = Image.fromarray(np.uint8(np.round(patch)))
                if overlay is not None:
                    updated.alpha_composite(overlay.crop((a-left,b-top,c-left,d-top)))
                tile.paste(updated,(a-tx,b-ty))
                if old.tobytes() != tile.tobytes():
                    tile.save(output/name,optimize=True)
