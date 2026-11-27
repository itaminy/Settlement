"""V6.4.5 visual height and materials, confined to the V6.4.4 study sites."""
import numpy as np

from generate_relief_demos import GRID, pixel
from relief_material import SITES, facets
from relief_raster import smooth


def extent(scene, x, y):
    center, radii = SITES[scene.name]
    cx, cy = pixel(scene, center)
    rx, ry = (n*GRID for n in radii)
    return np.hypot((x-cx)/rx,(y-cy)/ry)


def shape(scene, x, y, height):
    """Cut shelves and spurs inside the original positive and negative masks."""
    local = smooth((.82-extent(scene,x,y))/.27)
    if not np.any(local):
        return height
    u = (x+.22*y)[local>0]
    v = (y-.19*x)[local>0]
    z = height[local>0]
    planes,edge = facets(u,v,43,45)
    knuckles,small_edge = facets(u-v*.36,v,19,55)
    planes = (planes-.5)*smooth(edge/.17)
    knuckles = (knuckles-.5)*smooth(small_edge/.19)
    # Asymmetric, oblique benches truncate the otherwise round upper surface.
    phase = (z+.14*u+9*planes)/25%1
    profile = np.clip((abs(phase-.5)-.24)*4,-.7,.7)
    cut = planes*16+knuckles*5+profile*3
    weight = smooth(abs(z)/22)*local[local>0]
    if scene.name == "canyon":
        cut = np.where(z<0,cut*.78-9*smooth(-z/65),cut)
    result = height.copy()
    result[local>0] = np.where(z>0,np.maximum(.01,z+cut*weight),
                               np.where(z<0,np.minimum(-.01,z+cut*weight),0))
    return result


def material(scene, x, y, z, slope, light, strata, grit, salt):
    rock = smooth((abs(z)-1)/16)
    steep = smooth((slope-.34)/1.2)
    u = x+.23*z+.22*y
    v = .49*y+.67*z
    slab,_ = facets(u,v,38,62)
    chips,_ = facets(u-.33*v,v,11,43)
    # Open, eroded fissures. Each family ends independently along the rock.
    diagonal = abs(np.sin(u*.054 + v*.032 + np.sin(v*.108)*.23 + strata*.8))
    cross = abs(np.sin(u*.113 - v*.049 + np.sin(u*.143)*.18))
    gaps = smooth((np.sin(v*.106+slab*4)+strata-.64)/.34)
    hairlines = smooth((np.cos(u*.156+chips*3)+grit-.96)/.32)
    fissures = -13*(diagonal<.085)*gaps - 7*(cross<.072)*hairlines
    bedding = (v*.094 + u*.012 + .34*np.sin(u*.066) + .43*strata)%1
    layers = np.where(bedding<.105,-13,np.where(bedding>.88,9,0))
    material_detail = (slab-.5)*23+(chips-.5)*11+fissures
    material_detail += layers*smooth((strata+.31*slab-.46)/.32)
    dust = np.where(salt>.88,7,np.where(salt<.16,-8,0))+(grit-.5)*11
    if scene.name == "mountain":
        floor = np.array([106,121,101])
        stone = np.array([128,133,132])+steep[...,None]*[-16,-13,-9]
        recess = np.cos(v*.112+strata*3.1)*5
        drift = z + 2.4*(y/GRID+scene.origin[1]-68)-59+29*(slab-.5)+recess-24*steep
        snow = smooth(drift/12)*(1-.91*smooth((slope-.68)/1.15))
        snow_color = np.array([193,210,220])+(chips-.5)[...,None]*[19,15,12]
        stone += (snow_color-stone)*snow[...,None]
        detail = material_detail*rock*(1-.81*snow)+dust*.65
        detail += 9*((drift>1)&(drift<6)&(snow>.1))
    else:
        depth = smooth(-z/111)
        floor = np.array([183,139,86])
        stone = np.array([177,111,71])+steep[...,None]*[-21,-12,-7]
        stone += (np.array([62,47,50])-stone)*(depth*.87)[...,None]
        # Broken wedges of rubble where the wall meets the descending bed.
        scree = (z<0)*(1-steep)*smooth((-z-6)/26)*(1-.68*depth)
        rubble = np.where(chips>.70,16,np.where(chips<.31,-12,0))
        detail = material_detail*rock + dust + scree*rubble
    color = floor+(stone-floor)*rock[...,None]
    shade = np.round(np.minimum(light,1.19)*24)/24
    return np.clip((color+(np.round(detail/3)*3)[...,None])*shade[...,None],0,255)
