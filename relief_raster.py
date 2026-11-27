"""Offline 2.5D height-field rasterizer for 2-pixel Settlement terrain art."""
from dataclasses import dataclass

import numpy as np

PROJECTION = .78
LIGHT = np.array((-.58, -.66, .70), dtype=np.float32)
LIGHT /= np.linalg.norm(LIGHT)


def smooth(value):
    t = np.clip(value, 0, 1)
    return t*t*(3-2*t)


def displaced(field, dx, dy):
    """Sample around a point, clamping at the outer edge of the atlas."""
    height,width = field.shape
    rows = np.clip(np.arange(height)+dy,0,height-1)
    cols = np.clip(np.arange(width)+dx,0,width-1)
    return field[np.ix_(rows,cols)]


def illumination(height, occluders):
    """Diffuse light, soft horizon shadow and contact occlusion at each receiver."""
    dy,dx = np.gradient(height)
    normals = (-dx*LIGHT[0] - dy*LIGHT[1] + LIGHT[2])/np.sqrt(1+dx*dx+dy*dy)
    horizon = np.zeros_like(height)
    for n in (2,4,7,11,16,23,32,44,60,81,106,137,176):
        blockers = displaced(occluders,-round(n*.65),-round(n*.76))
        slack = blockers-height-n*.73
        horizon = np.maximum(horizon,smooth((slack+3.8)/(7+n*.065)))
    surround = sum(np.maximum(0,displaced(height,dx,dy)-height)
                   for dx,dy in [(-13,0),(13,0),(0,-13),(0,13),(-9,-9),(9,9)])
    contact = smooth(surround/99)
    return np.clip(.84+.47*normals-.28*horizon-.20*contact,.44,1.25)


@dataclass(frozen=True)
class View:
    rgb: np.ndarray
    visible_height: np.ndarray
    slope: np.ndarray
    opacity: np.ndarray
    source_y: np.ndarray


def project(height, paint, opacity, slope, perspective=PROJECTION, projected_height=None):
    """Project strips north to south. Later surfaces win each orthographic sightline."""
    rows,cols = height.shape
    world_y = np.arange(rows,dtype=np.float32)[:,None]
    grid = world_y-(height if projected_height is None else projected_height)*perspective
    target = np.zeros((rows,cols,3),dtype=np.uint8)
    source_y = np.full((rows,cols),-1,dtype=np.int16)
    zbuffer = np.zeros_like(height)
    face = np.zeros_like(height)
    alpha = np.zeros_like(height)
    for row in range(rows-1):
        a,b = grid[row],grid[row+1]
        low = np.floor(np.minimum(a,b)).astype(int)
        high = np.ceil(np.maximum(a,b)).astype(int)
        for offset in range(int((high-low).max())+1):
            y = low+offset
            x = np.flatnonzero((y<=high)&(y>=0)&(y<rows))
            yy = y[x]
            if not len(x):
                continue
            # Interpolate height and wall bedding between adjoining terrain samples.
            t = np.clip((yy-a[x])/np.where(abs(b[x]-a[x])<.001,.001,b[x]-a[x]),0,1)
            z = height[row,x]+(height[row+1,x]-height[row,x])*t
            steep = np.maximum(slope[row,x],slope[row+1,x])
            valid = np.maximum(opacity[row,x],opacity[row+1,x])>.002
            yy,x,t,z,steep = yy[valid],x[valid],t[valid],z[valid],steep[valid]
            target[yy,x] = paint(row,x, z, steep, t)
            zbuffer[yy,x] = z
            face[yy,x] = steep
            alpha[yy,x] = opacity[row,x]
            source_y[yy,x] = row
    return View(target,zbuffer,face,alpha,source_y)
