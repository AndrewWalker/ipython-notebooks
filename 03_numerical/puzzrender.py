import matplotlib.pyplot as plt
import matplotlib.patches as patches
import numpy as np
import math
from pylab import rcParams
rcParams['figure.figsize'] = 10, 10

def equilateral_triangle(b = 1.0, flip=False):
    if flip:
        hgt = math.sqrt(b**2- (b/2)**2)
        pts = [[-b/2, hgt],[b/2,hgt],[0,0]] 
        return np.array(pts)
    else:
        hgt = math.sqrt(b**2- (b/2)**2)
        pts = [[-b/2, 0],[b/2,0],[0,hgt]] 
        return np.array(pts)

def circle_locs(pts, flip=False):
    v0 = pts[0,:]
    v1 = pts[1,:]
    v2 = pts[2,:]
    e01_mid = (v1+v0)/2
    e12_mid = (v2+v1)/2
    e20_mid = (v0+v2)/2
    mid = (v0+v1+v2)/3
    a = (e01_mid + mid)/2
    c = (e12_mid + mid)/2
    b = (e20_mid + mid)/2
    if flip:
        return (a, c, b)
    else:
        return (a, b, c)

def plot_patch(ax, colors, offset, flip=False):
    if flip:
        pts = equilateral_triangle(flip=True)
        pts = pts + offset
        c1, c2, c3 = circle_locs(pts,flip=True)
    else:
        pts = equilateral_triangle(flip=False)
        pts = pts + offset
        c1, c2, c3 = circle_locs(pts,flip=False)
    ax.add_patch(patches.Polygon(xy=pts, facecolor='brown', edgecolor='black'))
    ax.add_patch(patches.Circle(xy=c1, radius = 0.1, color=colors[0]))
    ax.add_patch(patches.Circle(xy=c2, radius = 0.1, color=colors[1]))
    ax.add_patch(patches.Circle(xy=c3, radius = 0.1, color=colors[2]))

tile_offsets = [
    (3, 3, 0),
    
    (2.5, 2, 0),
    (3.0, 2, 1),
    (3.5, 2, 0),
    
    (2, 1, 0),
    (2.5, 1, 1),
    (3, 1, 0),
    (3.5, 1, 1),
    (4, 1, 0),
    
    (1.5, 0, 0),
    (2, 0, 1),
    (2.5, 0, 0),
    (3, 0, 1),
    (3.5, 0, 0),
    (4, 0, 1),
    (4.5, 0, 0)
]


y = math.sqrt(3)/2
wiggle = math.sqrt(3)*9/24
boundary_offsets = [

    [2.625, 0+7*y/2+wiggle],    
    [2.125, 0+5*y/2+wiggle],
    [1.625, 0+3*y/2+wiggle],
    [1.125, 0+1*y/2+wiggle],
 
    [6-2.625, 7*y/2+wiggle],     
    [6-2.125, 5*y/2+wiggle],
    [6-1.625, 3*y/2+wiggle],
    [6-1.125, 1*y/2+wiggle],  

    [1.5, y/2],
    [2.5, y/2],
    [3.5, y/2],
    [4.5, y/2],
]

def plot_board(tris, boundaries):

    p1p = equilateral_triangle(6) 
    p1p = p1p + np.array([3, 0])
    p1 = patches.Polygon(xy=p1p, color='grey')

    p2p = equilateral_triangle(4)
    vec = np.array([3, math.sqrt(3)/3])
    p2p = p2p + vec
    p2 = patches.Polygon(xy=p2p, color='white')


    ax = plt.gca()
    ax.add_patch(p1)
    ax.add_patch(p2)
    
    for i, (dx, dy) in enumerate(boundary_offsets):
        ax.add_patch(patches.Circle(xy=[dx,dy], radius = 0.1, color = boundaries[i]))

    for i, (dx, dy, flip) in enumerate(tile_offsets):
        plot_patch(ax, tris[i], np.array([dx, (dy*y)+math.sqrt(3)/3]), bool(flip))

    ax.set_xlim(0,6)
    ax.set_ylim(-0.5,5.5)


