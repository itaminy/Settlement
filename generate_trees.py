"""Draw Settlement trees at native resolution with Pillow.

Run from any directory: python generate_trees.py
"""
from math import cos, hypot, pi, sin
from pathlib import Path
from random import Random

from PIL import Image, ImageDraw

ROOT = Path(__file__).resolve().parent
OUTPUT = ROOT / "resources/images/nature_v3"
PREVIEWS = ROOT / "docs/previews"
SIZE = 128

BARK = ("#332725", "#564031", "#79563a", "#a77a4c", "#c29761")
SHADED_BARK = ("#282823", "#383329", "#484033", "#61513b", "#785b40")
BIRCH = ("#45443c", "#77756b", "#aaa899", "#d8d4bb", "#f5e7ca")
DRY = ("#3b3130", "#594941", "#7a6354", "#9e846b", "#bca080")
PINE = ("#19352e", "#254b38", "#376249", "#547e51", "#7b9a54")
SPRUCE = ("#172f2d", "#20453c", "#326050", "#497c62", "#73a17a")
LIME = ("#293e29", "#3c582a", "#587735", "#7d9843", "#a6b857")
OAK = ("#203728", "#324d2b", "#4a6934", "#6c863e", "#93a64d")
AUTUMN = ("#482d2a", "#763d2a", "#a7532c", "#cc7234", "#e99e43")


def stem(draw, points, palette, rng):
    """Paint tapered bark facets along a polyline of (x, y, radius)."""
    edges = []
    for i, (x, y, r) in enumerate(points):
        a = points[max(0, i - 1)]
        b = points[min(len(points) - 1, i + 1)]
        dx, dy = b[0] - a[0], b[1] - a[1]
        length = hypot(dx, dy)
        nx, ny = -dy / length, dx / length
        edges.append([(round(x + nx * r * step), round(y + ny * r * step))
                      for step in (-1, -.78, -.34, .19, .71, 1)])
    for band, shade in enumerate((0, 3, 2, 1, 0)):
        for a, b in zip(edges, edges[1:]):
            draw.polygon([a[band], b[band], b[band + 1], a[band + 1]], palette[shade])
    for (x, y, r), (tx, ty, tr) in zip(points, points[1:]):
        for _ in range(int(hypot(tx - x, ty - y) / 4)):
            t = rng.random()
            radius = r + (tr - r) * t
            if radius < 2:
                continue
            px = round(x + (tx - x) * t - radius * rng.uniform(.2, .65))
            py = round(y + (ty - y) * t)
            if palette == BIRCH:
                draw.line((px, py, px + round(radius * .8), py + 1), palette[1], 1)
            else:
                draw.line((px, py, px + (tx - x) * .16, py + (ty - y) * .16),
                          palette[rng.choice((1, 4))], 1)


def trunk(draw, palette, rng, width=6):
    for root in [[(63, 113, 4), (54, 117, 2), (48, 121, 0)],
                 [(64, 112, 4), (72, 117, 2), (78, 119, 0)],
                 [(64, 114, 4), (66, 120, 2), (69, 123, 0)]]:
        stem(draw, root, palette, rng)
    stem(draw, [(64, 120, width), (63, 105, width - 1), (64, 91, width - 2),
                (61, 73, width - 3), (65, 50, 2), (63, 24, 0)], palette, rng)


def leaf_shape(draw, cx, cy, rx, ry, palette, level, rng):
    outline = []
    for i in range(24):
        angle = i * pi / 12
        factor = rng.uniform(.84, 1.14)
        outline.append((round(cx + cos(angle) * rx * factor),
                        round(cy + sin(angle) * ry * factor)))
    draw.polygon(outline, palette[level])
    draw.polygon([(round(cx - 1 + (x - cx) * .83),
                   round(cy - 2 + (y - cy) * .68)) for x, y in outline],
                 palette[level + 1])
    for _ in range(round(rx * ry / 9)):
        x, y = rng.uniform(-.75, .75), rng.uniform(-.7, .52)
        if x*x + y*y < .54:
            px, py = round(cx + x*rx), round(cy + y*ry)
            shade = level + (2 if x + y < -.3 else rng.choice((0, 1, 1)))
            draw.polygon([(px - 1, py), (px + 1, py - 1), (px + 3, py),
                          (px + 1, py + 1)], palette[shade])


def deciduous(draw, rng, palette, bark, crowns):
    trunk(draw, bark, rng, 5 if bark == BIRCH else 8)
    # Each lobe sits on a fork. The low forks and the gaps between lobes
    # expose the branching structure below the upper canopy.
    for x, y, rx, ry in crowns:
        side = -1 if x < 64 else 1
        stem(draw, [(63, min(101, y + 43), 4), (63 + (x - 63)*.52, y + 20, 2),
                    (x, y + 2, 1)], bark, rng)
        stem(draw, [(63 + (x - 63)*.52, y + 20, 2),
                    (x + side * rx*.63, y + ry*.48, 0)], bark, rng)
    for x, y, rx, ry in crowns:
        leaf_shape(draw, x, y, rx, ry, palette, 0, rng)
        for ox, oy, scale, light in [(.40, .10, .55, 1), (-.42, .06, .55, 1),
                                     (.02, -.38, .58, 2), (-.37, -.33, .44, 2),
                                     (-.02, .26, .42, 1)]:
            leaf_shape(draw, round(x + ox*rx), round(y + oy*ry),
                       rx*scale, ry*scale, palette, light, rng)


def needle_bough(draw, rng, x, y, tip_x, tip_y, depth, palette, droop):
    """Trace individual sprays around a curved bough and shade its upper face."""
    span = tip_x - x
    top, bottom = [], []
    for i in range(1, 13):
        t = i / 13
        px = x + span * t
        py = y + (tip_y - y) * t + droop * sin(pi*t)
        width = depth * sin(pi*t) ** .65
        for edge, direction in ((top, -1), (bottom, 1)):
            jag = rng.uniform(.63, 1.17)
            advance = span / 13 * rng.uniform(.24, .87)
            edge.extend([(round(px), round(py + direction*width*.57)),
                         (round(px + advance),
                          round(py + direction*width*jag))])
    draw.polygon([(x, y), *top, (tip_x, tip_y), *reversed(bottom)], palette[0])
    draw.polygon([(x, y), *top[1:-1], (tip_x, tip_y),
                  *[(px, round(py*.53 + (y + (tip_y-y)*(px-x)/span)*.47))
                    for px, py in reversed(bottom[1:-2])]], palette[1])
    for i in range(2, 18):
        t = i / 19
        px = x + span*t
        py = y + (tip_y-y)*t + droop*sin(pi*t)
        width = depth*sin(pi*t)
        offset = rng.uniform(-.63, .13)*width
        shade = 4 if offset < -.33*width and span < 0 and i % 3 == 0 else rng.choice((2, 2, 3))
        length = rng.randint(2, 5)
        direction = 1 if span > 0 else -1
        draw.polygon([(round(px), round(py+offset)),
                      (round(px+direction*length), round(py+offset+length*.48)),
                      (round(px+direction*2), round(py+offset+2))], palette[shade])


def conifer(draw, rng, spruce):
    palette = SPRUCE if spruce else PINE
    trunk(draw, BARK, rng, 6 if spruce else 7)
    rows = ([(81, 41, 46), (69, 38, 35), (57, 31, 33), (46, 26, 22),
             (35, 18, 20), (25, 13, 14), (17, 8, 8)] if spruce else
            [(83, 33, 23), (71, 44, 37), (58, 37, 45), (45, 33, 29),
             (34, 22, 23), (24, 12, 15)])
    draw.polygon([(61, 43), (57, 28), (62, 13), (65, 6 if spruce else 11),
                  (69, 25), (73, 39), (68, 63), (71, 78), (62, 92), (55, 71)],
                 palette[1])
    for y, left, right in rows:
        for span in (right, -left):
            reach = abs(span)
            tip_y = y + round(reach * (.39 if spruce else .06)) + rng.randint(-3, 3)
            if reach > 25:
                stem(draw, [(64, y + 9, 2), (64 + span*.6, tip_y + 3, 1),
                            (64 + span, tip_y, 0)], SHADED_BARK, rng)
            needle_bough(draw, rng, 64, y, 64+span, tip_y,
                         max(4, reach*(.22 if spruce else .27)), palette, 3 if spruce else -2)
        needle_bough(draw, rng, 65, y - 4, 54 if y % 2 else 77, y + 8,
                     5, palette, 1)


def dead(draw, rng):
    branches = [
        [(62, 87, 5), (47, 68, 4), (38, 56, 2), (33, 40, 1), (25, 33, 0)],
        [(42, 62, 2), (26, 58, 2), (20, 50, 1), (12, 46, 0)],
        [(34, 48, 2), (39, 38, 1), (36, 26, 0)],
        [(63, 95, 6), (81, 77, 4), (93, 62, 3), (98, 48, 2), (113, 37, 0)],
        [(86, 71, 3), (94, 73, 2), (105, 67, 1), (116, 67, 0)],
        [(95, 55, 2), (89, 42, 1), (92, 28, 0)],
        [(67, 57, 4), (76, 43, 3), (78, 31, 2), (85, 19, 1), (85, 11, 0)],
        [(77, 37, 2), (69, 28, 1), (67, 18, 0)],
        [(61, 75, 5), (57, 55, 4), (52, 41, 3), (55, 28, 2), (49, 15, 0)],
        [(52, 46, 2), (45, 38, 1), (46, 25, 0)],
        [(54, 32, 2), (61, 22, 1), (59, 10, 0)],
        [(64, 104, 6), (78, 99, 4), (86, 91, 2), (88, 85, 2)],
    ]
    trunk(draw, DRY, rng, 9)
    for branch in branches:
        stem(draw, branch, DRY, rng)
    draw.polygon([(86, 86), (88, 83), (90, 86), (88, 90)], DRY[4])
    draw.polygon([(58, 107), (61, 94), (63, 91), (64, 97), (61, 101),
                  (61, 109), (59, 113)], DRY[0])
    draw.line([(57, 108), (57, 103), (59, 97)], DRY[4], 1)


def save_checked(image, path):
    image.save(path)
    with Image.open(path) as saved:
        assert (saved.mode, saved.size) == ("RGBA", (SIZE, SIZE)), path
        alpha = saved.getchannel("A")
        assert alpha.getextrema() == (0, 255), path
        assert sum(alpha.histogram()[1:255]) == 0, path
        left, top, right, bottom = alpha.getbbox()
        margins = (left, top, SIZE-right, SIZE-bottom)
        assert min(margins) >= 4, (path, margins)
        print(f"{path.name}: transparent margins {margins}")


def main():
    OUTPUT.mkdir(parents=True, exist_ok=True)
    PREVIEWS.mkdir(parents=True, exist_ok=True)
    crowns = {
        "birch": (LIME, BIRCH, [(41, 69, 17, 13), (83, 62, 19, 15),
                               (49, 47, 18, 15), (73, 39, 19, 18), (61, 23, 14, 16)]),
        "broadleaf_tree": (OAK, BARK, [(88, 60, 24, 18), (40, 64, 25, 17),
                                      (79, 38, 29, 20), (41, 43, 25, 19),
                                      (54, 27, 24, 18), (65, 57, 21, 15)]),
        "autumn_tree": (AUTUMN, BARK, [(88, 68, 21, 16), (37, 61, 21, 19),
                                      (53, 42, 27, 22), (86, 42, 22, 18),
                                      (65, 26, 21, 19), (60, 66, 21, 14)]),
    }
    names = ("pine", "spruce", "birch", "broadleaf_tree", "autumn_tree", "dead_tree")
    preview = Image.new("RGBA", (456, 312), "#414941")
    labels = ImageDraw.Draw(preview)
    for index, name in enumerate(names):
        image = Image.new("RGBA", (SIZE, SIZE))
        draw = ImageDraw.Draw(image)
        rng = Random(1238 + index)
        if name in crowns:
            deciduous(draw, rng, *crowns[name])
        elif name == "dead_tree":
            dead(draw, rng)
        else:
            conifer(draw, rng, name == "spruce")
        save_checked(image, OUTPUT / f"{name}.png")
        x, y = index % 3 * 152 + 12, index // 3 * 156 + 4
        preview.alpha_composite(image, (x, y))
        labels.text((x + 64, y + 138), name, fill="#f4efdc", anchor="mm")
    preview.save(PREVIEWS / "trees.png")


if __name__ == "__main__":
    main()
