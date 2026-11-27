"""Generate the two inventory tool icons on a 32px pixel grid."""
from pathlib import Path

from PIL import Image

from scripts.visual_v3.primitives import Canvas, WOOD

ROOT = Path(__file__).resolve().parent
OUTPUT = ROOT / "resources/images/visual_v3/resources"
STEEL = ("#26333a", "#475960", "#6f8385", "#9caaa5", "#cbd1ba")


def handle(canvas, start, end):
    canvas.line([start, end], WOOD[0], 5)
    canvas.line([start, end], WOOD[2], 3)
    canvas.line([(x - 1, y) for x, y in (start, end)], WOOD[4])
    x, y = start
    canvas.line([(x - 1, y - 1), (x + 1, y + 1)], WOOD[1], 2)


def axe(canvas):
    handle(canvas, (7, 26), (22, 9))
    canvas.poly([(12,9),(18,6),(20,6),(24,4),(27,5),(28,8),(27,12),
                 (24,16),(20,18),(18,17),(17,13),(13,12)], STEEL[0])
    canvas.poly([(15,9),(20,7),(24,5),(26,6),(27,8),(25,12),(20,16),
                 (19,13),(17,11)], STEEL[2])
    canvas.poly([(24,5),(26,6),(27,8),(27,10),(24,14),(20,16),(21,13),
                 (24,10),(25,7)], STEEL[4])
    canvas.line([(17,9),(19,9),(21,11),(20,13)], STEEL[1], 2)
    canvas.line([(14,9),(16,8),(18,8)], STEEL[3])


def pickaxe(canvas):
    handle(canvas, (7, 26), (19, 12))
    canvas.poly([(8,5),(13,4),(19,6),(23,9),(26,13),(28,19),(27,20),
                 (23,15),(19,12),(15,10),(10,8),(6,8)], STEEL[0])
    canvas.poly([(8,6),(13,5),(18,7),(22,9),(25,13),(26,17),(23,14),
                 (19,11),(15,9),(10,7),(8,7)], STEEL[2])
    canvas.line([(10,6),(13,5),(17,6),(21,8),(24,11)], STEEL[4])
    canvas.line([(17,9),(19,10),(21,12)], STEEL[1], 2)
    canvas.line([(18,10),(20,12)], WOOD[4])


def main():
    for painter in (axe, pickaxe):
        canvas = Canvas(32, 0)
        painter(canvas)
        icon = canvas.image.resize((64, 64), Image.Resampling.NEAREST)
        path = OUTPUT / f"{painter.__name__}.png"
        icon.save(path)
        with Image.open(path) as saved:
            assert (saved.format, saved.mode, saved.size) == ("PNG", "RGBA", (64, 64))
            alpha = saved.getchannel("A")
            left, top, right, bottom = alpha.getbbox()
            assert min(left, top, 64 - right, 64 - bottom) >= 4
            assert alpha.getextrema() == (0, 255) and not sum(alpha.histogram()[1:255])
        print(path.relative_to(ROOT))


if __name__ == "__main__":
    main()
