"""Derive bandit walk frames from the existing red character silhouette."""
from pathlib import Path

from PIL import Image

DIRECTORY = Path(__file__).resolve().parent / "resources/images/characters"
DIRECTIONS = ("up", "down", "left", "right")


def recolor(x, y, pixel):
    r, g, b, a = pixel
    if a < 128:
        return 0, 0, 0, 0
    if r > g * 1.38 and r > b * 1.42 and abs(g - b) < 35 and y >= 24:
        # Preserve folds while replacing the red tunic with slate and violet.
        light = max(0, min(1, (r - 42) / 196))
        if y % 16 in (3, 4, 5, 6) or g > 85:
            return round(53 + light * 91), round(35 + light * 58), round(73 + light * 111), a
        return round(29 + light * 49), round(30 + light * 48), round(36 + light * 54), a
    if y < 19 and b < g < r < 180 and r - g > 7:
        value = round(15 + 0.54 * g)
        return value, value, value + 8, a
    return pixel


def main():
    for direction in DIRECTIONS:
        for frame in range(3):
            suffix = f"{direction}_{frame}.png"
            with Image.open(DIRECTORY / ("bot_" + suffix)) as source:
                base = source.convert("RGBA")
            target = Image.new("RGBA", base.size)
            for y in range(base.height):
                for x in range(base.width):
                    target.putpixel((x, y), recolor(x, y, base.getpixel((x, y))))
            assert target.size == (64, 64) and target.getchannel("A").getextrema()[0] == 0
            target.save(DIRECTORY / ("bandit_" + suffix))


if __name__ == "__main__":
    main()
