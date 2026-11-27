"""Assemble comparisons and a gallery from captured Swing frames."""
from pathlib import Path
from sys import argv
from PIL import Image, ImageDraw

V6 = argv[1:] == ["world-v6"]
OUTPUT = Path("docs/previews/world_v6" if V6 else "docs/previews/world_v5")
BASELINE = "v5" if V6 else "v41"
VERSIONS = ("WORLD V5", "WORLD V6") if V6 else ("WORLD V4.1", "WORLD V5")
SCENES = {
    "forest": "DECIDUOUS FOREST",
    "conifer": "CONIFER FOREST",
    "clearing": "FOREST CLEARING",
    "desert": "DESERT",
    "canyon": "CANYON",
    "oasis": "OASIS",
    "swamp": "SWAMP",
    "swamp-crossing": "MARSH WALKWAY",
    "mountains": "MOUNTAIN RANGE",
    "mountain-pass": "MOUNTAIN PASS",
    "snow": "SNOW RIDGES",
    "river": "RIVER",
    "lake": "LAKE",
    "waterfall": "WATERFALL",
    "crossroads": "CROSSROADS",
    "settlement": "SETTLEMENT",
    "bandit-camp": "BANDIT CAMP",
}


if V6:
    SCENES = {
        "conifer": "CONIFER FOREST", "forest": "DECIDUOUS FOREST",
        "clearing": "FOREST CLEARING", "mountains": "CORDILLERA",
        "gorge": "GORGE", "mountain-pass": "MOUNTAIN PASS", "snow": "GLACIER",
        "dunes": "DUNES", "canyon": "CANYON", "ruins": "DESERT RUINS",
        "oasis": "OASIS", "swamp": "MIRE", "swamp-crossing": "MARSH WALKWAY",
        "river": "RIVER", "lake": "LAKE", "waterfall": "WATERFALL",
        "settlement": "SETTLEMENT", "bandit-camp": "BANDIT CAMP",
        "transition": "BIOME TRANSITION",
    }


def load(name):
    with Image.open(OUTPUT / f"{name}.png") as image:
        return image.convert("RGB")


def compare(name, title):
    images = [load(f"{BASELINE}-{name}"), load(name)]
    width, height = images[0].size
    result = Image.new("RGB", (width * 2 + 24, height + 52), "#25291f")
    draw = ImageDraw.Draw(result)
    for i, (image, version) in enumerate(zip(images, VERSIONS)):
        x = 8 + (width + 8) * i
        draw.text((x, 16), f"{version}  |  {title}", fill="#eadcaa")
        result.paste(image, (x, 42))
    result.save(OUTPUT / f"compare-{name}.jpg", quality=93, subsampling=0)


def gallery():
    names = [*SCENES, "overview", "fullscreen"]
    cell_width, cell_height = 316, 228
    sheet = Image.new("RGB", (cell_width * 4, cell_height * ((len(names) + 3) // 4)), "#25291f")
    draw = ImageDraw.Draw(sheet)
    for i, name in enumerate(names):
        image = load(name)
        image.thumbnail((cell_width - 16, cell_height - 34), Image.Resampling.NEAREST)
        x, y = (i % 4) * cell_width, (i // 4) * cell_height
        draw.text((x + 8, y + 8), SCENES.get(name, name.upper()), fill="#eadcaa")
        sheet.paste(image, (x + (cell_width - image.width) // 2, y + 28))
    sheet.save(OUTPUT / "gallery.png")


if __name__ == "__main__":
    for scene, title in SCENES.items():
        compare(scene, title)
    compare("overview", "FULL MAP AND ROAD NETWORK")
    gallery()
