"""Compare captured World V4 and V4.1 Swing frames."""
from pathlib import Path
from PIL import Image, ImageDraw

PREVIEWS = Path(__file__).resolve().parents[1] / "docs/previews"
BEFORE = PREVIEWS / "world_v4"
AFTER = PREVIEWS / "world_v4_1"

for topic, scene in (
    ("mountains", "mountains"), ("waterfall", "waterfall"),
    ("forest", "forest"), ("swamp", "swamp"),
    ("desert", "oasis"), ("roads", "settlement"),
):
    with Image.open(BEFORE / f"{scene}.png") as old, Image.open(AFTER / f"{scene}.png") as new:
        header = 30
        canvas = Image.new("RGB", (old.width + new.width, max(old.height, new.height) + header),
                           (32, 38, 33))
        canvas.paste(old, (0, header))
        canvas.paste(new, (old.width, header))
        draw = ImageDraw.Draw(canvas)
        draw.text((12, 9), "WORLD V4", fill=(242, 239, 214))
        draw.text((old.width + 12, 9), "WORLD V4.1", fill=(242, 239, 214))
        canvas.save(AFTER / f"compare-{topic}.png")
