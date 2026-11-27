"""Generate the Settlement UI V1 wordmark and reusable wood tile."""
from pathlib import Path
from random import Random
from PIL import Image, ImageDraw

OUT = Path(__file__).resolve().parent / "resources/images/ui_v1"
OUT.mkdir(parents=True, exist_ok=True)
LETTERS = {
    "S": ("01111", "11000", "11000", "01110", "00011", "00011", "11110"),
    "E": ("11111", "11000", "11000", "11110", "11000", "11000", "11111"),
    "T": ("11111", "00100", "00100", "00100", "00100", "00100", "00100"),
    "L": ("11000", "11000", "11000", "11000", "11000", "11000", "11111"),
    "M": ("11011", "11111", "10101", "10101", "10001", "10001", "10001"),
    "N": ("11001", "11001", "11101", "10101", "10111", "10011", "10011"),
}


def logo():
    word = "SETTLEMENT"
    image = Image.new("RGBA", (166, 41))
    d = ImageDraw.Draw(image)
    d.polygon([(5, 11), (16, 5), (149, 5), (160, 11), (160, 30),
               (149, 36), (16, 36), (5, 30)], (36, 25, 19))
    d.polygon([(6, 11), (17, 6), (148, 6), (159, 11), (159, 29),
               (148, 35), (17, 35), (6, 29)], outline=(146, 108, 63))
    d.line((19, 8, 146, 8), (192, 151, 80))
    d.line((19, 33, 146, 33), (98, 76, 50))
    for x in (10, 154):
        d.polygon([(x, 16), (x + 3, 20), (x, 24), (x - 3, 20)], (179, 135, 68))
    for index, letter in enumerate(word):
        for y, row in enumerate(LETTERS[letter]):
            for x, bit in enumerate(row):
                if bit == "1":
                    px, py = 24 + index * 12 + x * 2, 13 + y * 2
                    d.rectangle((px + 1, py + 2, px + 2, py + 3), (71, 46, 26))
                    d.rectangle((px, py, px + 1, py + 1),
                                (250 - y * 7, 216 - y * 9, 147 - y * 8))
    image.resize((332, 82), Image.Resampling.NEAREST).save(OUT / "wordmark.png")


def wood():
    rng = Random(5902)
    image = Image.new("RGB", (64, 64), (42, 34, 29))
    d = ImageDraw.Draw(image)
    for y in (15, 32, 48):
        d.line((0, y, 63, y), (38, 30, 25))
        d.line((0, y + 1, 63, y + 1), (46, 37, 30))
    for _ in range(52):
        x, y = rng.randrange(1, 61), rng.randrange(1, 62)
        d.line((x, y, min(62, x + rng.randrange(2, 12)), y), rng.choice([(48, 37, 29), (37, 30, 26)]))
    image.save(OUT / "wood.png")


if __name__ == "__main__":
    logo()
    wood()
