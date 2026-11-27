"""Material passes for the existing World V7 object silhouettes."""
from functools import lru_cache
from pathlib import Path

import numpy as np
from PIL import Image, ImageDraw

from generate_massive_mountain import edn, geometry
from generate_natural_relief import full_scenes
from generate_relief_demos import pixel
from relief_integrated import stairs

IMAGES = Path("resources/images")
ART = IMAGES / "world_v7_props"
BASE = IMAGES / "visual_v3"

SOURCES = {
    "house": ("buildings/neutral_house", "house"),
    "blue-house": ("buildings/blue_house", "house"),
    "red-house": ("buildings/red_house", "house"),
    "sawmill": ("buildings/sawmill", "house"),
    "watchtower": ("buildings/watchtower", "house"),
    "mine": ("buildings/mine", "wood"),
    "gate": ("buildings/gate", "wood"),
    "wooden-fence": ("buildings/fence", "wood"),
    "tent": ("buildings/tent", "cloth"),
    "sign": ("buildings/sign", "wood"),
    "well": ("decorations/well", "stone"),
    "shore-rocks": ("decorations/shore_rocks", "stone"),
    "campfire": ("decorations/campfire", "fire"),
    "crate": ("resources/crate", "wood"),
    "barrel": ("resources/barrel", "wood"),
    "chest": ("resources/chest", "wood"),
    "log-bundle": ("resources/log_bundle", "bark"),
    "stone-pile": ("resources/stone_pile", "stone"),
    "stump": ("vegetation/stump", "bark"),
    "fallen-tree": ("vegetation/fallen_tree", "bark"),
    "axe": ("resources/axe", "metal"),
    "pickaxe": ("resources/pickaxe", "metal"),
    "stone": ("mountains/small_rock", "stone"),
    "stone-alt": ("mountains/medium_rock", "stone"),
    "large-boulder": ("mountains/large_boulder", "stone"),
    "mossy-rock": ("mountains/mossy_rock", "stone"),
    "gold-ore": ("mountains/gold_ore", "stone"),
    "iron-ore": ("mountains/iron_ore", "stone"),
    "crystals": ("mountains/crystals", "stone"),
}


def soften(a):
    a = np.clip(a, 0, 1)
    return a * a * (3 - 2 * a)


def surface(source, material, seed):
    """Shade hewn boards, roof laps, cloth folds and split mineral faces."""
    rgba = np.asarray(source.convert("RGBA")).copy()
    rgb = rgba[:, :, :3].astype(np.float32)
    alpha = rgba[:, :, 3]
    h, w = alpha.shape
    y, x = np.mgrid[:h, :w]
    u, v = x // 2, y // 2
    ylocal, xlocal = y / max(1, h - 1), x / max(1, w - 1)
    lum = np.mean(rgb, axis=2)
    span = np.max(rgb, axis=2) - np.min(rgb, axis=2)
    mid = soften((lum - 37) / 35) * (1 - .64 * soften((lum - 187) / 54))
    wood = (rgb[..., 0] > rgb[..., 2] + 15) & (rgb[..., 1] > rgb[..., 2] + 5)
    noise = ((u * 83 + v * 131 + seed * 137) ^ (u * v * 17)) % 17
    variation = (noise - 8) * .38
    shade = 1.12 - .10 * xlocal - .105 * ylocal
    relief = np.zeros((h, w), dtype=np.float32)
    if material in {"house", "wood", "bark"}:
        roof = (ylocal < .56) & (material == "house")
        grain = np.sin(v * .86 + .27 * np.sin(u * .44))
        check = (np.mod(u + 4 * v + seed, 23) < 4) & (np.mod(v + seed, 5) < 2)
        knots = ((u + 2 * v + seed) % 31 < 3) & (grain > .45)
        relief += (grain * 2.5 - 6 * check + 5 * knots) * wood * (~roof)
        lap = np.mod(v + .56 * (u - w * .25), 5.7)
        ridge = np.where(lap < .90, -9, np.where(lap > 4.8, 8, 0))
        broken = .42 + .58 * soften((np.sin(u * .44 + v * .72) + .45) / 1.35)
        relief += roof * ridge * broken
        if material == "bark":
            relief += 5 * np.sin(u * .72 + .42 * np.sin(v * .23)) * wood
    if material in {"stone", "metal", "fire", "house"}:
        facets = np.sin(u * .29 - .43 * v) + .68 * np.cos(u * .49 + v * .32)
        joins = (np.abs(np.sin(u * .60 + v * .33 + .7 * np.cos(v * .35))) < .18)
        mineral = (span < 30) if material in {"house", "fire"} else 1
        relief += (np.floor(facets * 2.5) * 2.4 - 10 * joins * (facets > -.56)) * mineral
    if material == "cloth":
        folds = np.sin(u * .42 + v * .13) + .45 * np.sin(.7 * u + .3 * v)
        seam = np.mod(v + .35 * u, 9) < .95
        relief += (folds * 5 - 6 * seam * (np.sin(u * .28) > 0)) * (lum > 55)
    above = np.vstack([lum[:1], lum[:-1]])
    below = np.vstack([lum[1:], lum[-1:]])
    inside = np.vstack([alpha[:1], alpha[:-1]]) > 0
    relief += np.clip(lum - above, 0, 34) * .11 * inside
    relief -= np.clip(lum - below, 0, 34) * .06
    out = rgb * shade[..., None] + ((relief + variation) * mid)[..., None]
    if material == "fire":
        hot = (rgb[..., 0] > 170) & (rgb[..., 0] > rgb[..., 2] + 55)
        out += hot[..., None] * [12, 3, -2]
    if material in {"wood", "stone", "bark", "cloth"}:
        out += mid[..., None] * [2.3, 1.5, -1.4]
    rgba[:, :, :3] = np.uint8(np.clip(np.rint(out), 0, 255))
    return Image.fromarray(rgba)


def save_material(asset, path, material):
    with Image.open(path) as src:
        result = surface(src, material, sum(map(ord, asset)))
        assert result.size == src.size
        assert result.getchannel("A").tobytes() == src.convert("RGBA").getchannel("A").tobytes()
    result.save(ART / (asset + ".png"), optimize=True)


@lru_cache(maxsize=16)
def stair_source(x, y):
    for revision in ("relief_v6_4_8", "relief_v6_4_2"):
        path = IMAGES / revision / f"mountain-lit-{x}-{y}.png"
        if path.exists():
            with Image.open(path) as image:
                return image.convert("RGBA")
    raise FileNotFoundError((x, y))


def stair_details():
    """Repaint the existing stair treads in-place over the approved mountain art."""
    trail = geometry()[0]
    scene = full_scenes()[0]
    parts = []
    for gx, gy in [(14, 68), (8, 74), (14, 80), (16, 82), (11, 85), (12, 87)]:
        point = gx, gy
        if point not in trail or point in (trail[0], trail[-1]):
            continue
        i = trail.index(point)
        old, new = trail[i - 1], trail[i + 1]
        if old[0] != new[0] and old[1] != new[1]:
            continue
        ox, oy = pixel(scene, point)
        oy += 5
        stencil = Image.new("RGBA", (40, 40))
        stairs(ImageDraw.Draw(stencil), 20, 20, old[0] != new[0], 1.0)
        alpha = stencil.getchannel("A")
        samples = Image.new("RGBA", stencil.size)
        for v in range(40):
            for u in range(40):
                if not alpha.getpixel((u, v)):
                    continue
                x, y = ox - 20 + u, oy - 20 + v
                tx, ty = (x // 128) * 128, (y // 128) * 128
                samples.putpixel((u, v), stair_source(tx, ty).getpixel((x - tx, y - ty)))
        samples.putalpha(alpha)
        asset = f"stairs-{gx}-{gy}"
        surface(samples, "stone", gx * 13 + gy).save(ART / f"{asset}.png", optimize=True)
        parts.append({"asset": asset, "x": gx*64-8, "y": gy*64+2, "width": 80, "height": 80})
    return parts


def bake():
    ART.mkdir(parents=True, exist_ok=True)
    for asset, (source, material) in SOURCES.items():
        save_material(asset, BASE / (source + ".png"), material)
    for kind, material in [("raider-tent", "cloth"), ("cairn", "stone"),
                           ("lantern", "metal"), ("ember-pit", "fire")]:
        for i in range(4):
            save_material(f"{kind}-{i}", IMAGES / "visual_v4/props" / f"{kind}_{i}.png", material)
    for kind, count, material in [("abandoned-camp", 5, "cloth"), ("relict-well", 4, "stone")]:
        for i in range(count):
            file = IMAGES / "visual_v5" / f"{kind}_{i}.png"
            if file.exists():
                save_material(f"{kind}-{i}", file, material)
    for path in sorted((IMAGES / "relief_v6_4_8").glob("*-camp-camp*.png")):
        save_material(path.stem, path, "cloth")
    (ART / "stairs.edn").write_text(edn(stair_details()) + "\n", encoding="utf-8")


if __name__ == "__main__":
    bake()
