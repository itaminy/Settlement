"""Encode the frames recorded by scripts/preview_movement_v3.clj at 25 fps."""
import argparse
from pathlib import Path

from PIL import Image


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("frames", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    paths = sorted(args.frames.glob("frame-*.png"))
    if not paths:
        parser.error("no recorded Swing frames")
    sample = Image.new("RGB", (640, 420))
    for index in range(16):
        with Image.open(paths[index * len(paths) // 16]) as image:
            sample.paste(image.resize((160, 105), Image.Resampling.NEAREST),
                         ((index % 4) * 160, (index // 4) * 105))
    palette = sample.quantize(colors=256)
    frames = []
    for path in paths:
        with Image.open(path) as image:
            frame = image.convert("RGB")
            frames.append(frame.quantize(palette=palette, dither=Image.Dither.NONE))
    args.output.parent.mkdir(parents=True, exist_ok=True)
    frames[0].save(args.output, save_all=True, append_images=frames[1:],
                   duration=40, loop=0, optimize=True)
    with Image.open(args.output) as preview:
        duration = 0
        for index in range(preview.n_frames):
            preview.seek(index)
            duration += preview.info["duration"]
        assert duration == 40 * len(paths)
        print(f"{preview.width}x{preview.height}, {duration / 1000:.2f} s, {preview.n_frames} GIF frames")


if __name__ == "__main__":
    main()
