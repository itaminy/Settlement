"""Assemble comparisons from the actual Swing relief captures."""
from pathlib import Path
from PIL import Image, ImageDraw

ROOT = Path("docs/previews/world_v6_4_1")


def sheet(views, columns, size, name, root=ROOT):
    w,h = size
    rows = (len(views)+columns-1)//columns
    image = Image.new("RGB",(8+columns*(w+8),8+rows*(h+28)),(36,42,39))
    d = ImageDraw.Draw(image)
    for i,(source,label) in enumerate(views):
        with Image.open(root/f"{source}.png") as raw:
            frame=raw.convert("RGB")
            frame.thumbnail(size,Image.Resampling.NEAREST)
        x,y = 8+i%columns*(w+8),8+i//columns*(h+28)
        d.text((x,y),label,fill=(233,218,191))
        image.paste(frame,(x+(w-frame.width)//2,y+18))
    image.save(root/f"{name}.png")


if __name__=="__main__":
    sheet([("lighting-off","BAKED LIGHTING OFF"),("canyon-demo","NORTHWEST LIGHT + SHADOWS")],
          2,(740,518),"lighting-comparison")
    sheet([("projection-off","FLAT PROJECTION"),("mountain-demo","2.5D HEIGHT PROJECTION")],
          2,(740,540),"depth-comparison")
    sheet([("mountain-demo","MOUNTAIN"),("canyon-demo","GORGE"),
           ("bridge-demo","BRIDGE"),("cave-demo","CAVE"),
           ("terraces-demo","TERRACES AND STAIRS"),("fullscreen-demo","FULLSCREEN")],
          3,(465,346),"gallery")
