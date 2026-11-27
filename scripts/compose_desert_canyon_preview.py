"""Assemble comparison sheets and an animated walk from Swing captures."""
from pathlib import Path

from PIL import Image, ImageDraw

OUTPUT = Path("docs/previews/world_v6_4")
FRAMES = Path(".canyon-frames")


def grid(views, columns, size, filename):
    width,height=size
    rows=(len(views)+columns-1)//columns
    sheet=Image.new("RGB",(columns*(width+8)+8,rows*(height+28)+8),(38,42,37))
    draw=ImageDraw.Draw(sheet)
    for i,(name,label) in enumerate(views):
        with Image.open(OUTPUT/f"{name}.png") as f:
            image=f.convert("RGB")
            image.thumbnail(size,Image.Resampling.NEAREST)
        x,y=8+i%columns*(width+8),8+i//columns*(height+28)
        draw.text((x,y),label,fill=(235,213,173))
        sheet.paste(image,(x+(width-image.width)//2,y+17))
    sheet.save(OUTPUT/filename)


def exploration():
    frames=[]
    for path in sorted(FRAMES.glob("frame-*.png")):
        with Image.open(path) as f:
            image=f.convert("RGB")
            image.thumbnail((640,428),Image.Resampling.NEAREST)
            frames.append(image)
    if not frames:
        raise RuntimeError("Missing Swing captures; run the TCP canyon walk first")
    swatch=Image.new("RGB",(768,480))
    for i in range(36):
        sample=frames[i*(len(frames)-1)//35].resize((128,80),Image.Resampling.NEAREST)
        swatch.paste(sample,(i%6*128,i//6*80))
    palette=swatch.quantize(colors=256)
    indexed=[f.quantize(palette=palette,dither=Image.Dither.NONE) for f in frames]
    indexed[0].save(OUTPUT/"exploration.gif",save_all=True,append_images=indexed[1:],
                    duration=75,loop=0,optimize=True)
    with Image.open(OUTPUT/"exploration.gif") as gif:
        print(f"{len(frames)} Swing images, {gif.n_frames} GIF frames")


if __name__=="__main__":
    camps=[(f"canyon-camp-{i}",f"CAMP {i}") for i in range(1,4)]
    grid([("canyon-before","WORLD V6.3.2"),("canyon-overview","WORLD V6.4")],
         2,(620,800),"compare-canyon.png")
    grid(camps,3,(455,335),"canyon-camps.png")
    grid([("canyon-entrance","ENTRANCE"),("deep-gorge","NORTH GORGE"),
          ("canyon-bridge","EAST BRIDGE"),*camps,("canyon-ruins","RUINS"),
          ("canyon-oasis","OASIS"),("fullscreen","FULLSCREEN")],3,(420,300),"gallery.png")
    exploration()
