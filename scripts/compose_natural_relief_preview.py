"""Package V6.4.2 comparisons, gallery and the two real Swing TCP walks."""
from pathlib import Path

from PIL import Image
from compose_relief_preview import sheet

ROOT = Path("docs/previews/world_v6_4_2")
GALLERY = [
    ("canyon-overview","CANYON"),("deep-gorge","DEEP GORGE"),("gorge-floor","GORGE FLOOR"),
    ("bridge-north","NORTH BRIDGE"),("bridge-east","EAST BRIDGE"),("cave","CAVE"),
    ("desert-camp","DESERT CAMP"),("mountain-overview","MOUNTAIN"),("foothills","FOOTHILLS"),
    ("mountain-slope","ROCKY SLOPE"),("highlands","HIGHLANDS"),("summit","SUMMIT"),
    ("mountain-lake","MOUNTAIN LAKE"),("rock-occlusion","FOREGROUND ROCK"),
    ("fullscreen-canyon","FULLSCREEN CANYON"),("fullscreen-mountain","FULLSCREEN MOUNTAIN"),
]


def gif(folder,name):
    images=[]
    for f in sorted(Path(folder).glob("frame-*.png")):
        with Image.open(f) as original:
            out=original.convert("RGB")
            out.thumbnail((640,447),Image.Resampling.NEAREST)
            images.append(out)
    if not images:
        raise RuntimeError(f"No Swing images in {folder}")
    swatch=Image.new("RGB",(756,480))
    for i in range(36):
        sample=images[i*(len(images)-1)//35].resize((126,80),Image.Resampling.NEAREST)
        swatch.paste(sample,(i%6*126,i//6*80))
    palette=swatch.quantize(colors=256)
    indexed=[f.quantize(palette=palette,dither=Image.Dither.NONE) for f in images]
    indexed[0].save(ROOT/name,save_all=True,append_images=indexed[1:],loop=0,duration=80,optimize=True)
    with Image.open(ROOT/name) as out:
        print(name,len(images),"Swing frames,",out.n_frames,"GIF frames")


if __name__=="__main__":
    sheet(GALLERY,3,(445,315),"gallery",root=ROOT)
    compare=[("prior-canyon","V6.4.1 CANYON"),("bridge-north","V6.4.2 CANYON"),
             ("prior-mountain","V6.4.1 SUMMIT"),("summit","V6.4.2 SUMMIT")]
    sheet(compare,2,(720,492),"version-comparison",root=ROOT)
    lighting=[("unlit-canyon","CANYON | LIGHTING OFF"),("bridge-north","CANYON | LIGHTING ON"),
              ("unlit-mountain","MOUNTAIN | LIGHTING OFF"),("summit","MOUNTAIN | LIGHTING ON")]
    sheet(lighting,2,(720,492),"lighting-comparison",root=ROOT)
    gif(".natural-canyon-frames","canyon-exploration.gif")
    gif(".natural-mountain-frames","mountain-ascent.gif")
