"""Generate and validate Settlement's standalone visual_v3 asset pack.

Run with Python and Pillow: python generate_visual_v3.py
"""
from math import sin
from pathlib import Path
from random import Random

from PIL import Image, ImageDraw, ImageFont

from scripts.visual_v3 import natural as n, primitives as p, structures as s, tiles as t

ROOT = Path(__file__).resolve().parent
ASSETS = ROOT / "resources/images/visual_v3"
PREVIEWS = ROOT / "docs/previews/visual_v3"
NEAREST = Image.Resampling.NEAREST

OBJECTS = {
    "vegetation": {
        "bush": (64,p.bush), "berry_bush": (64,lambda c:p.bush(c,True)),
        **{f"{color}_flowers":(32,lambda c,color=color:p.flowers(c,color)) for color in p.FLOWERS},
        "tall_grass": (32,lambda c:p.blades(c,15,28,23,20)),
        "reeds": (32,lambda c:p.blades(c,15,28,23,14,True)),
        "fern": (32,p.fern), "mushrooms": (32,lambda c:p.mushrooms(c,False)),
        "mushroom_group": (32,lambda c:p.mushrooms(c,True)),
        "stump": (32,p.stump), "fallen_tree": (64,p.fallen),
    },
    "mountains": {
        "small_rock": (32,n.small_rock),
        "medium_rock": (64,lambda c:n.rock(c,11,22,42,30)),
        "large_boulder": (64,lambda c:n.rock(c,5,13,54,44)),
        "mossy_rock": (64,lambda c:n.rock(c,8,21,47,32,moss=True)),
        "rock_peak": (96,lambda c:n.formation(c,"peak")),
        "mountain": (128,lambda c:n.formation(c,"mountain")),
        "snow_mountain": (128,lambda c:n.formation(c,"snow")),
        "cliff": (96,lambda c:n.formation(c,"cliff")), "cave": (64,n.cave),
        "gold_ore": (64,lambda c:n.rock(c,7,18,50,37,ore=n.GOLD)),
        "iron_ore": (64,lambda c:n.rock(c,7,18,50,37,ore=n.IRON)),
        "crystals": (64,n.crystal),
    },
    "desert": {
        "cactus": (64,lambda c:n.cactus(c,False)),
        "flowering_cactus": (64,lambda c:n.cactus(c,True)),
        "dry_bush": (32,n.dry_bush), "bones": (32,n.bones),
        "sand_boulders": (64,lambda c:[n.rock(c,*shape,p.SAND) for shape in
                                     [(5,20,36,29),(30,30,28,23),(11,39,23,17)]]),
        "sand_rocks": (96,lambda c:n.formation(c,"desert")), "palm": (96,n.palm),
    },
    "buildings": {
        **{f"{team}_house":(96,lambda c,team=team:s.house(c,team)) for team in s.ROOFS},
        "fence": (64,lambda c:s.fence(c,False)), "gate": (64,lambda c:s.fence(c,True)),
        "watchtower": (96,s.tower), "sawmill": (96,s.sawmill), "mine": (64,s.mine),
        "tent": (64,s.tent), "bridge": (128,s.bridge),
        "blue_flag": (32,lambda c:s.flag(c,"blue")), "red_flag": (32,lambda c:s.flag(c,"red")),
        "sign": (32,s.sign),
    },
    "resources": {
        "log_bundle": (32,p.log_bundle),
        "stone_pile": (32,n.stone_pile), "barrel": (32,s.barrel),
        "crate": (32,s.crate), "chest": (32,s.chest),
    },
    "decorations": {
        "campfire": (32,s.campfire), "well": (64,s.well), "shore_rocks": (32,n.drift_rocks),
    },
    "water": {"waterfall": (96,t.waterfall)},
}
WATERS = ("deep_water","shallow_water","lily_water","river","lake")
MULTIPART_SPRITES = {("desert","bones"),("decorations","shore_rocks")}


def sprite_key(category,name):
    # Team palettes share a seed so every non-team-colored pixel is identical.
    if category=="buildings" and name.endswith("_house"):
        return "team_house"
    return category+"/"+name


def connected_sizes(alpha):
    remaining={(x,y) for y in range(alpha.height) for x in range(alpha.width)
               if alpha.getpixel((x,y))}
    sizes=[]
    while remaining:
        queue=[remaining.pop()]
        size=0
        while queue:
            x,y=queue.pop()
            size+=1
            neighbors=remaining.intersection((x+dx,y+dy) for dx in (-1,0,1) for dy in (-1,0,1))
            remaining.difference_update(neighbors)
            queue.extend(neighbors)
        sizes.append(size)
    return sorted(sizes,reverse=True)


def save_asset(category,name,image,is_sprite,inventory):
    width,height=image.size
    path=ASSETS/category/(name+".png")
    path.parent.mkdir(parents=True,exist_ok=True)
    image.resize((width*2,height*2),NEAREST).save(path)
    with Image.open(path) as saved:
        assert saved.format=="PNG" and saved.mode=="RGBA",path
        assert saved.size==(width*2,height*2) and width in (32,64,96,128) and height==width,path
        saved.verify()
    with Image.open(path) as saved:
        assert saved.tobytes()==image.resize(saved.size,NEAREST).tobytes(),path
        alpha=saved.getchannel("A")
        assert sum(alpha.histogram()[1:255])==0,path
        left,top,right,bottom=alpha.getbbox()
        if is_sprite:
            assert min(left,top,saved.width-right,saved.height-bottom)>=4,(path,(left,top,right,bottom))
            assert alpha.getextrema()==(0,255),path
            components=connected_sizes(image.getchannel("A"))
            expected=2 if (category,name) in MULTIPART_SPRITES else 1
            assert len(components)==expected and min(components)>=8,(path,components)
        else:
            assert alpha.getextrema()==(255,255),path
        inventory.append((path.relative_to(ROOT).as_posix(), f"{saved.width}x{saved.height}"))
    return image


def verify_tiles(terrain,water,shore):
    # Water interiors can vary independently while retaining the same socket.
    reference=water["deep_water"]
    for name,tile in water.items():
        for y in range(32):
            for x in range(32):
                if min(x,y,31-x,31-y)<3:
                    assert tile.getpixel((x,y))==reference.getpixel((x,y)),(name,x,y)
    # Compare every side with the corresponding side of an independent reference
    # using the same two corner values. Neighboring sides sample adjacent
    # columns/rows from the periodic ground and water textures.
    masks={**t.CORNERS,"land":(1,1,1,1),"water":(0,0,0,0)}
    sides=[((0,1),[(x,0) for x in range(32)]),
           ((1,2),[(31,y) for y in range(32)]),
           ((3,2),[(x,31) for x in range(32)]),
           ((0,3),[(0,y) for y in range(32)])]
    for ground in t.SHORE:
        tiles={**{mask:shore[f"{ground}_{mask}"] for mask in t.CORNERS},
               "land":terrain[ground],"water":reference}
        for corner_indices,coordinates in sides:
            signatures={}
            for name,corners in masks.items():
                pair=tuple(corners[i] for i in corner_indices)
                edge=[tiles[name].getpixel(xy) for xy in coordinates]
                assert signatures.setdefault(pair,edge)==edge,(ground,name,corner_indices)
    # Screen periodic textures for edge discontinuities against interior samples.
    for name,tile in {**terrain,**water}.items():
        def jump(axis,offset):
            def xy(x,y):
                return (x,y) if axis==0 else (y,x)
            colors=[(tile.getpixel(xy(offset-1 if offset else 31,y)),
                     tile.getpixel(xy(offset,y))) for y in range(32)]
            return sum(sum(abs(int(a)-int(b)) for a,b in zip(first[:3],second[:3]))
                       for first,second in colors)
        for axis in (0,1):
            assert jump(axis,0)<=max(jump(axis,offset) for offset in range(1,32)),(name,axis)


def main():
    PREVIEWS.mkdir(parents=True,exist_ok=True)
    inventory=[]
    art={}
    for category,objects in OBJECTS.items():
        art[category]={}
        for name,(size,paint) in objects.items():
            canvas=p.Canvas(size,sum((i+1)*ord(ch) for i,ch in enumerate(sprite_key(category,name))))
            paint(canvas)
            art[category][name]=save_asset(category,name,canvas.image,True,inventory)
    art["terrain"]={name:save_asset("terrain",name,t.terrain_tile(name),False,inventory)
                    for name in t.TERRAIN}
    water={name:save_asset("water",name,t.water_tile(name),False,inventory) for name in WATERS}
    art["water"]={**water,**art["water"]}
    art["transitions"]={}
    for ground in t.SHORE:
        for name,corners in t.CORNERS.items():
            basename=f"{ground}_{name}"
            tile=t.shore_tile(ground,water["deep_water"],art["terrain"][ground],corners)
            art["transitions"][basename]=save_asset("transitions",basename,tile,False,inventory)
    verify_tiles(art["terrain"],water,art["transitions"])
    verify_team_houses(art["buildings"])
    for category,images in art.items():
        contact_sheet(category,images).save(PREVIEWS/(category+".png"))
        print(f"{category}: {len(images)}")
    terrain_sheet(art).save(PREVIEWS/"tile_seams.png")
    showcase(art).save(PREVIEWS/"demo_map.png")
    print(f"Validated {len(inventory)} PNG files")
    (PREVIEWS/"asset_manifest.txt").write_text(
        "visual_v3 asset files and native pixel sizes\n\n"
        +"\n".join(f"{path}  {size}" for path,size in sorted(inventory))+"\n",encoding="utf-8")


def verify_team_houses(buildings):
    blue={tuple(int(color[i:i+2],16) for i in (1,3,5))+(255,):index
          for index,color in enumerate(s.ROOFS["blue"])}
    red=[tuple(int(color[i:i+2],16) for i in (1,3,5))+(255,) for color in s.ROOFS["red"]]
    a,b=buildings["blue_house"],buildings["red_house"]
    assert a.getchannel("A").tobytes()==b.getchannel("A").tobytes()
    for y in range(a.height):
        for x in range(a.width):
            pixel=a.getpixel((x,y))
            assert b.getpixel((x,y))==(red[blue[pixel]] if pixel in blue else pixel),(x,y)


def font(size):
    return ImageFont.load_default(size=size)


def contact_sheet(category,images):
    tiles=category in ("terrain","transitions")
    cols=7 if category=="transitions" else 4
    rows=(len(images)+cols-1)//cols
    width=128 if tiles else 174
    height=104 if tiles else 159
    sheet=Image.new("RGBA",(cols*width+20,rows*height+58),"#283433")
    draw=ImageDraw.Draw(sheet)
    draw.text((19,17),category.replace("_"," ").title(),font=font(23),fill="#e9e2cc")
    for index,(name,sprite) in enumerate(images.items()):
        x,y=10+(index%cols)*width,53+(index//cols)*height
        draw.rectangle((x+2,y,x+width-3,y+height-8),fill="#3f4b43")
        if category=="transitions":
            label=name.replace("rocky_ground","rock").replace("diagonal","diag")
        else:
            label=name
        scale=2 if tiles or sprite.width<=64 else 1
        item=sprite.resize((sprite.width*scale,sprite.height*scale),NEAREST)
        if tiles or sprite.width==32 or category=="water":
            offset=(height-29-item.height)//2
        else:
            offset=height-25-item.height
        if offset<0:
            offset=0
        sheet.alpha_composite(item,(x+(width-item.width)//2,y+offset))
        draw.text((x+width//2,y+height-18),label.replace("_"," "),font=font(12),
                  fill="#f0ead6",anchor="mm")
    return sheet


def terrain_sheet(art):
    combinations=[(name,tile) for name,tile in art["terrain"].items()]
    combinations.extend((name,art["water"][name]) for name in WATERS)
    sheet=Image.new("RGBA",(800,((len(combinations)+3)//4)*220+48),"#283433")
    draw=ImageDraw.Draw(sheet)
    draw.text((12,10),"Repeated 3 x 3 tiles",font=font(20),fill="#f0ead6")
    for index,(name,tile) in enumerate(combinations):
        x,y=index%4*200+4,index//4*220+48
        tile=tile.resize((64,64),NEAREST)
        for row in range(3):
            for col in range(3):
                sheet.alpha_composite(tile,(x+64*col,y+64*row))
        draw.text((x+96,y+204),name.replace("_"," "),font=font(12),
                  fill="#f0ead6",anchor="mm")
    return sheet


def showcase(art):
    width,height=25,16
    # Sample the river/lake at tile vertices. Every adjacent shoreline tile
    # therefore agrees on the two corner values shared with its neighbor.
    def is_water(x,y):
        river=abs(x-(12.8+1.1*sin(y*.56)))<1.35
        lake=((x-14)/4.15)**2+((y-11.1)/3.15)**2<1
        pond=((x-20)/2.35)**2+((y-13.1)/1.65)**2<1
        return river or lake or pond
    def ground_at(x,y):
        if x>16.7+.85*sin(y*.75) and y>6.15+.55*sin(x*.93):
            return "desert" if x>21.1+.9*sin(y*.71) and y<11.3+.7*sin(x) else "sand"
        if x>12.8+.55*sin(y) and y<4.85+.48*sin(x*.86):
            return "rocky_ground"
        return "grass"

    def land_tile(x,y,mask):
        name=reverse.get(mask)
        candidate=Image.new("RGBA",(32,32))
        ground_textures=art["terrain"]
        shore_textures=art["transitions"]
        has_flowers=rng.randrange(7)==0
        for py in range(32):
            for px in range(32):
                u,v=x+px/32,y+py/32
                ground=ground_at(u,v)
                if name:
                    shore_ground="sand" if ground=="desert" else ground
                    texture=shore_textures[f"{shore_ground}_{name}"]
                else:
                    if ground=="grass":
                        road_width=.32+.055*sin(u*13+v*9)
                        main_road=min(u-5.65,11.06-u,road_width) > abs(v-(8.15+.28*sin(u*1.14)))
                        branch=min(v-4.23,11.96-v,road_width) > abs(u-(9.1+.23*sin(v*.96)))
                        if main_road or branch:
                            ground="dirt_road"
                        elif has_flowers:
                            ground="flowering_grass"
                    texture=ground_textures[ground]
                candidate.putpixel((px,py),texture.getpixel((px,py)))
        return candidate
    reverse={mask:name for name,mask in t.CORNERS.items()}
    sheet=Image.new("RGBA",(width*64,height*64))
    rng=Random(812)
    for y in range(height):
        for x in range(width):
            mask=tuple(int(not is_water(u,v)) for u,v in ((x,y),(x+1,y),(x+1,y+1),(x,y+1)))
            if mask==(0,0,0,0):
                water="river" if y<9 else rng.choice(("lake","lake","lily_water","shallow_water"))
                tile=art["water"][water]
            else:
                tile=land_tile(x,y,mask)
            sheet.paste(tile.resize((64,64),NEAREST),(x*64,y*64))

    sprites=[]
    def add(image,x,y):
        sprites.append((round(y*64),round(x*64),image))
    def at(category,name,x,y):
        image=art[category][name]
        add(image.resize((image.width*2,image.height*2),NEAREST),x,y)
    def original(folder,name,x,y):
        with Image.open(ROOT/"resources/images"/folder/(name+".png")) as image:
            add(image.convert("RGBA"),x,y)

    for name,x,y in [("pine",2.1,3.2),("spruce",4.05,2.9),("pine",5.3,4.7),
                     ("broadleaf_tree",1.7,5.3),("birch",3.4,6.3),("spruce",1.2,8),
                     ("broadleaf_tree",4.1,8.1),("birch",5.1,2.3),("autumn_tree",2.9,9.48),
                     ("dead_tree",1.9,13.5),("autumn_tree",5.1,13.2),("pine",.85,11.9)]:
        original("nature_v3",name,x,y)
    for category,names in {
        "vegetation":[("berry_bush",3.7,4.5),("bush",1.0,4.5),("fallen_tree",3.1,11.3),
                      ("tall_grass",2.1,10),("fern",1.1,6.6),("mushroom_group",4.6,9),
                      ("yellow_flowers",4.4,6),("white_flowers",5.2,10.5),("blue_flowers",2.8,7),
                      ("reeds",10.8,12.3),("reeds",16.6,10.1)],
        "mountains":[("mountain",20.7,4.9),("snow_mountain",17.7,4.1),("rock_peak",23.1,4.5),
                     ("cliff",16.5,5),("iron_ore",23.5,5.45),("crystals",21.5,5.6),
                     ("mossy_rock",3.5,12.9),("cave",19.0,5.45),("large_boulder",7.6,3.1),
                     ("medium_rock",13.3,1.85),("large_boulder",15.5,2.25)],
        "buildings":[("blue_house",7.4,7.2),("red_house",10.2,7.2),("neutral_house",7.6,11.1),
                     ("bridge",12.2,9.2),("sawmill",6.6,5.05),("watchtower",9.55,4.45),
                     ("mine",22.5,6.7),("tent",8.1,13.8),("fence",6.3,8.9),("gate",7.5,8.9),
                     ("blue_flag",6.25,7.5),("red_flag",10.7,7.5),("sign",9.55,9.1)],
        "desert":[("sand_rocks",23.3,9.6),("sand_boulders",21.1,8.2),("cactus",23.5,12.6),
                  ("flowering_cactus",19,9.45),("palm",22.4,14.5),("palm",18.25,10.8),
                  ("dry_bush",21.8,10.1),("bones",23.8,14.5)],
        "resources":[("log_bundle",6,5.35),("barrel",8.15,7.6),("crate",7.9,7.75),
                     ("stone_pile",21.9,7.1),("chest",10.2,10.0)],
        "decorations":[("well",9.5,11.25),("campfire",7.8,14.25),("shore_rocks",17.8,13.55)],
    }.items():
        for name,x,y in names:
            at(category,name,x,y)
    at("water","waterfall",14.4,3.0)
    for name,x,y in [("player_right_0",9.9,8.65),("bot_down_0",9.15,10.3),
                     ("player_up_1",19.2,10.6),("bot_left_0",6.4,12.2)]:
        original("characters",name,x,y)
    for y,x,image in sorted(sprites,key=lambda item:item[:2]):
        sheet.alpha_composite(image,(x-image.width//2,y-image.height))
    return sheet


if __name__=="__main__":
    main()



