
from PIL import Image, ImageDraw
from pathlib import Path
import random

folder = Path("resources/images")
folder.mkdir(parents=True, exist_ok=True)

SIZE = 44

def generate_tile(filename, base_color, variations):
    random.seed(42)
    image = Image.new("RGB", (SIZE, SIZE), base_color)
    draw = ImageDraw.Draw(image)

    # Добавляем мелкие пиксельные детали
    for _ in range(90):
        x = random.randint(0, SIZE - 2)
        y = random.randint(0, SIZE - 2)
        color = random.choice(variations)
        draw.rectangle((x, y, x + 1, y + 1), fill=color)

    image.save(folder / filename)
    print(f"{filename}: создан")

# Трава
generate_tile(
    "grass.png",
    (112, 151, 76),
    [(99, 139, 65), (126, 162, 85), (88, 130, 60)]
)

# Земля
generate_tile(
    "dirt.png",
    (151, 111, 70),
    [(135, 94, 58), (169, 125, 79), (120, 83, 51)]
)

print("Текстуры готовы!")
