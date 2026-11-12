
from pathlib import Path
from PIL import Image

folder = Path("resources/images")

for name in ["tree.png", "stone.png", "house.png"]:
    path = folder / name

    with Image.open(path) as source:
        image = source.convert("RGBA")

    # Убираем полностью прозрачные поля
    bounds = image.getbbox()
    if bounds:
        image = image.crop(bounds)

    # Масштабируем с сохранением пропорций
    image.thumbnail((40, 40), Image.Resampling.NEAREST)

    # Размещаем по центру прозрачной клетки 44x44
    result = Image.new("RGBA", (44, 44), (0, 0, 0, 0))
    x = (44 - image.width) // 2
    y = (44 - image.height) // 2
    result.paste(image, (x, y), image)

    result.save(path)
    print(f"{name}: исправлено")

print("Готово!")
