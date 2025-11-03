#!/usr/bin/env python3

import os

THEME_NAME = "MiniDrac"
BORDER_WIDTH = 3
GAP = 0
OFF_COLOR = "#282a36"
ON_COLOR = "#44475a"

THEMERC_TEMPLATE = """\
title_full_width=false
title_shadow_active=false
title_shadow_inactive=false
active_text_color={on_color}
inactive_text_color={off_color}
frame_border_top={total}
frame_border_left={total}
frame_border_right={total}
frame_border_bottom={total}
maximized_offset
"""

XPM_TEMPLATE = """\
static char *{name}[] = {{
"{width} {height} 2 1 ",
"@ c {color}",
". c None",
{pixels}
}};
"""

BORDER_MAP = {
    'left': ['left-active.xpm', 'left-inactive.xpm'],
    'right': ['right-active.xpm', 'right-inactive.xpm'],
    'bottom': ['bottom-active.xpm', 'bottom-inactive.xpm'],
    'bottom-left': ['bottom-left-active.xpm', 'bottom-left-inactive.xpm'],
    'bottom-right': ['bottom-right-active.xpm', 'bottom-right-inactive.xpm'],
    'top': [f"title-{i}-active.xpm" for i in range(1, 6)] + [f"title-{i}-inactive.xpm" for i in range(1, 6)],
    'top-left': ['top-left-active.xpm', 'top-left-inactive.xpm'],
    'top-right': ['top-right-active.xpm', 'top-right-inactive.xpm'],
}


def write_themerc(path):
    total = BORDER_WIDTH + GAP
    content = THEMERC_TEMPLATE.format(on_color=ON_COLOR, off_color=OFF_COLOR, total=total)
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)


def gen_border_pixels(side):
    total = BORDER_WIDTH + GAP
    lines = []

    for row in range(total):
        line = []
        for col in range(total):
            gap_zone = (
                (side == 'left' and col < GAP)
                or (side == 'right' and col >= BORDER_WIDTH)
                or (side == 'top' and row < GAP)
                or (side == 'bottom' and row >= BORDER_WIDTH)
                or (side == 'top-left' and (col < GAP or row < GAP))
                or (side == 'top-right' and (col >= BORDER_WIDTH or row < GAP))
                or (side == 'bottom-left' and (row >= BORDER_WIDTH or col < GAP))
                or (side == 'bottom-right' and (row >= BORDER_WIDTH or col >= BORDER_WIDTH))
            )
            line.append("." if gap_zone else "@")
        lines.append(f"\"{''.join(line)}\"")

    return ",\n".join(lines) + "\n"


def gen_borders(path):
    total = BORDER_WIDTH + GAP
    os.makedirs(path, exist_ok=True)

    for side, files in BORDER_MAP.items():
        for file in files:
            color = ON_COLOR if "-active" in file else OFF_COLOR
            pixels = gen_border_pixels(side)
            name = os.path.splitext(file)[0]
            content = XPM_TEMPLATE.format(
                name=name,
                width=total,
                height=total,
                color=color,
                pixels=pixels
            )
            try:
                with open(os.path.join(path, file), "w", encoding="utf-8") as f:
                    f.write(content)
            except Exception as e:
                print(f"Error writing {file}: {e}")


def main():
    base_path = os.path.join(os.path.dirname(__file__), THEME_NAME, "xfwm4")
    os.makedirs(base_path, exist_ok=True)

    write_themerc(os.path.join(base_path, "themerc"))
    gen_borders(base_path)

    print(f"Done!")
    print(f"On color: {ON_COLOR}, Off color: {OFF_COLOR}")
    print(f"Border width: {BORDER_WIDTH}px, Gap: {GAP}px, Total: {BORDER_WIDTH + GAP}px")


if __name__ == "__main__":
    main()
