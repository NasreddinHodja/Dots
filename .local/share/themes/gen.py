#!/usr/bin/env python3

import os

themerc = """title_full_width=false
title_shadow_active=false
title_shadow_inactive=false
active_text_color=%s
inactive_text_color=%s
frame_border_top=%d
frame_border_left=%d
frame_border_right=%d
frame_border_bottom=%d
maximized_offset"""

xpm = '''/* XPM */
static char *bottom_right_inactive[] = {
/* columns rows colors chars-per-pixel */
"%d %d 2 1 ",
"@ c %s",
". c None",
/* pixels */
%s
};'''

def getInput():
    return "MiniDrac", 3, 0, "#282a36", "#44475a"

def genRC(path, width, gap, onColor, offColor):
    try:
        total_width = width + gap
        open(path, "wt").write(themerc % (onColor, offColor, total_width, total_width, total_width, total_width))
    except Exception as e:
        print("Error: Failed to create themerc")
        print(e)

def genBorderPixels(width, gap, gap_side):
    """
    gap_side can be: 'left', 'right', 'top', 'bottom', 'none'
    """
    total = width + gap
    pixels = ""

    for row in range(total):
        line = "\""
        for col in range(total):
            is_gap = False

            if gap_side == 'left' and col < gap:
                is_gap = True
            elif gap_side == 'right' and col >= width:
                is_gap = True
            elif gap_side == 'top' and row < gap:
                is_gap = True
            elif gap_side == 'bottom' and row >= width:
                is_gap = True
            elif gap_side == 'top-left' and (col < gap or row < gap):
                is_gap = True
            elif gap_side == 'top-right' and (col >= width or row < gap):
                is_gap = True
            elif gap_side == 'bottom-left' and (row >= width or col < gap):
                is_gap = True
            elif gap_side == 'bottom-right' and (row >= width or col >= width):
                is_gap = True

            line += "." if is_gap else "@"

        line += "\""
        if row < total - 1:
            line += ",\n"
        else:
            line += "\n"
        pixels += line

    return pixels

def genBorders(path, width, gap, onColor, offColor):
    total = width + gap

    border_map = {
        'left-active.xpm': 'left',
        'left-inactive.xpm': 'left',
        'right-active.xpm': 'right',
        'right-inactive.xpm': 'right',
        'bottom-active.xpm': 'bottom',
        'bottom-inactive.xpm': 'bottom',
        'bottom-left-active.xpm': 'bottom-left',
        'bottom-left-inactive.xpm': 'bottom-left',
        'bottom-right-active.xpm': 'bottom-right',
        'bottom-right-inactive.xpm': 'bottom-right',
        'title-1-active.xpm': 'top',
        'title-1-inactive.xpm': 'top',
        'title-2-active.xpm': 'top',
        'title-2-inactive.xpm': 'top',
        'title-3-active.xpm': 'top',
        'title-3-inactive.xpm': 'top',
        'title-4-active.xpm': 'top',
        'title-4-inactive.xpm': 'top',
        'title-5-active.xpm': 'top',
        'title-5-inactive.xpm': 'top',
        'top-left-active.xpm': 'top-left',
        'top-left-inactive.xpm': 'top-left',
        'top-right-active.xpm': 'top-right',
        'top-right-inactive.xpm': 'top-right',
    }

    for file, gap_side in border_map.items():
        try:
            color = onColor if file.find("-active") > 0 else offColor
            pixels = genBorderPixels(width, gap, gap_side)
            print(gap_side)
            print(pixels)
            open(path + file, "wt").write(xpm % (total, total, color, pixels))
        except Exception as e:
            print("Error: Failed to write file " + file)
            print(e)

def main():
    name, width, gap, offColor, onColor = getInput()
    path = os.path.dirname(__file__) + '/' + name + "/xfwm4/"
    try:
        os.makedirs(path)
    except:
        print("Theme exists! Overwriting!")
    genRC(path + "themerc", width, gap, onColor, offColor)
    genBorders(path, width, gap, onColor, offColor)
    print("Done! Border width: %d pixels, Gap size: %d pixels, Total: %d pixels" % (width, gap, width + gap))

if __name__ == "__main__":
    main()
