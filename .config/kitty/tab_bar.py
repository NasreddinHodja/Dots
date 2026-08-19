from kitty.tab_bar import DrawData, ExtraData, Screen, TabBarData, as_rgb, color_as_int, draw_title


def draw_tab(
    draw_data: DrawData, screen: Screen, tab: TabBarData, before: int, max_tab_length: int,
    index: int, is_last: bool, extra_data: ExtraData
) -> int:
    if draw_data.leading_spaces:
        screen.draw(' ' * draw_data.leading_spaces)
    draw_title(draw_data, screen, tab, index, max_tab_length)
    trailing_spaces = min(max_tab_length - 1, draw_data.trailing_spaces)
    max_tab_length -= trailing_spaces
    extra = screen.cursor.x - before - max_tab_length
    if extra > 0:
        screen.cursor.x -= extra + 1
        screen.draw('…')
    if trailing_spaces:
        screen.draw(' ' * trailing_spaces)
    end = screen.cursor.x
    screen.cursor.bold = screen.cursor.italic = False
    screen.cursor.fg = 0
    if not is_last:
        # same as kitty's builtin "separator" style, except the gap between
        # pills uses the tab bar's normal background instead of inactive_bg
        screen.cursor.bg = as_rgb(color_as_int(draw_data.default_bg))
        screen.draw(draw_data.sep)
    screen.cursor.bg = 0
    return end
