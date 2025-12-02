import os, glob
import subprocess

import libqtile.resources
from libqtile import bar, layout, qtile, widget, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from qtile_extras import widget
from qtile_extras.widget import decorations
from qtile_extras.widget.decorations import BorderDecoration

from modules.dracula import colors
from modules.sysmon import get_sysmon

wallpaper_dir = os.path.expanduser("~/.config/")
wallpaper = None
for ext in ("jpg", "jpeg", "png", "gif", "bmp", "webp"):
    matches = glob.glob(f"{wallpaper_dir}/wp.{ext}")
    if matches:
        wallpaper = matches[0]
        break

outer_gaps = 10
inner_gaps = 5
mod = "mod4"
spawn_cmd = "rofi -show drun"
screen_saver = "xscreensaver -no-splash &"
power_menu = os.path.expanduser("~/.local/bin/power-menu")
window_menu = os.path.expanduser("~/.local/bin/window-menu")
screen_shot = "flameshot gui"

@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser("~/.config/qtile/autostart.sh")
    subprocess.Popen([home])

keys = [
    # https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key(
        [mod, "shift"],
        "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    # Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),
    Key(
        [mod],
        "f",
        lazy.window.toggle_fullscreen(),
        desc="Toggle fullscreen on the focused window",
    ),
    Key([mod], "t", lazy.window.toggle_floating(), desc="Toggle floating on the focused window"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    # Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),

    Key([mod], "comma", lazy.next_screen(), desc="Shutdown Qtile"),

    Key([], "XF86AudioRaiseVolume",
        lazy.spawn("/home/nasreddin/.local/bin/setvol up")),
    Key([], "XF86AudioLowerVolume",
        lazy.spawn("/home/nasreddin/.local/bin/setvol down")),
    Key([], "XF86AudioMute",
        lazy.spawn("/home/nasreddin/.local/bin/setvol mute")),
    Key([], "XF86AudioPlay",
        lazy.spawn("playerctl play-pause")),
    Key([], "XF86AudioNext",
        lazy.spawn("playerctl next")),
    Key([], "XF86AudioPrev",
        lazy.spawn("playerctl previous")),


    Key([mod], "s", lazy.spawn(spawn_cmd), desc="Rofi drun menu"),
    Key([mod], "w", lazy.spawn(window_menu), desc="Rofi window menu"),
    Key([mod, "shift"], "x", lazy.spawn(power_menu), desc="Power menu"),
    Key([mod], "p", lazy.spawn(screen_shot), desc="Screenshot"),
    Key([mod, "shift"], "b", lazy.spawn("/home/nasreddin/.local/bin/pickbg"), desc="Wallpaper picker"),
    Key([mod, "mod1"], "b", lazy.spawn("/home/nasreddin/.local/bin/rand-bg"), desc="Wallpaper picker"),
]

# Add key bindings to switch VTs in Wayland.
# We can't check qtile.core.name in default config as it is loaded before qtile is started
# We therefore defer the check until the key binding is run by using .when(func=...)
for vt in range(1, 8):
    keys.append(
        Key(
            ["control", "mod1"],
            f"f{vt}",
            lazy.core.change_vt(vt).when(func=lambda: qtile.core.name == "wayland"),
            desc=f"Switch to VT{vt}",
        )
    )


groups = [Group(i) for i in "12345678"]

for i in groups:
    keys.extend(
        [
            # mod + group number = switch to group
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc=f"Switch to group {i.name}",
            ),
            # mod + shift + group number = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=False),
                desc=f"Switch to & move focused window to group {i.name}",
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod + shift + group number = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )

layouts = [
    layout.Columns(
        border_focus=colors["purple"],
        border_normal=colors["gray"],
        border_focus_stack=colors["purple"],
        border_normal_stack=colors["gray"],
        border_width=3,
        margin=inner_gaps,
        margin_on_single=inner_gaps,
        single_border_width=3,
        border_on_single=True,
    ),
    layout.Max(
        margin=outer_gaps,
    ),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(
    #     num_stacks=1,
    #     border_width=3,
    #     border_focus=colors["purple"],
    #     border_normal=colors["bg"],
    #     margin=inner_gaps,
    #     single_border_width=3,
    #     single_margin=8
    # )
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="Source Code Pro",
    fontsize=12,
    padding=4,
    foreground=colors["fg"],
    background=colors["bg_dark"],
)
sep_defaults = dict(
    foreground=colors["fg"],
    background=colors["bg_dark"],
    linewidth=1,
    padding=16,
    size_percent=45,
)
sep_blank_defaults = dict(
    foreground=colors["bg_dark"],
    background=colors["bg_dark"],
    linewidth=1,
    padding=10,
    size_percent=50,
)
extension_defaults = widget_defaults.copy()

logo = os.path.join(os.path.dirname(libqtile.resources.__file__), "logo.png")
screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.GroupBox(
                    padding_x=4,
                    padding_y=2,
                    margin_x=4,

                    background=colors["bg_dark"],
                    foreground=colors["fg"],
                    active=colors["fg"],
                    inactive=colors["gray"],

                    highlight_method="block",
                    highlight_color=[colors["bg"], colors["bg"]],
                    this_current_screen_border=colors["fg"],
                    this_screen_border=colors["fg"],
                    other_current_screen_border=colors["purple"],
                    other_screen_border=colors["purple"],

                    block_highlight_text_color=colors["bg_dark"],

                    rounded=False,
                    borderwidth=0,
                ),
                widget.Sep(**sep_blank_defaults),
                widget.Prompt(),
                widget.WindowName(),
                widget.Chord(
                    chords_colors={
                        "launch": ("#ff0000", "#ffffff"),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                # NB Systray is incompatible with Wayland, consider using StatusNotifier instead
                # widget.StatusNotifier(),
                widget.Sep(**sep_blank_defaults),
                widget.Systray(icon_size=14),
                widget.Sep(**sep_blank_defaults),
                widget.CurrentLayout(
                    decorations=[
                        BorderDecoration(
                            border_width=4,
                            colour=colors["bg_dark"],
                        ),
                    ],
                    foreground=colors["fg"],
                    background=colors["gray"],
                ),
                widget.Sep(foreground=colors["bg_dark"]),
                widget.GenPollText(
                    func=lambda: get_sysmon(0),
                    update_interval=3,
                    foreground=colors["fg"],
                ),
                widget.Sep(**sep_defaults),
                widget.GenPollText(
                    func=lambda: get_sysmon(1),
                    update_interval=3,
                    foreground=colors["fg"],
                ),
                widget.Sep(**sep_defaults),
                widget.GenPollText(
                    func=lambda: get_sysmon(2),
                    update_interval=3,
                    foreground=colors["fg"],
                ),
                widget.Sep(**sep_defaults),
                widget.PulseVolume(
                    fmt='󰕾 {}',
                ),
                widget.Sep(**sep_defaults),
                widget.Clock(format="%Y-%m-%d %a %I:%M %p"),
                widget.Sep(foreground=colors["bg_dark"], background=colors["bg_dark"]),
            ],
            24,
            margin=[inner_gaps,
                    360,
                    inner_gaps,
                    360],
            padding=4
        ),
        right=bar.Gap(outer_gaps),
        top=bar.Gap(outer_gaps),
        left=bar.Gap(outer_gaps),
    ),
    Screen(
        bottom=bar.Gap(outer_gaps),
        right=bar.Gap(outer_gaps),
        top=bar.Gap(outer_gaps),
        left=bar.Gap(outer_gaps),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ],
    border_focus=colors["purple"],
    border_normal=colors["bg"],
    border_width=3,
)
auto_fullscreen = True
focus_on_window_activation = "smart"
focus_previous_on_window_remove = False
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# xcursor theme (string or None) and size (integer) for Wayland backend
wl_xcursor_theme = None
wl_xcursor_size = 24

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
