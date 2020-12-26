# -*- coding: utf-8 -*-
# autostart = true

import re
from xkeysnail.transform import *

# These things should work anywhere
define_modmap({
    Key.CAPSLOCK: Key.LEFT_CTRL
})

# Super should work like control in in most applications
# define_conditional_modmap(
#         lambda wm_class: wm_class not in ['Emacs', 'Gnome-terminal'], {
#             Key.CAPSLOCK: Key.LEFT_CTRL,
#             Key.LEFT_META: Key.LEFT_CTRL,
#             })

define_keymap(
        lambda wm_class: wm_class not in ['Emacs', 'Gnome-terminal'], {
            K("Super-Backspace"): K("C-Backspace"),
            K("Super-a"): K("C-a"),
            K("Super-b"): K("C-b"),
            K("Super-c"): K("C-c"),
            K("Super-d"): K("C-d"),
            K("Super-e"): K("C-e"),
            K("Super-f"): K("C-f"),
            K("Super-g"): K("C-g"),
            # K("Super-h"): K("C-h"),
            K("Super-i"): K("C-i"),
            # K("Super-j"): K("C-j"),
            # K("Super-k"): K("C-k"),
            # K("Super-l"): K("C-l"),
            K("Super-m"): K("C-m"),
            K("Super-n"): K("C-n"),
            K("Super-o"): K("C-o"),
            K("Super-p"): K("C-p"),
            K("Super-q"): K("C-q"),
            K("Super-r"): K("C-r"),
            K("Super-s"): K("C-s"),
            K("Super-t"): K("C-t"),
            K("Super-u"): K("C-u"),
            K("Super-v"): K("C-v"),
            K("Super-w"): K("C-w"),
            K("Super-x"): K("C-x"),
            K("Super-y"): K("C-y"),
            K("Super-x"): K("C-x"),
            K("Super-Shift-a"): K("C-Shift-a"),
            K("Super-Shift-b"): K("C-Shift-b"),
            K("Super-Shift-c"): K("C-Shift-c"),
            K("Super-Shift-d"): K("C-Shift-d"),
            K("Super-Shift-e"): K("C-Shift-e"),
            K("Super-Shift-f"): K("C-Shift-f"),
            K("Super-Shift-g"): K("C-Shift-g"),
            K("Super-Shift-h"): K("C-Shift-h"),
            K("Super-Shift-i"): K("C-Shift-i"),
            K("Super-Shift-j"): K("C-Shift-j"),
            K("Super-Shift-k"): K("C-Shift-k"),
            K("Super-Shift-l"): K("C-Shift-l"),
            K("Super-Shift-m"): K("C-Shift-m"),
            K("Super-Shift-n"): K("C-Shift-n"),
            K("Super-Shift-o"): K("C-Shift-o"),
            K("Super-Shift-p"): K("C-Shift-p"),
            K("Super-Shift-q"): K("C-Shift-q"),
            K("Super-Shift-r"): K("C-Shift-r"),
            K("Super-Shift-s"): K("C-Shift-s"),
            K("Super-Shift-t"): K("C-Shift-t"),
            K("Super-Shift-u"): K("C-Shift-u"),
            K("Super-Shift-v"): K("C-Shift-v"),
            K("Super-Shift-w"): K("C-Shift-w"),
            K("Super-Shift-x"): K("C-Shift-x"),
            K("Super-Shift-y"): K("C-Shift-y"),
            K("Super-Shift-x"): K("C-Shift-x"),
            })

# UI keybindings that should work everywhere
define_keymap(None, {
    K("Super-Space"): K("Alt-F1"),
    K("Super-C-h"): K("Super-Left"),
    K("Super-C-l"): K("Super-Right"),
    K("Super-C-u"): K("Super-Up"),
    K("Super-h"): K("Super-Page_Up"),
    K("Super-l"): K("Super-Page_Down"),
    K("Super-k"): K("Super-Page_Up"),
    K("Super-j"): K("Super-Page_Down"),
    K("Super-Enter"): K("F11"),
    }, "ui keymap")


# Terminal Keybindings
define_keymap(
        lambda wm_class: wm_class in ['Gnome-terminal'], {
            K("Super-c"): K("C-Shift-c"),
            K("Super-v"): K("C-Shift-v"),
            K("Super-t"): K("C-Shift-t"),
            K("Super-w"): K("C-Shift-w"),
            K("Super-q"): K("C-Shift-q"),
            K("Super-n"): K("C-Shift-n"),
            }, "terminals")
