# -*- coding: utf-8 -*-
# autostart = true

import re
from xkeysnail.transform import *

terminals = ["gnome-terminal"]
term_str = "|".join(str(x) for x in terminals)

browsers = ["firefox"]
browser_str = "|".join(str(x) for x in browsers)

define_modmap({
    Key.CAPSLOCK: Key.LEFT_CTRL
})

define_keymap(re.compile(term_str, re.IGNORECASE), {
    # Converts Cmd to use Ctrl-Shift
    K("Super-c"): K("C-Shift-c"),
    K("Super-v"): K("C-Shift-v"),
    K("Super-t"): K("C-Shift-t"),
    K("Super-w"): K("C-Shift-w"),
    K("Super-q"): K("C-Shift-q"),
    K("Super-n"): K("C-Shift-n"),
}, "terminals")

# Keybindings for Firefox/Chrome
define_keymap(re.compile(browser_str, re.IGNORECASE), {
    K("Super-Shift-p"): K("C-Shift-p"),
    K("Super-r"): K("C-r"),
}, "browsers")

define_keymap(None, {
    K("Super-Space"): K("Alt-F1"),
    K("Super-C-h"): K("Super-Left"),
    K("Super-C-l"): K("Super-Right"),
    K("Super-C-u"): K("Super-Up"),
    K("Super-h"): K("Super-Page_Up"),
    K("Super-l"): K("Super-Page_Down"),
    K("Super-k"): K("Super-Page_Up"),
    K("Super-j"): K("Super-Page_Down"),
    # K("Super-C-k"): K("Super-Shift-Page_Up"),
    # K("Super-C-j"): K("Super-Shift-Page_Down"),
    K("Super-Enter"): K("Super-Shift-End"),
    K("Super-c"): K("C-c"),
    K("Super-v"): K("C-v"),
    K("Super-t"): K("C-t"),
    K("Super-w"): K("C-w"),
    K("Super-q"): K("C-q"),
    K("Super-n"): K("C-n"),
    K("Super-f"): K("C-f"),
})
