# Window Tool Bar Mode
[![GNU Emacs](https://img.shields.io/badge/Part_of-GNU_Emacs-7F5AB6.svg?&logo=gnu-emacs&logoColor=white)](https://www.gnu.org/software/emacs)
[![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://github.com/chaosemer/window-tool-bar/actions/workflows/test.yml/badge.svg?branch=main)](https://github.com/chaosemer/window-tool-bar/actions)
[![GNU ELPA](https://elpa.gnu.org/packages/window-tool-bar.svg)](https://elpa.gnu.org/packages/window-tool-bar)


This package is a part of GNU Emacs.

Add tool bars to windows.  Keep tool bars clean and useful for
experienced Emacs users.  Tool bars for GUIs and Terminals.

![Screenshot of the window tool bar](images/screenshot.png)

You may really like this package if any of the following is true:

* You like tool bars, but find Emacs' default tool bar not helpful
  because you already know how to do basic commands like open file,
  save file, cut, copy, or paste.
* You use the mouse when "browsing", like in help or info mode.
* You use `mouse-autoselect-window` and would like to use tool bars.
* You like how VSCode does tool bars.

This package puts a tool bar in each window.  This allows you to see
multiple tool bars simultaneously directly next to the buffer it acts
on which feels much more intuitive.  Emacs "browsing" modes generally
have sensible tool bars, for example: \*info\*, \*Help\*, and \*eww\*
have them.

It does this while being mindful of screen real estate.  If
`tool-bar-map` is nil, then this package will not take up any space
for an empty tool bar.  Most modes do not define a custom tool bar, so
calling `(setq tool-bar-map nil)` in your init file will make most
buffers not take up space for a tool bar.

## Installation

Window Tool Bar is included in Emacs 30 and higher. For older Emacsen,
installing is as simple as `M-x` `package-install` `RET`
`window-tool-bar`.  Additionally, on GNU Emacs 29 and higher, you can
run `M-x` `package-vc-install` `RET`
`http://github.com/chaosemer/window-tool-bar-mode` to install directly
from source.

## Customization

The default behavior is to make the per-window tab line show the tool
bar for each window's buffer.  To enable this, add
`(global-window-tool-bar-mode)` to your init file or enable via `M-x`
`customize-group` `RET` `window-tool-bar` `RET`.  If you want to
enable the window tool bar for only specific modes, you can add
`window-tool-bar-mode` to mode specific hooks.

It is common to only want to show mode specific tool bars and not the
default tool bar.  To do this, add `(setq tool-bar-map nil)` to your
init file.  This is the configuration the author of this package uses.

If you want to share space with an existing tab line, mode line, or
header line, add `(:eval (window-tool-bar-string))` to
`tab-line-format`, `mode-line-format`, or `header-line-format`.

For additional documentation, see info node [(emacs)Window Tool
Bar](https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Tool-Bar.html).

## Gallery

### Emacs default GUI
![Emacs default screenshot](images/emacs_default.png)

### Terminal (iTerm2)
![iTerm2 screenshot](images/iterm2.png)

### Modus
![Modus Operandi screenshot](images/modus_operandi.png)

![Modus Vivendi screenshot](images/modus_vivendi.png)

### Doom
![Doom Nova](images/doom_nova.png)

![Doom Monokai Classic](images/doom_monokai.png)

![Doom Homage White](image/doom_homage.png)

### Zenburn
![Zenburn](images/zenburn.png)

### Spacemacs
![Spacemacs Light](image/spacemacs_light.png)

![Spacemacs Dark](image/spacemacs_dark.png)

### Solarized
![Solarized Light](image/solarized_light.png)

![Solarized Dark](image/solarized_dark.png)
