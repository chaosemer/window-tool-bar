# Window Tool Bar Mode
[![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://github.com/chaosemer/window-tool-bar/actions/workflows/test.yml/badge.svg?branch=main)](https://github.com/chaosemer/window-tool-bar/actions)

Add tool bars to windows.  Keep tool bars clean and useful for
experienced Emacs users.

![A picture is worth a thousand words](images/screenshot.png)

You may really like this package if any of the following is true:

* You like tool bars, but find Emacs' default tool bar not helpful
  because you already know how to do basic commands like open file,
  save file, cut, copy, or paste.
* You use the mouse when "browsing", like in help or info mode.
* You use `mouse-autoselect-window` and would like to use tool bars.
* You like how VSCode does tool bars.

By putting a tool bar in each window, you can see multiple tool bars
simultaneously directly next to the buffer the tool bar acts on.  This
works especially well with in modes such as *info*, *help*, and
*eww*.

## Installation

The easiest way to install window-tool-bar-mode is on GNU Emacs 29 via
`package-vc-install`. Just run `M-x package-vc-install RET
http://github.com/chaosemer/window-tool-bar-mode RET`. This installs
the package locally.

## Customization

The default behavior is to make the per-window tab line show tool bars
if the mode has a custom tool bar.  To enable this behavior either add
`(global-window-tool-bar-mode 1)` to your init file or enable via
customize via `M-x` `customize-group` `RET` `window-tool-bar` `RET`.
If you want to enable for only specific modes, you can add
`(window-tool-bar-mode 1)` to mode specific hooks.

If you want to share the tool bar with an existing tab line, add
`(:eval (window-tool-bar-string))` to `tab-line-format`.  You can also
add this to `mode-line-format` or `header-line-format`.  This is
recommended if you already are using the tab line for other things,
like displaying tabs.

NOTE: Support on text-only displays (terminals) is currently
experimental so you probably want to wrap any enable logic behind a
test for image display:

``` emacs-lisp
(when (display-images-p)
  (global-window-tool-bar-mode 1))
```
