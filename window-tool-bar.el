;;; window-tool-bar.el --- Add tool bars inside windows -*- lexical-binding: t -*-

;; Copyright 2023 Jared Finder
;; Author: Jared Finder <jared@finder.org>
;; Created: Nov 21, 2023
;; Version: 0.1-beta
;; Keywords: mouse
;; URL: http://github.com/chaosemer/window-tool-bar
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is in a very early state.  The eventual goals are as follows:
;;
;; * Add tool bars to windows instead of frames.
;; * Clean up the default tool bar bindings.
;; * Make tool bars work in terminals.

;;; Todo:
;;
;; Remaining work to hit 1.0:
;;
;; * Play with this on for a bit.
;; * Upload to MELPA.
;;
;; Post 1.0 work:
;;
;; * Properly support button labels
;; * Make this work on non-graphical frames.
;; * Show keyboard shortcut on help text.
;; * Clean up Emacs toolbars
;;     * Default: Remove defaul tool-bar
;;     * grep, vc: Remove default tool-bar inherited
;;     * info: Remove Next / Prev / Up, which is already in the header
;;     * smerge: Add toolbar for next/prev
;; * Add utility function to clean up Emacs toolbars (Info, grep)
;;
;; * Add a bit more documentation.
;; * Add customization option: ignore-default-tool-bar-map
;; * Make tab-line dragging resize the window

;;; Code:

(require 'mwheel)
(require 'tab-line)

(defgroup window-tool-bar nil
  "Tool bars per-window."
  :group 'convenience)

(defvar-keymap window-tool-bar--button-keymap
  :doc "Keymap used by `window-tool-bar--keymap-entry-to-string'."
  "<follow-link>" 'mouse-face
  ;; Follow link on all clicks of mouse-1 and mouse-2 since the tool
  ;; bar is not a place the point can travel to.
  "<tab-line> <mouse-1>" #'window-tool-bar--call-button
  "<tab-line> <double-mouse-1>" #'window-tool-bar--call-button
  "<tab-line> <triple-mouse-1>" #'window-tool-bar--call-button
  "<tab-line> <mouse-2>" #'window-tool-bar--call-button
  "<tab-line> <double-mouse-2>" #'window-tool-bar--call-button
  "<tab-line> <triple-mouse-2>" #'window-tool-bar--call-button

  ;; Mouse down events do nothing.  A binding is needed so isearch
  ;; does not exit when the tab bar is clicked.
  "<tab-line> <down-mouse-1>" #'window-tool-bar--ignore
  "<tab-line> <double-down-mouse-1>" #'window-tool-bar--ignore
  "<tab-line> <triple-down-mouse-1>" #'window-tool-bar--ignore
  "<tab-line> <down-mouse-2>" #'window-tool-bar--ignore
  "<tab-line> <double-down-mouse-2>" #'window-tool-bar--ignore
  "<tab-line> <triple-down-mouse-2>" #'window-tool-bar--ignore)
(fset 'window-tool-bar--button-keymap window-tool-bar--button-keymap) ; So it can be a keymap property

;; Register bindings that stay in isearch.  Technically, these
;; commands don't pop up a menu but they act very similar in that they
;; end up calling an actual command via `call-interactively'.
(push 'window-tool-bar--call-button isearch-menu-bar-commands)
(push 'window-tool-bar--ignore isearch-menu-bar-commands)

(defvar-local window-tool-bar-string--cache nil
  "Cache for previous result of `window-tool-bar-string'.")

;;;###autoload
(defun window-tool-bar-string ()
  "Return a (propertized) string for the tool bar.

This is for when you want more customizations than
`window-tool-bar-mode' provides.  Commonly added to the variable
`tab-line-format', `header-line-format', or `mode-line-format'"
  (when (or (null window-tool-bar-string--cache)
            (window-tool-bar--last-command-triggers-refresh-p))
    (let ((toolbar-menu (cdr (keymap-global-lookup "<tool-bar>"))))
      (setf window-tool-bar-string--cache
            (mapconcat #'window-tool-bar--keymap-entry-to-string
                       toolbar-menu
                       ;; Without spaces between the text, hovering
                       ;; highlights all adjacent buttons.
                       (propertize " " 'invisible t)))))

  window-tool-bar-string--cache)

(defun window-tool-bar--keymap-entry-to-string (menu-item)
  "Convert MENU-ITEM into a (propertized) string representation.

MENU-ITEM: Menu item to convert.  See info node (elisp)Tool Bar."
  (pcase menu-item
    ;; Separators
    ((or `(,_ "--")
         `(,_ menu-item ,(and (pred stringp)
                              (pred (string-prefix-p "--")))))
     "|")

    ;; Menu item, turn into propertized string button
    (`(,key menu-item ,name-expr ,binding . ,_)
     (when binding      ; If no binding exists, then button is hidden.
       (let* ((name (eval name-expr))
              (str (format "[%s]" (eval name-expr)))
              (len (length str))
              (enable-form (plist-get menu-item :enable))
              (enabled (or (not enable-form)
                           (eval enable-form))))
         (when enabled
           (add-text-properties 0 len
                                '(mouse-face tab-line-highlight
                                  keymap window-tool-bar--button-keymap)
                                str))
         (when-let ((spec (plist-get menu-item :image)))
           (put-text-property 0 len
                              'display
                              (append spec
                                      (if enabled '(:margin 2)
                                        '(:margin 2 :conversion disabled)))
                              str))
         (put-text-property 0 len
                            'help-echo
                            (or (plist-get menu-item :help) name)
                            str)
         (put-text-property 0 len 'tool-bar-key key str)
         str)))))

(defun window-tool-bar--call-button ()
  "Call the button that was clicked on in the tab line."
  (interactive)
  (when (mouse-event-p last-command-event)
    (let ((posn (event-start last-command-event)))
      (with-current-buffer (window-buffer (posn-window posn))
        (let* ((str (posn-string posn))
               (key (get-text-property (cdr str) 'tool-bar-key (car str)))
               (cmd (lookup-key global-map (vector 'tool-bar key))))
          (call-interactively cmd))))))

(defun window-tool-bar--ignore ()
  "Do nothing.  This command exists for isearch."
  (interactive)
  nil)

(defvar window-tool-bar--ignored-event-types
  (list 'mouse-movement
        mouse-wheel-up-event mouse-wheel-up-alternate-event
        mouse-wheel-down-event mouse-wheel-down-alternate-event
        mouse-wheel-left-event mouse-wheel-left-alternate-event
        mouse-wheel-right-event mouse-wheel-right-alternate-event
        'pinch)
  "Cache for `window-tool-bar--last-command-triggers-refresh-p'.")

(defun window-tool-bar--last-command-triggers-refresh-p ()
  "Test if the recent command or event should trigger a tool bar refresh."
  (let ((type (event-basic-type last-command-event)))
    (and
     ;; Assume that key presses and button presses are the only user
     ;; interactions that can alter the tool bar.  Specifically, this
     ;; excludes mouse movement, mouse wheel scroll, and pinch.
     (not (member type window-tool-bar--ignored-event-types))
     ;; Assume that any command that triggers shift select can't alter
     ;; the tool bar.  This excludes pure navigation commands.
     (not (window-tool-bar--command-triggers-shift-select-p last-command))
     ;; Assume that self-insert-command won't alter the tool bar.
     ;; This is the most commonly executed command.
     (not (eq last-command 'self-insert-command)))))

(defun window-tool-bar--command-triggers-shift-select-p (command)
  "Test if COMMAND would trigger shift select."
  (let* ((form (interactive-form command))
         (spec (car-safe (cdr-safe form))))
    (and (eq (car-safe form) 'interactive)
         (stringp spec)
         (seq-position spec ?^))))

;;;###autoload
(define-minor-mode window-tool-bar-mode
  "Toggle display of the toolbar in the tab line of the current buffer."
  :lighter nil
  (if (and window-tool-bar-mode
           (not (eq tool-bar-map (default-value 'tool-bar-map))))
      (setq tab-line-format '(:eval (window-tool-bar-string)))
    (setq tab-line-format nil)))

(defun window-tool-bar--turn-on ()
  "Internal function called by `global-window-tool-bar-mode'."
  (when global-window-tool-bar-mode
    (window-tool-bar-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-window-tool-bar-mode
  window-tool-bar-mode window-tool-bar--turn-on
  :group 'window-tool-bar
  (add-hook 'isearch-mode-hook #'window-tool-bar--turn-on)
  (add-hook 'isearch-mode-end-hook #'window-tool-bar--turn-on))

(provide 'window-tool-bar)

;;; window-tool-bar.el ends here
