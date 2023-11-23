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
;; * Optimize allocations in `window-tool-bar-string' by having it
;;   instead return a list of strings, so only items that can become
;;   disabled are recalculated.
;; * Upload to MELPA.
;;
;; Post 1.0 work:
;;
;; * Properly support button labels
;; * Make this work on non-graphical frames.
;; * Add utility function to clean up Emacs toolbars (Info, grep)
;;
;; * Add a bit more documentation.
;; * Add customization option: ignore-default-tool-bar-map
;; * Make tab-line dragging resize the window

;;; Code:

(require 'tab-line)

(defgroup window-tool-bar nil
  "Tool bars per-window."
  :group 'convenience)

;;;###autoload
(defun window-tool-bar-string ()
  "Return a (propertized) string for the tool bar.

This is for when you want more customizations than
`window-tool-bar-mode' provides.  Commonly added to the variable
`tab-line-format', `header-line-format', or `mode-line-format'"
  (let ((toolbar-menu (cdr (keymap-global-lookup "<tool-bar>"))))
    (mapconcat #'window-tool-bar--keymap-entry-to-string toolbar-menu
               ;; Without spaces between the text, hovering highlights
               ;; all adjacent buttons.
               (propertize " " 'invisible t))))

(defun window-tool-bar--keymap-entry-to-string (menu-item)
  "Convert MENU-ITEM into a (propertized) string representation.

MENU-ITEM: Menu item to convert.  See info node (elisp)Tool Bar."
  (pcase menu-item
    (`(,key menu-item ,name . ,_)
     ;; Normal menu item, turn into propertized string button
     (let* ((str (format "[%s]" name))
            (enable-form (plist-get menu-item :enable))
            (enabled (or (not enable-form)
                         (eval enable-form))))
       (setq str (apply #'propertize
                        (append (list str)
                                (when enabled
                                  `(mouse-face tab-line-highlight
                                               keymap ,(define-keymap
                                                         "<follow-link>" 'mouse-face
                                                         "<tab-line> <mouse-2>" #'window-tool-bar--call-button))))))
       (when-let ((spec (plist-get menu-item :image)))
         (setq str (propertize str
                               'display (append spec
                                                '(:margin 2)
                                                (unless enabled '(:conversion disabled))))))
       (when-let ((spec (plist-get menu-item :help)))
         (setq str (propertize str
                               'help-echo spec)))
       (setq str (propertize str
                             'tool-bar-key key))
       str))
    (`(,_ "--")
     "|")))

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
  (window-tool-bar-mode 1))

;;;###autoload
(define-globalized-minor-mode global-window-tool-bar-mode
  window-tool-bar-mode window-tool-bar--turn-on
  :group 'window-tool-bar
  (add-hook 'isearch-mode-hook #'window-tool-bar--turn-on)
  (add-hook 'isearch-mode-end-hook #'window-tool-bar--turn-on))

(provide 'window-tool-bar)

;;; window-tool-bar.el ends here
