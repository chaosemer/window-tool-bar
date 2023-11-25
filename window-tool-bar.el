;;; window-tool-bar.el --- Add tool bars inside windows -*- lexical-binding: t -*-

;; Copyright 2023 Jared Finder
;; Author: Jared Finder <jared@finder.org>
;; Created: Nov 21, 2023
;; Version: 0.1
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
;; This package puts a tool bar in each window.  This allows you to see
;; multiple tool bars simultaneously directly next to the buffer it
;; acts on which feels much more intuitive.  Emacs "browsing" modes
;; generally have sensible tool bars, for example: *info*, *help*, and
;; *eww* have them.
;;
;; It does this while being mindful of screen real estate.  Most modes
;; do not provide a custom tool bar, and this package does not show the
;; default tool bar.  This means that for most buffers there will be no
;; space taken up.  Furthermore, you can put this tool bar in the mode
;; line or tab line if you want to share it with existing content.
;;
;; To get the default behavior, run (global-window-tool-bar-mode 1) or
;; enable via M-x customize-group RET window-tool-bar RET.  This uses
;; the per-window tab line to show the tool bar.
;;
;; If you want to share space with an existing tab line, mode line, or
;; header line, add (:eval (window-tool-bar-string)) to
;; `tab-line-format', `mode-line-format', or `header-line-format'.

;;; Todo:
;;
;; Not all features planned are implemented yet.  Eventually I would
;; like to also generally make tool bars better.
;;
;; Targeting 0.2:
;;
;; * Properly support button labels
;; * Make this work on non-graphical frames.
;;
;; Targeting 1.0:
;;
;; * Clean up Emacs tool bars
;;     * Default: Remove default tool-bar entirely
;;     * grep, vc: Remove default tool-bar inherited
;;     * info: Remove Next / Prev / Up, which is already in the header
;;     * smerge: Add tool bar for next/prev
;;
;; Post 1.0 work:
;;
;; * Show keyboard shortcut on help text.
;;
;; * Add a bit more documentation.
;; * Add customization option: ignore-default-tool-bar-map
;; * Make tab-line dragging resize the window

;;; Code:

(require 'mwheel)
(require 'tab-line)

;;; Benchmarking code
;;
;; Refreshing the tool bar is computationally simple, but generates a
;; lot of garbage.  So this benchmarking focuses on garbage
;; generation.  Since it has to run after most commands, generating
;; significantly more garbage will cause noticeable performance
;; degration.
;;
;; The refresh has two steps:
;;
;; Step 1: Look up the <tool-bar> map.
;; Step 2: Generate a Lisp string using text properties for the tool
;; bar string.
;;
;; Additionally, we keep track of the percentage of commands that
;; acutally created a refresh.
(defvar window-tool-bar--memory-use-delta-step1 (make-list 7 0)
  "Absolute delta of memory use counters during step 1.
This is a list in the same structure as `memory-use-counts'.")
(defvar window-tool-bar--memory-use-delta-step2 (make-list 7 0)
  "Absolute delta of memory use counters during step 2.
This is a list in the same structure as `memory-use-counts'.")
(defvar window-tool-bar--refresh-done-count 0
  "Number of tool bar string refreshes run.
The total number of requests is the sum of this and
`window-tool-bar--refresh-skipped-count'.")
(defvar window-tool-bar--refresh-skipped-count 0
  "Number of tool bar string refreshes that were skipped.
The total number of requests is the sum of this and
`window-tool-bar--refresh-done-count'.")

(defun window-tool-bar--memory-use-avg-step1 ()
  "Return average memory use delta during step 1."
  (mapcar (lambda (elt) (/ elt window-tool-bar--refresh-done-count 1.0))
          window-tool-bar--memory-use-delta-step1))

(defun window-tool-bar--memory-use-avg-step2 ()
  "Return average memory use delta during step 2."
  (mapcar (lambda (elt) (/ elt window-tool-bar--refresh-done-count 1.0))
          window-tool-bar--memory-use-delta-step2))

(declare-function time-stamp-string "time-stamp")

(defun window-tool-bar-show-memory-use ()
  "Pop up a window showing the memory use metrics."
  (interactive)
  (require 'time-stamp)
  (save-selected-window
    (pop-to-buffer "*WTB Memory Report*")
    (unless (eq major-mode 'special-mode)
      (special-mode))

    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert (propertize (concat "Function: window-tool-bar-string "
                                  (time-stamp-string))
                          'face 'underline 'font-lock-face 'underline)
              "\n\n")
      (window-tool-bar--insert-memory-use "Step 1" (window-tool-bar--memory-use-avg-step1))
      (window-tool-bar--insert-memory-use "Step 2" (window-tool-bar--memory-use-avg-step2))
      (insert (format "Refresh count  %d\n" window-tool-bar--refresh-done-count)
              (format "Refresh executed percent %.2f\n"
                      (/ window-tool-bar--refresh-done-count
                         (+ window-tool-bar--refresh-done-count window-tool-bar--refresh-skipped-count)
                         1.0))
              "\n"))))

(defun window-tool-bar--insert-memory-use (label avg-memory-use)
  "Insert memory use into current buffer.

LABEL: A prefix string to be in front of the data.
AVG-MEMORY-USE: A list of averages, with the same meaning as
  `memory-use-counts'."
  (let* ((label-len (length label))
         (padding (make-string label-len ?\s)))
    (insert (format "%s  %8.2f Conses\n" label (elt avg-memory-use 0)))
    (insert (format "%s  %8.2f Floats\n" padding (elt avg-memory-use 1)))
    (insert (format "%s  %8.2f Vector cells\n" padding (elt avg-memory-use 2)))
    (insert (format "%s  %8.2f Symbols\n" padding (elt avg-memory-use 3)))
    (insert (format "%s  %8.2f String chars\n" padding (elt avg-memory-use 4)))
    (insert (format "%s  %8.2f Intervals\n" padding (elt avg-memory-use 5)))
    (insert (format "%s  %8.2f Strings\n" padding (elt avg-memory-use 6)))))

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
  (if (or (null window-tool-bar-string--cache)
          (window-tool-bar--last-command-triggers-refresh-p))
      (let* ((mem0 (memory-use-counts))
             (toolbar-menu (cdr (keymap-global-lookup "<tool-bar>")))
             (mem1 (memory-use-counts))
             (result (mapconcat #'window-tool-bar--keymap-entry-to-string
                                toolbar-menu
                                ;; Without spaces between the text, hovering
                                ;; highlights all adjacent buttons.
                                (propertize " " 'invisible t)))
             (mem2 (memory-use-counts)))
        (cl-mapl (lambda (l-init l0 l1)
                   (cl-incf (car l-init) (- (car l1) (car l0))))
                 window-tool-bar--memory-use-delta-step1 mem0 mem1)
        (cl-mapl (lambda (l-init l1 l2)
                   (cl-incf (car l-init) (- (car l2) (car l1))))
                 window-tool-bar--memory-use-delta-step2 mem1 mem2)

        (setf window-tool-bar-string--cache
              result)
        (cl-incf window-tool-bar--refresh-done-count))
    (cl-incf window-tool-bar--refresh-skipped-count))

  window-tool-bar-string--cache)

(defconst window-tool-bar--separator
  (let ((str (make-string 3 ?\s)))
    (set-text-properties 0 1 '(display (space :width (4))) str)
    (set-text-properties 1 2 '(display (space :width (1)) face cursor) str)
    (set-text-properties 2 3 '(display (space :width (4))) str)
    str))

(defun window-tool-bar--keymap-entry-to-string (menu-item)
  "Convert MENU-ITEM into a (propertized) string representation.

MENU-ITEM: Menu item to convert.  See info node (elisp)Tool Bar."
  (pcase menu-item
    ;; Separators
    ((or `(,_ "--")
         `(,_ menu-item ,(and (pred stringp)
                              (pred (string-prefix-p "--")))))
     window-tool-bar--separator)

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
                                      (if enabled '(:margin 2 :ascent center)
                                        '(:margin 2 :ascent center
                                          :conversion disabled)))
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
  "Toggle display of the tool bar in the tab line of the current buffer."
  :lighter nil
  (if (and window-tool-bar-mode
           (not (eq tool-bar-map (default-value 'tool-bar-map))))
      (setq tab-line-format '(:eval (window-tool-bar-string)))
    (setq tab-line-format nil)))

;;;###autoload
(define-globalized-minor-mode global-window-tool-bar-mode
  window-tool-bar-mode window-tool-bar--turn-on
  :group 'window-tool-bar
  (add-hook 'isearch-mode-hook #'window-tool-bar--turn-on)
  (add-hook 'isearch-mode-end-hook #'window-tool-bar--turn-on))

(defun window-tool-bar--turn-on ()
  "Internal function called by `global-window-tool-bar-mode'."
  (when global-window-tool-bar-mode
    (window-tool-bar-mode 1)))

(provide 'window-tool-bar)

;;; window-tool-bar.el ends here
