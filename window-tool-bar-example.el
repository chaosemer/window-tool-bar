;;; window-tool-bar-example.el --- Example tool bar -*- lexical-binding: t; package-lint-main-file: "window-tool-bar.el"; -*-

;; Copyright 2024 Jared Finder

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
;; This has a mode with an tool bar that uses all features of the tool
;; bar, which is very helpful for debugging and testing.  It is
;; particularly valuable for tool bar specs that are not commonly used
;; like :button.

;;; Code:

(require 'map)
(require 'tool-bar)

(define-derived-mode window-tool-bar-example--mode special-mode
  "ToolBar-Example"
  :interactive nil      ;Main entry point is `window-tool-bar-example'
  (setq-local tool-bar-map window-tool-bar-example--tool-bar-map)
  (setf buffer-undo-list t)
  (window-tool-bar-example--refresh))

(defvar window-tool-bar-example--integer 5)
(defvar window-tool-bar-example--boolean nil)
(defvar window-tool-bar-example--symbol :lock)

(defun window-tool-bar-example--refresh ()
  "Refresh the detailed description of the tool bar."
  (let ((inhibit-read-only t)
        (old-line (line-number-at-pos))
        (old-column (current-column)))
    (erase-buffer)

    ;; Print interactive state
    (insert "Variables:\n"
            (format "* window-tool-bar-example--integer: %d\n"
                    window-tool-bar-example--integer)
            (format "* window-tool-bar-example--boolean: %s\n"
                    window-tool-bar-example--boolean)
            (format "* window-tool-bar-example--symbol: %s\n"
                    window-tool-bar-example--symbol)
            "\n")

    ;; Print the tool bar
    (insert "Toolbar:\n")
    (map-keymap #'window-tool-bar-example--insert-single-entry
                tool-bar-map)
    (set-buffer-modified-p nil)

    (goto-char (point-min))
    (forward-line (1- old-line))
    (move-to-column old-column)))

(defun window-tool-bar-example--insert-single-entry (event def)
  "Insert into the current buffer details for a single toolbar entry.
EVENT and DEF are passed by `map-keymap'."
  (when (eq (car def)  'menu-item)
    (let ((plist (nthcdr 3 def)))
      (insert (format "* %S -- Properties:\n" event))
      (map-apply (lambda (key value)
                   (unless (eq key :image)
                     (insert (format "    %S\n" (list key value)))))
                 plist))))

(defvar window-tool-bar-example--tool-bar-map
  (make-sparse-keymap))

;; The following properties are supported by window-tool-bar and
;; tested below. (:image is not listed since it's always there):
;;
;; :enable, :filter, :help, :label, :visible, :wrap

;; :help, :label
(tool-bar-local-item "help" #'window-tool-bar-example--help 'help
                     window-tool-bar-example--tool-bar-map
                     :help "Display \"Help\"")
(tool-bar-local-item "attach" #'window-tool-bar-example--attach 'attach
                     window-tool-bar-example--tool-bar-map
                     :label "Paperclip icon")
(define-key-after window-tool-bar-example--tool-bar-map [separator-1]
  menu-bar-separator)

;; :enable, :visible
(tool-bar-local-item "left-arrow" #'window-tool-bar-example--left 'left
                     window-tool-bar-example--tool-bar-map
                     :enable '(> window-tool-bar-example--integer 0))
(tool-bar-local-item "right-arrow" #'window-tool-bar-example--right 'right
                     window-tool-bar-example--tool-bar-map
                     :enable '(< window-tool-bar-example--integer 10))
(tool-bar-local-item "new" #'window-tool-bar-example--reset 'reset
                     window-tool-bar-example--tool-bar-map
                     :visible '(/= window-tool-bar-example--integer 5))
(define-key-after window-tool-bar-example--tool-bar-map [separator-2]
  menu-bar-separator)

;; :button (:toggle), :button (:radio)
(tool-bar-local-item "describe" #'window-tool-bar-example--toogle-boolean 'toogle
                     window-tool-bar-example--tool-bar-map
                     :button '(:toggle . window-tool-bar-example--boolean))
(tool-bar-local-item "lock" #'window-tool-bar-example--symbol-lock 'symbol-lock
                     window-tool-bar-example--tool-bar-map
                     :button '(:radio . (eq window-tool-bar-example--symbol :lock)))
(tool-bar-local-item "lock-ok" #'window-tool-bar-example--symbol-lock-ok 'symbol-lock-ok
                     window-tool-bar-example--tool-bar-map
                     :button '(:radio . (eq window-tool-bar-example--symbol :lock-ok)))
(tool-bar-local-item "lock-broken" #'window-tool-bar-example--symbol-lock-broken 'symbol-lock-broken
                     window-tool-bar-example--tool-bar-map
                     :button '(:radio . (eq window-tool-bar-example--symbol :lock-broken)))
(define-key-after window-tool-bar-example--tool-bar-map [separator-3]
  menu-bar-separator)

;; :filter, :vert-only
(tool-bar-local-item "index" nil 'menu
                     window-tool-bar-example--tool-bar-map
                     :filter #'window-tool-bar-example--menu
                     :vert-only t)

;; :wrap
(tool-bar-local-item "preferences" #'window-tool-bar-example--prefs 'prefs
                     window-tool-bar-example--tool-bar-map
                     :wrap t)

;;;###autoload
(defun window-tool-bar-example ()
  "Show a buffer with an example tool bar using all supported features."
  (interactive)
  (switch-to-buffer "*ToolBar-Example*")
  (window-tool-bar-example--mode))

;;; Tool bar functions

(defun window-tool-bar-example--help ()
  "Binding for toolbar."
  (interactive)
  (message "Help clicked"))
(defun window-tool-bar-example--attach ()
  "Binding for toolbar."
  (interactive)
  (message "Paperclip clicked"))

(defun window-tool-bar-example--left ()
  "Binding for toolbar."
  (interactive)
  (cl-decf window-tool-bar-example--integer)
  (window-tool-bar-example--refresh))
(defun window-tool-bar-example--right ()
  "Binding for toolbar."
  (interactive)
  (cl-incf window-tool-bar-example--integer)
  (window-tool-bar-example--refresh))
(defun window-tool-bar-example--reset ()
  "Binding for toolbar."
  (interactive)
  (setf window-tool-bar-example--integer 5)
  (window-tool-bar-example--refresh))

(defun window-tool-bar-example--toogle-boolean ()
  "Binding for toolbar."
  (interactive)
  (setf window-tool-bar-example--boolean
        (null window-tool-bar-example--boolean))
  (window-tool-bar-example--refresh))
(defun window-tool-bar-example--symbol-lock ()
  "Binding for toolbar."
  (interactive)
  (setf window-tool-bar-example--symbol :lock)
  (window-tool-bar-example--refresh))
(defun window-tool-bar-example--symbol-lock-ok ()
  "Binding for toolbar."
  (interactive)
  (setf window-tool-bar-example--symbol :lock-ok)
  (window-tool-bar-example--refresh))
(defun window-tool-bar-example--symbol-lock-broken ()
  "Binding for toolbar."
  (interactive)
  (setf window-tool-bar-example--symbol :lock-broken)
  (window-tool-bar-example--refresh))

(defun window-tool-bar-example--menu (_orig-binding)
  "Binding for toolbar."
  ;; :filter is most useful for keymaps, but tool bars do not support
  ;; buttons being bound to keymaps.
 (lambda ()
   (interactive)
   (x-popup-menu t (global-key-binding (kbd "<menu-bar>")))))

(defun window-tool-bar-example--prefs ()
  "Binding for toolbar."
  (interactive)
  (error "This should not be called"))

(provide 'window-tool-bar-example)

;;; window-tool-bar-example.el ends here
