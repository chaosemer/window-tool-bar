;;; window-tool-bar.el --- Add tool bars inside windows -*- lexical-binding: t -*-

;; Copyright 2023 Jared Finder
;; Author: Jared Finder <jared@finder.org>
;; Created: Nov 21, 2023
;; Version: 0.3
;; Keywords: mouse
;; URL: http://github.com/chaosemer/window-tool-bar
;; Package-Requires: ((emacs "27.1"))

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

;;; Known issues:
;;
;; On GNU Emacs 29.1 and earlier, terminals dragging to resize windows
;; will error with message "<tab-line> <mouse-movement> is undefined".
;; This is a bug in GNU Emacs,
;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=67457>.
;;
;; On GNU Emacs 29 and earlier, performance in terminals is lower than
;; on graphical frames.  This is due to a workaround, see "Workaround
;; for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=68334", below.
;;
;; Dragging empty space on the tab-line (which this package uses to
;; display the window tool bar) doesn't resize windows.  This is
;; unlike the mode line, where dragging empty space resizes the
;; window.

;;; Todo:
;;
;; Not all features planned are implemented yet.  Eventually I would
;; like to also generally make tool bars better.
;;
;; Post 1.0 work:
;;
;; * Clean up Emacs tool bars
;;     * Default: Remove default tool-bar entirely
;;     * grep, vc: Remove default tool-bar inherited
;;     * info: Remove Next / Prev / Up, which is already in the header
;;     * smerge: Add tool bar for next/prev

;;; Code:

(require 'mwheel)
(require 'tab-line)
(require 'tool-bar)

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
      (window-tool-bar--insert-memory-use
       "Step 1" (window-tool-bar--memory-use-avg-step1))
      (window-tool-bar--insert-memory-use
       "Step 2" (window-tool-bar--memory-use-avg-step2))
      (insert (format "Refresh count  %d\n" window-tool-bar--refresh-done-count)
              (format "Refresh executed percent %.2f\n"
                      (/ window-tool-bar--refresh-done-count
                         (+ window-tool-bar--refresh-done-count
                            window-tool-bar--refresh-skipped-count)
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
  :group 'convenience
  :prefix "window-tool-bar-")

(defvar window-tool-bar--button-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<follow-link>") 'mouse-face)
    ;; Follow link on all clicks of mouse-1 and mouse-2 since the tool
    ;; bar is not a place the point can travel to.
    (define-key map (kbd "<tab-line> <mouse-1>") #'window-tool-bar--call-button)
    (define-key map (kbd "<tab-line> <double-mouse-1>") #'window-tool-bar--call-button)
    (define-key map (kbd "<tab-line> <triple-mouse-1>") #'window-tool-bar--call-button)
    (define-key map (kbd "<tab-line> <mouse-2>") #'window-tool-bar--call-button)
    (define-key map (kbd "<tab-line> <double-mouse-2>") #'window-tool-bar--call-button)
    (define-key map (kbd "<tab-line> <triple-mouse-2>") #'window-tool-bar--call-button)

    ;; Mouse down events do nothing.  A binding is needed so isearch
    ;; does not exit when the tab bar is clicked.
    (define-key map (kbd "<tab-line> <down-mouse-1>") #'window-tool-bar--ignore)
    (define-key map (kbd "<tab-line> <double-down-mouse-1>") #'window-tool-bar--ignore)
    (define-key map (kbd "<tab-line> <triple-down-mouse-1>") #'window-tool-bar--ignore)
    (define-key map (kbd "<tab-line> <down-mouse-2>") #'window-tool-bar--ignore)
    (define-key map (kbd "<tab-line> <double-down-mouse-2>") #'window-tool-bar--ignore)
    (define-key map (kbd "<tab-line> <triple-down-mouse-2>") #'window-tool-bar--ignore)
    map)
  "Keymap used by `window-tool-bar--keymap-entry-to-string'.")
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

This is for when you want more customizations than the command
`window-tool-bar-mode' provides.  Commonly added to the variable
`tab-line-format', `header-line-format', or `mode-line-format'"
  (if (or (null window-tool-bar-string--cache)
          (window-tool-bar--last-command-triggers-refresh-p))
      (let* ((mem0 (memory-use-counts))
             (toolbar-menu (window-tool-bar--get-keymap))
             (mem1 (memory-use-counts))
             (strs (mapcar #'window-tool-bar--keymap-entry-to-string
                           (cdr toolbar-menu))) ;Skip 'keymap
             (result (mapconcat #'identity
                                (delete nil strs)
                                ;; Without spaces between the text, hovering
                                ;; highlights all adjacent buttons.
                                (if (eq 'text (window-tool-bar--style)) " "
                                  (propertize " " 'invisible t))))
             (mem2 (memory-use-counts)))
        (cl-mapl (lambda (l-init l0 l1)
                   (cl-incf (car l-init) (- (car l1) (car l0))))
                 window-tool-bar--memory-use-delta-step1 mem0 mem1)
        (cl-mapl (lambda (l-init l1 l2)
                   (cl-incf (car l-init) (- (car l2) (car l1))))
                 window-tool-bar--memory-use-delta-step2 mem1 mem2)

        (setf window-tool-bar-string--cache
              (concat
               ;; The tool bar face by default puts boxes around the
               ;; buttons.  However, this box is not displayed if the
               ;; box starts at the leftmost pixel of the tab-line.
               ;; Add a single space in this case so the box displays
               ;; correctly.
               (when (display-supports-face-attributes-p
                      '(:box (line-width 1)))
                 (propertize " " 'display '(space :width (1))))
               result))
        (cl-incf window-tool-bar--refresh-done-count))
    (cl-incf window-tool-bar--refresh-skipped-count))

  window-tool-bar-string--cache)

(defconst window-tool-bar--graphical-separator
  (let ((str (make-string 3 ?\s)))
    (set-text-properties 0 1 '(display (space :width (4))) str)
    (set-text-properties 1 2
                         '(display (space :width (1))
                           face (:inverse-video t))
                         str)
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
     (if (eq 'text (window-tool-bar--style)) "|"
       window-tool-bar--graphical-separator))

    ;; Menu item, turn into propertized string button
    (`(,key menu-item ,name-expr ,binding . ,plist)
     (let* ((visible-entry (plist-member plist :visible))
            (visible (or (null visible-entry) ;Default is visible
                         (eval (cadr visible-entry))))
            (wrap (plist-get plist :wrap))
            (filter (plist-get plist :filter)))
       (when filter
         (setf binding
               ;; You would expect this to use `funcall', but existing
               ;; code in `parse_tool_bar_item' uses `eval'.
               (eval `(,filter ',binding))))
       (when (and binding
                  visible
                  (null wrap))
         (let* ((name (eval name-expr))
                (str (upcase-initials (or (plist-get plist :label)
                                          (string-trim-right name "\\.+"))))
                (len (length str))
                (enable-form (plist-get plist :enable))
                (enabled (or (not enable-form)
                             (eval enable-form)))
                (button-spec (plist-get plist :button))
                (button-selected (eval (cdr-safe button-spec)))
                (vert-only (plist-get plist :vert-only))
                image-start
                image-end)
           ;; Depending on style, Images can be displayed to the
           ;; left, to the right, or in place of the text
           (pcase (window-tool-bar--style)
             ('image
              (setf image-start 0
                    image-end len))
             ('text
              ;; Images shouldn't be available
              )
             ((or 'both 'both-horiz)
              (if vert-only
                  (setf image-start 0 image-end len)
                (setf str (concat " " str)
                      image-start 0
                      image-end 1
                      len (1+ len))))
             ('text-image-horiz
              (if vert-only
                  (setf image-start 0 image-end len)
                (setf str (concat str " ")
                      image-start len
                      image-end (1+ len)
                      len (1+ len))))
             (_
              (error "Unexpected value from window-tool-bar--style")))

           (cond
            ((and enabled button-selected)
             (add-text-properties 0 len
                                  '(mouse-face
                                    window-tool-bar-button-checked-hover
                                    keymap window-tool-bar--button-keymap
                                    face window-tool-bar-button-checked)
                                  str))
            (enabled
             (add-text-properties 0 len
                                  '(mouse-face window-tool-bar-button-hover
                                    keymap window-tool-bar--button-keymap
                                    face window-tool-bar-button)
                                  str))
            (t
             (put-text-property 0 len
                                'face
                                'window-tool-bar-button-disabled
                                str)))
           (when-let ((spec (and image-start image-end
                                 (plist-get menu-item :image))))
             (put-text-property image-start image-end
                                'display
                                (append spec
                                        (if enabled '(:margin 2 :ascent center)
                                          '(:margin 2 :ascent center
                                                    :conversion disabled)))
                                str))
           (let ((help-text (or (plist-get plist :help) name))
                 (keys (where-is-internal binding nil t)))
             (put-text-property 0 len
                                'help-echo
                                (if keys
                                    (concat help-text
                                            "  ("
                                            (key-description keys)
                                            ")")
                                  help-text)
                                str))
           (put-text-property 0 len 'tool-bar-key key str)
           str))))))

(defun window-tool-bar--call-button ()
  "Call the button that was clicked on in the tab line."
  (interactive)
  (when (mouse-event-p last-command-event)
    (let ((posn (event-start last-command-event)))
      ;; Commands need to execute with the right buffer and window
      ;; selected.  The selection needs to be permanent for isearch.
      (select-window (posn-window posn))
      (let* ((str (posn-string posn))
             (key (get-text-property (cdr str) 'tool-bar-key (car str)))
             (cmd (lookup-key (window-tool-bar--get-keymap) (vector key))))
        (call-interactively cmd)))))

(defun window-tool-bar--ignore ()
  "Do nothing.  This command exists for isearch."
  (interactive)
  nil)

(defvar window-tool-bar--ignored-event-types
  (list 'mouse-movement
        mouse-wheel-down-event mouse-wheel-up-event
        mouse-wheel-left-event mouse-wheel-right-event
        (bound-and-true-p mouse-wheel-down-alternate-event)
        (bound-and-true-p mouse-wheel-up-alternate-event)
        (bound-and-true-p mouse-wheel-left-alternate-event)
        (bound-and-true-p mouse-wheel-right-alternate-event)
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
  (window-tool-bar--force-update))

;;;###autoload
(define-globalized-minor-mode global-window-tool-bar-mode
  window-tool-bar-mode window-tool-bar--turn-on
  :group 'window-tool-bar
  :package-version '(window-tool-bar . "0.1")
  (add-hook 'isearch-mode-hook #'window-tool-bar--turn-on)
  (add-hook 'isearch-mode-end-hook #'window-tool-bar--turn-on))

(defun window-tool-bar--turn-on ()
  "Internal function called by the command `global-window-tool-bar-mode'."
  (when global-window-tool-bar-mode
    (window-tool-bar-mode 1)))

(defun window-tool-bar--force-update ()
  "Forcibly refresh the tool bar state."
  (if (and window-tool-bar-mode
           (or window-tool-bar-show-default
               (not (eq tool-bar-map (default-value 'tool-bar-map)))))
      (setq tab-line-format '(:eval (window-tool-bar-string)))
    (setq tab-line-format nil)))

(defun window-tool-bar--style ()
  "Return the effective style based on `window-tool-bar-style'.

This also takes into account frame capabilities.  If the current
frame can not display images (see `dislay-images-p'), then this
will always return text."
  (if (not (display-images-p))
      'text
    (let ((style window-tool-bar-style))
      (when (eq style 'tool-bar-style)
        (setf style tool-bar-style))
      (unless (memq style '(image text both both-horiz text-image-horiz))
        (setf style (if (fboundp 'tool-bar-get-system-style)
                        (tool-bar-get-system-style)
                      'image)))
      style)))

;;; Display styling:
(defcustom window-tool-bar-style 'image
  "Tool bar style to use for window tool bars.
The meanining is the same as for `tool-bar-style', which see.  If
set to the symbol `tool-bar-style', then use the value of
`tool-bar-style' instead.

When images can not be displayed (see `display-images-p'), text
is used."
  :type '(choice (const :tag "Images" :value image)
                 (const :tag "Text" :value text)
                 ;; This option would require multiple tool bar lines.
                 ;;(const :tag "Both" :value both)
                 (const :tag "Both-horiz" :value both-horiz)
                 (const :tag "Text-image-horiz" :value text-image-horiz)
                 (const :tag "Inherit tool-bar-style" :value tool-bar-style)
                 (const :tag "System default" :value nil))
  :group 'window-tool-bar
  :package-version '(window-tool-bar . "0.3"))

(defface window-tool-bar-button
  '((default
     :inherit tab-line)
    (((class color) (min-colors 88) (supports :box t))
     :box (:line-width -1 :style released-button)
     :background "grey85")
    ;; If the box is not supported, dim the button background a bit.
    (((class color) (min-colors 88))
     :background "grey70")
    (t
     :inverse-video t))
  "Face used for buttons when the mouse is not hovering over the button."
  :group 'window-tool-bar
  :package-version '(window-tool-bar . "0.2"))

(defface window-tool-bar-button-hover
  '((default
     :inherit tab-line)
    (((class color) (min-colors 88))
     :box (:line-width -1 :style released-button)
     :background "grey95")
    (t
     :inverse-video t))
  "Face used for buttons when the mouse is hovering over the button."
  :group 'window-tool-bar
  :package-version '(window-tool-bar . "0.2"))

(defface window-tool-bar-button-disabled
  '((default
     :inherit tab-line)
    (((class color) (min-colors 88))
     :box (:line-width -1 :style released-button)
     :background "grey50"
     :foreground "grey70")
    (t
     :inverse-video t
     :background "brightblack"))
  "Face used for buttons when the button is disabled."
  :group 'window-tool-bar
  :package-version '(window-tool-bar . "0.2"))

(defface window-tool-bar-button-checked
  '((default
     :inherit tab-line)
    (((supports :box t))
     :box (:line-width -1 :style pressed-button)
     :background "grey85")
    (((class color))
     :background "blue"
     :foreground "white")
    (t
     :inverse-video t))
  "Face used for buttons when they are toggled."
  :group 'window-tool-bar
  :package-version '(window-tool-bar . "0.3"))

(defface window-tool-bar-button-checked-hover
  '((default
     :inherit tab-line)
    (((class color) (min-colors 88) (supports :box t))
     :box (:line-width -1 :style pressed-button)
     :background "grey95")
    (((class color))
     :background "brightblue"
     :foreground "white")
    (t
     :inverse-video t))
  "Face used for buttons when the mouse is hovering over the button."
  :group 'window-tool-bar
  :package-version '(window-tool-bar . "0.3"))

(defun window-tool-bar--set-and-refresh (symbol value)
  "Internal function to be called by changing customize options.

SYMBOL and VALUE have the same meaning as for
`set-default-toplevel-value'."
  (set-default-toplevel-value symbol value)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when window-tool-bar-mode
        (window-tool-bar--force-update)))))

(defcustom window-tool-bar-show-default nil
  "If the default tool bar is shown or not.

If non-nil, the default tool bar is shown.
If nil, the default tool bar is not shown."
  :type 'boolean
  :group 'window-tool-bar
  :package-version '(window-tool-bar . "0.3")
  :set #'window-tool-bar--set-and-refresh)

;;; Workaround for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=68334.
(defun window-tool-bar--get-keymap ()
  "Return the tool bar keymap."
  (if (and (version< emacs-version "30")
           (eq 'text (window-tool-bar--style)))
      ;; This code path is a less efficient workaround.
      (window-tool-bar--make-keymap-1)
    (global-key-binding (kbd "<tool-bar>"))))

(declare-function image-mask-p "image.c" (spec &optional frame))

(defun window-tool-bar--make-keymap-1 ()
  "Patched copy of `tool-bar-make-keymap-1'."
  (mapcar (lambda (bind)
            (let (image-exp plist)
              (when (and (eq (car-safe (cdr-safe bind)) 'menu-item)
			 ;; For the format of menu-items, see node
			 ;; `Extended Menu Items' in the Elisp manual.
			 (setq plist (nthcdr (if (consp (nth 4 bind)) 5 4)
					     bind))
			 (setq image-exp (plist-get plist :image))
			 (consp image-exp)
			 (not (eq (car image-exp) 'image))
			 (fboundp (car image-exp)))
		(let ((image (and (display-images-p)
                                  (eval image-exp))))
		  (unless (and image (image-mask-p image))
		    (setq image (append image '(:mask heuristic))))
		  (setq bind (copy-sequence bind)
			plist (nthcdr (if (consp (nth 4 bind)) 5 4)
				      bind))
		  (plist-put plist :image image)))
	      bind))
          tool-bar-map))

(provide 'window-tool-bar)

;;; window-tool-bar.el ends here
