;;; -*- lexical-binding: t -*-
;;;
;;; TODO:
;;; * Add a bit more documentation.
;;; * Upload to github, internal git repo, and MELPA.
;;; * Play with this on for a bit.
;;; * Properly support button labels
;;; * Make this work on non-graphical frames.
;;; * Add customization option: ignore-default-tool-bar-map
;;; * Make disabled properties display better (no mouse over)
;;; * Add utility function to clean up Emacs toolbars (Info, grep)
;;; * Make tab-line dragging resize the window
(require 'tab-line)

(defun tab-line-from-tool-bar ()
  (let ((toolbar-menu (cdr (keymap-global-lookup "<tool-bar>"))))
    (mapconcat #'tab-line-convert-keymap-entry toolbar-menu " ")))

(defun tab-line-convert-keymap-entry (menu-item)
  "Convert MENU-ITEM into a (propertized) string representation."
  (pcase menu-item
    (`(,key menu-item ,name . ,props)
     ;; Normal menu item, turn into tab-line-button
     (let ((str (format "[%s]" name)))
       (setq str (propertize str
                             'mouse-face 'tab-line-highlight
                             'keymap (define-keymap
                                       "<follow-link>" 'mouse-face
                                       "<tab-line> <mouse-2>" #'tlftb--call-button)))
       (when-let ((spec (plist-get menu-item :image)))
         (setq str (propertize str
                               'display spec)))
       (when-let ((spec (plist-get menu-item :help)))
         (setq str (propertize str
                               'help-echo spec)))
       (setq str (propertize str
                             'tool-bar-key key))
       str))
    (`(,_ "--")
     "|")))

(defun tlftb--call-button ()
  "Call the button that was clicked on in the tab line."
  (interactive)
  (when (mouse-event-p last-command-event)
    (let ((posn (event-start last-command-event)))
      (with-current-buffer (window-buffer (posn-window posn))
        (let* ((str (posn-string posn))
               (key (get-text-property (cdr str) 'tool-bar-key (car str)))
               (cmd (lookup-key global-map (vector 'tool-bar key))))
          (call-interactively cmd))))))

(define-minor-mode tab-line-from-tool-bar-mode
  "Toggle display of the toolbar in the tab line of the current buffer."
  :lighter nil
  (if (and tab-line-from-tool-bar-mode
           (not (eq tool-bar-map (default-value 'tool-bar-map))))
      (setq tab-line-format '(:eval (tab-line-from-tool-bar)))
    (setq tab-line-format nil)))

(defun tab-line-from-tool-bar--turn-on ()
  (tab-line-from-tool-bar-mode 1))

(define-globalized-minor-mode global-tab-line-from-tool-bar-mode
  tab-line-from-tool-bar-mode tab-line-from-tool-bar--turn-on
  (add-hook 'isearch-mode-hook #'tab-line-from-tool-bar--turn-on)
  (add-hook 'isearch-mode-end-hook #'tab-line-from-tool-bar--turn-on))
