;;; -*- lexical-binding: t -*-
;;;
;;; TODO:
;;;; For 1.0
;;; * Fix package style.
;;; * Play with this on for a bit.
;;; * Upload to MELPA.

;;;; For 2.0
;;; * Properly support button labels
;;; * Make this work on non-graphical frames.
;;; * Add utility function to clean up Emacs toolbars (Info, grep)
;;;
;;; * Add a bit more documentation.
;;; * Add customization option: ignore-default-tool-bar-map
;;; * Make tab-line dragging resize the window
(require 'tab-line)

;;;###autoload
(defun window-tool-bar-string ()
  (let ((toolbar-menu (cdr (keymap-global-lookup "<tool-bar>"))))
    (mapconcat #'window-tool-bar--keymap-entry-to-string toolbar-menu " ")))

(defun window-tool-bar--keymap-entry-to-string (menu-item)
  "Convert MENU-ITEM into a (propertized) string representation."
  (pcase menu-item
    (`(,key menu-item ,name . ,props)
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
  (window-tool-bar-mode 1))

;;;###autoload
(define-globalized-minor-mode global-window-tool-bar-mode
  window-tool-bar-mode window-tool-bar--turn-on
  (add-hook 'isearch-mode-hook #'window-tool-bar--turn-on)
  (add-hook 'isearch-mode-end-hook #'window-tool-bar--turn-on))

(provide 'window-tool-bar)
