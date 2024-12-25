;;; package --- Assorted buffer-related functions.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)

(defun my-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when the
minibuffer is open: we want it to close the minibuffer, even without
explicitly focusing it.  The DWIM behaviour of this command is as
follows:
- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(let ((gm global-map))
  (define-key gm (kbd "C-g") #'my-keyboard-quit-dwim))

(provide 'my-global-keymaps)
;;; my-global-keymaps.el ends here.
