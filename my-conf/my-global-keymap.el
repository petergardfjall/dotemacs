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


;; Define keybindings that go into the the `global-map'.
(let ((gm global-map))
  (define-key gm (kbd "C-g") #'my-keyboard-quit-dwim)
  ;; Unbind unneeded/disturbing keybindings.
  (define-key gm (kbd "C-t") nil) ;; Transpose characters.
  (define-key gm (kbd "M-t") nil) ;; Transpose words.
  (define-key gm (kbd "M-h") nil) ;; Mark-paragraph.
  ;; Comment line(s).
  (define-key gm (kbd "C-c c") #'comment-line)
  (define-key gm (kbd "C-c w") #'delete-trailing-whitespace)
  ;; Find definition of thing at point (if supported by mode).
  (define-key gm (kbd "<M-down>") #'xref-find-definitions)
  (define-key gm (kbd "<M-up>")   #'xref-pop-marker-stack)
  (define-key gm (kbd "C-c f d")  #'xref-find-definitions)
  (define-key gm (kbd "C-c f r")  #'xref-find-references)
  ;; See if documentation can be found for thing at point.
  (define-key gm (kbd "C-c C-d")  #'eldoc)
  ;; Move between windows with C-x w <up|down|left|right>.
  (define-key gm (kbd "C-x w <up>")    #'windmove-up)
  (define-key gm (kbd "C-x w <down>")  #'windmove-down)
  (define-key gm (kbd "C-x w <right>") #'windmove-right)
  (define-key gm (kbd "C-x w <left>")  #'windmove-left))

(provide 'my-global-keymap)
;;; my-global-keymap.el ends here.
