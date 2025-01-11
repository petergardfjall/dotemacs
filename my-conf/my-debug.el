;;; package --- Configuration of debugging tools.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)


;; Debug Adapter Protocol (DAP) client for Emacs.
(use-package dape
  :straight t
  :commands (dape dape-breakpoint-toggle)
  :hook ((kill-emacs . dape-breakpoint-save)  ;; Save breakpoints on quit.
         (after-init . dape-breakpoint-load)) ;; Load breakpoints on startup.
  :config
  (setq dape-buffer-window-arrangement 'right)
  ;; Pulse source line (performance hit)
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)
  ;; Customize selection of window for displaying the source code when clicking
  ;; a stack trace entry. The default seems to select the *projtree* window
  ;; despite it being a dedicated window.
  ;; Note: this is used as a `display-buffer' ACTION argument.
  (setq dape-display-source-buffer-action
        '((display-buffer-use-some-window display-buffer-pop-up-window)))
  ;; Save unsaved buffers when starting a `dape' session.
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t))))


(provide 'my-debug)
;;; my-debug.el ends here.
