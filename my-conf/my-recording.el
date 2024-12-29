;;; package --- Configuration of recording tools.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)

;; a package for making screencasts within Emacs.
(use-package gif-screencast
  :straight t
  ;; Lazily load when called for.
  :bind (("C-c C-s s" . gif-screencast-start-or-stop)
	 ("C-c C-s p" . gif-screencast-toggle-pause))
  :config
  (setq gif-screencast-program "scrot")
  (setq gif-screencast-output-directory (expand-file-name "~/.emacs.d/screencasts")))


;; enable through `keycast-mode` or `keycast-log-mode`
(use-package keycast
  :straight t
  :defer t)


(provide 'my-recording)
;;; my-recording.el ends here.
