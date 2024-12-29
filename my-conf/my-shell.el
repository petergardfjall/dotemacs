;;; package --- Shell and terminal configuration.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)

(use-package ansi-term
  :straight (:type built-in)
  :commands (ansi-term)
  :config)

(use-package shell
  :straight (:type built-in)
  :commands shell
  :config)

(use-package term
  :straight (:type built-in)
  :commands (term)
  :config)

(provide 'my-shell)
;;; my-shell.el ends here.
