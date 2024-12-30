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


;; Transparent Remote Access, Multiple Protocols -- edit remote files
(use-package tramp
  :defer 5 ;; wait 5 seconds before loading
  :config
  ;; default method for transferring files (scp, ssh)
  (customize-set-variable 'tramp-default-method "ssh"))


(provide 'my-shell)
;;; my-shell.el ends here.
