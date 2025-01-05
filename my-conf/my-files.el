;;; package --- Directory and file configuration.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)


;; Transparently open compressed files such as tar.gz.
(use-package auto-compression-mode
  :straight (:type built-in)
  :defer 5
  :config
  (auto-compression-mode t))


(provide 'my-files)
;;; my-files.el ends here.
