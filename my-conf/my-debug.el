;;; package --- Configuration of debugging tools.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)


;; Debug programs using `delve'. This debugger relies on Emacs' GUD framework.
(use-package go-dlv
  :straight t
  :commands (dlv dlv-current-function)
  :config)


(provide 'my-debug)
;;; my-debug.el ends here.
