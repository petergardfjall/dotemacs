;;; package --- Functions related to treesit.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)

(require 'treesit)

(defun my-treesit-install-language (lang)
  "Install/update a treesit grammar for LANG."
  (interactive (list
                (let ((languages (mapcar 'car treesit-language-source-alist)))
                  (completing-read "Choose a language: " languages))))
  (message "languages: %s" treesit-language-source-alist)
  (treesit-install-language-grammar (intern lang))
  (message "`%s' treesit language grammar installed." lang))

(provide 'my-treesit-functions)
;;; my-treesit-functions.el ends here.
