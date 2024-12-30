;;; package --- Search-related configuration.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)


;; Configure consult to provide `completing-read'-based search with live
;; preview.
(use-package consult
  :straight t
  :bind (("M-g g"   . consult-goto-line) ;; goto-line
	 ;; Search buffer with live preview. Replacement for `isearch'.
	 ("C-s"     . consult-line)
	 ;; "Search git": free-text search in version-controlled files.
	 ("C-c s g" . consult-git-grep)
	 ;; "Search project": free-text search in all project files.
	 ("C-c s p" . consult-ripgrep))
  :init
  ;; Use Consult to select xref locations with preview.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Delay before starting a new async search (for example for `consult-grep').
  (setq consult-async-input-debounce 0.2))


;; Incremental buffer search configured to support navigation with up/down key.
(use-package isearch
  :straight (:type built-in)
  ;; Lazily load when called for.
  :bind (("C-S-s" . isearch-forward)
	 ("C-r"   . isearch-backward))
  :config
  (let ((m isearch-mode-map))
    (define-key m (kbd "<up>") #'isearch-repeat-backward)
    (define-key m (kbd "<down>") #'isearch-repeat-forward)))


(provide 'my-search)
;;; my-search.el ends here.
