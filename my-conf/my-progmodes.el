;;; package --- Programming-related configuration.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)


;; Dockerfile editing
(use-package dockerfile-ts-mode
  :straight (:type built-in)
  :config
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'dockerfile-ts-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'dockerfile-ts-mode-hook #'my-strip-on-save-hook))


;; Emacs frontend for GNU Global (gtags) to generate and search code tags.
;; Wraps the gtags and global command-line tools.
(use-package ggtags
  :straight t
  :diminish
  ;; add ggtags as a xref backend in emacs-lisp-mode (xref-find-definitions)
  :hook ((emacs-lisp-mode . ggtags-mode))
  ;; Lazily load when called for.
  :bind (("C-c t c"   . my-ggtags-create)
	 ("C-c t f d" . my-ggtags-find-definition)
	 ("C-c t f r" . my-ggtags-find-reference))
  :config
  ;; interferes with beginning/end of buffer key bindings
  (define-key ggtags-navigation-map (kbd "M->") nil)
  (define-key ggtags-navigation-map (kbd "M-<") nil))


(use-package groovy-mode
  :straight t
  :disabled t
  :mode (("\\.groovy$" . groovy-mode)
         ("\\.gvy$" . groovy-mode)
	 ("\\.gy$" . groovy-mode)
	 ("^Jenkinsfile$" . groovy-mode)))


(use-package highlight-indent-guides
  :straight t
  :diminish
  :hook ((emacs-lisp-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character))


(use-package graphql-mode
  :straight t
  :mode (("\\.gql$" . graphql-mode)
         ("\\.graphql$" . graphql-mode))
  :hook ((graphql-mode . prettier-mode)))


(use-package python-mode
  :straight (:type built-in)
  :mode (("\\.py$" . python-mode))
  :init
  ;; Add settings for buffers in this mode.
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local
               indent-tabs-mode nil ;; No tabs for indentation.
               ;; PEP-8 calls for max 79 characters per line.
               fill-column 79
               python-indent-offset 4
               python-indent-guess-indent-offset nil)))
  (add-hook 'python-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'python-mode-hook #'my-strip-on-save-hook)
  (add-hook 'python-mode-hook #'eglot-ensure))


(use-package ruby-mode
  :straight (:type built-in)
  :mode (("\\.rb$"  . ruby-mode))
  :config)


;; Use `sphinx-doc' when `python-mode' is activated. Gives a templated docstring
;; when pressing "C-c M-d" in function head.
(use-package sphinx-doc
  :straight t
  :commands python-mode
  :config
  (sphinx-doc-mode))


;; A language template system for emacs. lsp-mode auto-configures yasnippet for
;; use with a given language server.  Write a snippet key and press the key
;; associated with yas-expand (TAB by default) to have the snippet expanded. To
;; see available snippets: M-x yas-describe-tables.
;; Custom (per-mode) snippets can be placed under ~/.emacs.d/snippets/.
(use-package yasnippet
  :straight t
  :defer 2
  :diminish yas-minor-mode ; don't display on modeline
  :config
  ;; use yasnippet as a global minor mode
  ;; note: it can also be activated per language/major-mode
  ;;    see https://github.com/joaotavora/yasnippet
  (yas-global-mode 1))

;; A collection of snippets for many languages.
(use-package yasnippet-snippets
  :straight t
  :after yasnippet)


(provide 'my-progmodes)
;;; my-progmodes.el ends here.
