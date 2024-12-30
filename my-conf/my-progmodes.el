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


(use-package ruby-mode
  :straight (:type built-in)
  :mode (("\\.rb$"  . ruby-mode))
  :config)


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
