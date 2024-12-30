;;; package --- Configuration of documentation packages.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)


;; Major mode for AsciiDoc (.adoc) file editing.
(use-package adoc-mode
  :straight (adoc-mode :type git :host github
                       :repo "bbatsov/adoc-mode" :branch "master")
  :commands (adoc-mode)
  :mode (("\\.adoc$" . adoc-mode))
  :config
  (add-hook 'adoc-mode-hook #'my-highlight-todos)
  ;; Don't want orgtable "help" with formatting tables.
  (add-hook 'adoc-mode-hook (lambda () (orgtbl-mode -1))))


;; Built-in on-the-fly spell checking for text buffers.
(use-package flyspell
  :straight (:type built-in)
  :diminish
  :hook ((text-mode . flyspell-mode)))


;; Major mode for markdown (.md) file editing.
(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md$" . gfm-mode)
         ("\\.md$" . gfm-mode)
         ("\\.markdown$" . gfm-mode)
         ;; Cheat sheets under ~/dotfiles/cheat/sheets.
         ("\\.cheat$" . markdown-mode))
  :hook ((markdown-mode . prettier-mode))
  :init (setq markdown-command "pandoc")
  :config
  ;; no tabs for indentation
  (setq indent-tabs-mode nil)
  (setq markdown-list-indent-width 2)
  ;; Define key-bindings.
  (let ((m markdown-mode-map))
    ;; Subtree, list, and table editing
    (define-key m (kbd "M-<up>")    #'markdown-move-up)
    (define-key m (kbd "M-<down>")  #'markdown-move-down)
    (define-key m (kbd "M-<left>")  #'markdown-promote)
    (define-key m (kbd "M-<right>") #'markdown-demote)))


(use-package markdown-preview-mode
  :straight t
  ;; Lazily load when called for.
  :bind ("C-c p m" . markdown-preview-mode)
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-preview-stylesheets
        (list
         ;; style very similar to github's for markdown rendering
         "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.8.1/github-markdown.min.css"
         ;; style that adds some margins
         "https://petergardfjall.github.io/css/emacs-markdown/github-markdown-body.css"
         ;; style for syntax highlighting of fenced codeblocks
         "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css"))
  (setq markdown-preview-javascript
        (list
         ;; javascript lib for syntax highlighting of fenced code blocks
         "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/highlight.min.js"
         ;; javascript that applies the highlight js lib to the doc
         "https://petergardfjall.github.io/js/emacs-markdown/github-markdown-block-highlight.js")))


(provide 'my-documentation)
;;; my-documentation.el ends here.
