;;; emacs-init.el --- Main entry-point for emacs configuration. -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Uses `use-package' for package configuration and `straight' for a
;;; controlling versions of installed third-party packages.

;;; Code:

(require 'early-init)
(require 'use-package)

(message "Loaded early-init.el after %.3fs." my-early-init-duration)

;; Time for starting to load this file.
(defconst emacs-start-time (current-time))

;;
;; Declarations
;;

(defvar my-frame-width 120
  "Initial width of Emacs frame.")
(defvar my-frame-height 40
  "Initial height of Emacs frame.")
(defvar my-font "Roboto Mono"
  "Text font to use.
For example, `Source Code Pro`, `Ubuntu Mono`,`Cousine`, `JetBrains Mono`).")
(defvar my-font-size 10.5 "Font size to use in points (for example, 10.5).")

;;
;; Bootstrap package management.
;;
(defun my-bootstrap-straight-el ()
  "Ensures straight.el is installed and loaded."
  (message "bootstrapping straight.el ...")
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (message "loading straight.el bootstrapper ...")
    (load bootstrap-file nil 'nomessage)))


;;
;; Lazily loaded utility functions.
;;
(add-to-list 'load-path "~/.emacs.d/my-lisp")
(autoload 'my-byte-offset "my-buffer-functions.el"
  "Report the byte offset (0-indexed) at point (cursor position)." t)
(autoload 'my-rename-file-and-buffer "my-buffer-functions.el"
  "Rename the current buffer and the file it is visiting." t)
(autoload 'my-strip-on-save-hook "my-buffer-functions.el"
  "Register a buffer-local `before-save-hook' to run `delete-trailing-whitespace'." nil)
(autoload 'my-untabify-buffer "my-buffer-functions.el"
  "Run `untabify' on the whole buffer." t)
(autoload 'my-untabify-on-save-hook "my-buffer-functions.el"
  "Register a buffer-local `before-save-hook' to `untabify' entire buffer." nil)

(autoload 'my-ggtags-create "my-ggtags-functions.el"
  "Create `GTAGS' in the root directory of the current buffer's project." t)
(autoload 'my-ggtags-find-definition "my-ggtags-functions.el"
  "Replacement for `ggtags-find-definition' that will always prompt." t)
(autoload 'my-ggtags-find-reference "my-ggtags-functions.el"
  "Replacement for `ggtags-find-reference' that will always prompt." t)

(autoload 'my-color-lighten "my-display-functions.el"
  "Determine a brighter/darker shade of a hex color." t)
(autoload 'my-resolution-resize "my-display-functions.el"
  "Resize frame to its default size and scale the font after screen resolution." t)
(autoload 'my-scale-font "my-display-functions.el"
  "Scale the frame font according to screen RESOLUTION." t)
(autoload 'my-set-default-font-height "my-display-functions.el"
  "Reset the font height for the selected frame to the default font size." t)

(add-to-list 'load-path "~/.emacs.d/my-conf")
(require 'my-global-keymap)


;;
;; Start of actual initialization.
;;

(message "Loading %s ..." load-file-name)

;; Ensure that straight.el is installed and loaded.  Instead of the package.el
;; package manager that comes bundled with Emacs, we use straight.el to manage
;; packages with better control over package versions.
;;
;; To get a consistent and repeatable configuration across multiple machines,
;; one can create a "lockfile" (pinning each package to a specific commit) with
;; the `M-x straight-freeze-versions` command. It generates
;; `straight/versions/default.el` which can then be version controlled.
;; For a more stable alternative go with branch "master".
(setq straight-repository-branch "develop")
(my-bootstrap-straight-el)

;; The use-package macro is included in Emacs as of version 29. No need to
;; install.
(setq use-package-verbose nil) ;; set to t to see when packages are loaded

;;
;; Start of custom package installation/configuration.
;;
(require 'my-appearance)
(require 'my-completion)
(require 'my-projects)
(require 'my-recording)
(require 'my-search)
(require 'my-shell)


(use-package vundo
  :straight t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  ;; Display visual undo tree.
  (global-set-key (kbd "C-c u t") #'vundo)
  (let ((m vundo-mode-map))
    (define-key m (kbd "d") #'vundo-diff)
    (define-key m (kbd "C-x u") #'undo)))


(use-package postrace
  ;; :load-path "~/dev/git/emacs-postrace"
  :straight (postrace :type git :host github
                      :repo "petergardfjall/emacs-postrace"
                      :branch "main")
  ;; Lazily load when called for.
  :bind (("C-c p p" . postrace-push)
	 ("C-c p b" . postrace-browse)))


;; allows definition of hydras - families of commands with a common prefix
(use-package hydra
  :straight t
  :config
  ;; Window navigation/resizing hydra.
  (defhydra hydra-windows (:hint nil)
    "
windmove: ← → ↑ ↓      resize: shift + {↤ ⭲ ⭱ ↧}"
    ("<left>"    windmove-left)
    ("<right>"   windmove-right)
    ("<up>"      windmove-up)
    ("<down>"    windmove-down)
    ("S-<left>"  shrink-window-horizontally)
    ("S-<right>" enlarge-window-horizontally)
    ("S-<up>"    enlarge-window)
    ("S-<down>"  shrink-window)
    ("q"         nil))
  (define-key global-map (kbd "C-c C-w") 'hydra-windows/body))


;; Transparent Remote Access, Multiple Protocols -- edit remote files
(use-package tramp
  :defer 5 ;; wait 5 seconds before loading
  :config
  ;; default method for transferring files (scp, ssh)
  (customize-set-variable 'tramp-default-method "ssh"))


;; built-in on-the-fly syntax checking, which highlights erroneous lines.
(use-package flymake
  :straight (:type built-in)
  :diminish
  :hook ((prog-mode text-mode) . flymake-mode)
  :config
  (let ((m flymake-mode-map))
    ;; "show errors in project"
    (define-key m (kbd "C-c s e p") #'flymake-show-project-diagnostics)
    ;; "show show errors in file"
    (define-key m (kbd "C-c s e f") #'flymake-show-buffer-diagnostics)))


;; built-in on-the-fly spell checking for text buffers.
(use-package flyspell
  :straight (:type built-in)
  :diminish
  :hook ((text-mode . flyspell-mode)))

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


;; A Git porcelain inside Emacs.
(use-package magit
  :straight t
  ;; Lazily load when called for.
  :bind (("C-x g" . magit-status)))


;; Highlight diffs (in the fringe) for version-controlled buffers.
(use-package diff-hl
  :straight t
  :hook ((prog-mode text-mode) . diff-hl-mode)
  :diminish
  :config
  ;; refresh if magit does an update
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


;; transparently open compressed files
(use-package auto-compression-mode
  :straight (:type built-in)
  :defer 5
  :config
  (auto-compression-mode t))

;;;
;;; Development/coding
;;;


;; Emacs frontend for GNU global/gtags to generate and search code tags.
;; Wraps the 'gtags' and 'global' command-line tools.
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


;; when saving a buffer in sh-mode: untabify and delete trailing whitespace
(use-package sh-script
  :mode (("\\.sh$" . sh-mode)
         ("\\.env$" . sh-mode))
  :config
  ;; use bash-language-server (installed separately via npm)
  (add-hook 'sh-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'sh-mode-hook 'my-strip-on-save-hook))


;; Integrates the tree-sitter incremental language parsing library. It supports
;; syntax highlighting and comes with replacement major-modes for many languages
;; `<language>-ts-mode'. In the future a lot of interesting functionality might
;; come from it.
(use-package treesit
  :straight (:type built-in)
  :defer t
  :init
  ;; Use treesit-based major-modes where grammars are available.
  (add-to-list 'major-mode-remap-alist '(bash-mode   . bash-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-mode      . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode    . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(css-mode    . css-ts-mode))
  (add-to-list 'major-mode-remap-alist '(go-mode     . go-ts-mode))
  (add-to-list 'major-mode-remap-alist '(html-mode   . html-ts-mode))
  (add-to-list 'major-mode-remap-alist '(json-mode   . json-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-mode     . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (add-to-list 'major-mode-remap-alist '(ruby-mode   . ruby-ts-mode))
  (add-to-list 'major-mode-remap-alist '(sql-mode    . sql-ts-mode))
  (add-to-list 'major-mode-remap-alist '(toml-mode   . toml-ts-mode))
  ;; Specify which tree-sitter language grammar defintions to use.
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
  :config
  ;; Install language grammars if not already present.
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
      (unless (treesit-language-available-p lang)
        (display-warning 'init.el (format "Installing language grammar for `%s' ..." lang) :warning)
        (sleep-for 0.5)
        (treesit-install-language-grammar lang)
        (message "`%s' treesit language grammar installed." lang)))))


(defun my-add-eglot-format-on-save-hook ()
  "Register a buffer-local `before-save-hook' to format and organize imports."
  ;; The depth of -10 places this before eglot's willSave notification,
  ;; so that that notification reports the actual contents that will be saved.
  ;; See https://github.com/golang/tools/blob/master/gopls/doc/emacs.md.
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (add-hook 'before-save-hook (lambda () (call-interactively #'eglot-code-action-organize-imports)) -9 t))

(use-package eglot
  :straight (:type built-in)
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (cmake-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
	 (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure))
  :commands (eglot eglot-ensure)
  :diminish (eldoc-mode)
  :config
  ;; Automatically shut down server after killing last managed buffer.
  (setq eglot-autoshutdown t)
  ;; Require a manual restart (`eglot') when language servers crash.
  (setq eglot-autoreconnect nil)
  ;; Prevent long identifier documentation to be shown when cursor "hovers" over
  ;; an identifier.
  (setq eldoc-echo-area-use-multiline-p nil)
  ;; Be explicit about which LSP servers to use.
  (add-to-list 'eglot-server-programs '((c-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs '((c++-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs '((cmake-mode) . ("cmake-language-server")))
  (add-to-list 'eglot-server-programs '((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode)
                                        . ("typescript-language-server" "--stdio")))
  ;; See https://rust-analyzer.github.io/manual.html#emacs
  (add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) . ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

  ;; Additional gopls settings.
  ;; See https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
  ;;     https://github.com/golang/tools/blob/master/gopls/doc/settings.md
  (setq-default eglot-workspace-configuration
                '((:gopls
                   (:ui.completion.usePlaceholders . t)
                   ;; (:ui.diagnostic.staticcheck . t)
                   ;; For proper operation in files with certain build tags.
                   (:build.buildFlags . ["-tags=integration,db"]))))

  ;; If `xref-find-definitions' lands in a file outside the project, momentarily
  ;; consider that file managed by the same language server. This avoids
  ;; starting a new language server for such external files (startup cost).
  (setq eglot-extend-to-xref t)

  (defun my-find-workspace-symbol ()
    "Look up a workspace symbol by name.
Prompts the user for input. It does the equivalent of `C-u M-.'."
    (interactive)
    (setq current-prefix-arg '(1)) ;; programatically calls with prefix argument "C-u".
    (call-interactively 'xref-find-definitions))

  ;; Define key-bindings.
  (let ((m eglot-mode-map))
    (define-key m (kbd "<M-down>") #'xref-find-definitions)
    (define-key m (kbd "<M-up>")   #'xref-go-back)
    (define-key m (kbd "C-c f d")  #'xref-find-definitions)
    ;; find workspace symbol
    (define-key m (kbd "C-c f s")  #'my-find-workspace-symbol)
    (define-key m (kbd "C-c f i")  #'eglot-find-implementation)
    (define-key m (kbd "C-c f r")  #'xref-find-references)
    (define-key m (kbd "C-c C-r")  #'eglot-rename)
    (define-key m (kbd "C-c d")    #'eldoc)))


(use-package python
  :straight (:type built-in)
  :mode (("\\.py$" . python-mode))
  ;; note: no :ensure since it is already built into emacs
  :config
   ;; no tabs for indentation
  (setq indent-tabs-mode nil)
  ;; PEP-8 calls for a limit of 79 characters per line.
  (setq-local fill-column 79)
  ;; Always use four space for each indentation level.
  (setq-local python-indent-offset 4)
  (setq-local python-indent-guess-indent-offset 'nil)
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'python-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'python-mode-hook 'my-strip-on-save-hook))


;; Use sphinx-doc when python-mode is activated. Gives a templated docstring
;; when pressing C-c M-d in function head.
(use-package sphinx-doc
  :straight t
  :commands python-mode
  :config
  (sphinx-doc-mode))


(use-package go-ts-mode
  :straight (:type built-in)
  :mode (("\\.go$"  . go-ts-mode)
	 ("^go.mod$" . go-ts-mode))
  :config
  (message "go-ts-mode config ...")
  ;; Set up on-save hooks to have eglot format and organize imports.
  (add-hook 'go-ts-mode-hook 'my-add-eglot-format-on-save-hook)
  ;; Sets the fill column (where to break paragraphs on M-q)
  (add-hook 'go-ts-mode-hook (lambda () (setq fill-column 100))))


;; Debug programs using `delve'. This debugger relies on Emacs' GUD framework.
(use-package go-dlv
  :straight t
  :commands (dlv dlv-current-function)
  :config)

;; golangci-lint support via flymake
(use-package flymake-golangci
  :straight t
  :hook (go-mode . flymake-golangci-load))


;; Major mode for json file editing.
(use-package json-ts-mode
  :straight t
  :mode (("\\.json$" . json-ts-mode))
  :config
  (setq
   indent-tabs-mode nil
   js-indent-level 2) ; use 2 space indentation
  (setq indent-tabs-mode nil) ; no tabs for indentation
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'json-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'json-mode-hook 'my-strip-on-save-hook))


;; Major mode for JavaScript and React/JSX (built-into Emacs).
;; `js-mode` comes with syntax highlighting/indent support for JSX.
(use-package js
  :straight (:type built-in)
  :mode (("\\.js$" . js-ts-mode)
	 ("\\.jsx$" . js-ts-mode))
  :config
  (setq
   indent-tabs-mode nil
   js-indent-level 2))


;; Enable the Prettier code-formatter's minor mode to format on save whenever we
;; edit JavaSciprt/JSX.  https://prettier.io/.
(use-package prettier
  :straight t
  :hook ((json-ts-mode . prettier-mode)
	 (yaml-mode . prettier-mode)
	 (gfm-mode . prettier-mode)
	 (markdown-mode . prettier-mode)))


;; Major mode for yaml file editing.
(use-package yaml-mode
  :straight t
  :mode (("\\.yaml\\(.gotmpl\\)?$" . yaml-mode)
         ("\\.yml\\(.gotmpl\\)?$" . yaml-mode))
  :config
  (setq indent-tabs-mode nil) ; no tabs for indentation
  (add-hook 'yaml-mode-hook #'my-highlight-todos)
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'yaml-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'yaml-mode-hook 'my-strip-on-save-hook))


;; Major mode for markdown (.md) file editing.
(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md$" . gfm-mode)
         ("\\.md$" . gfm-mode)
         ("\\.markdown$" . gfm-mode)
         ;; cheat sheets under ~/dotfiles/cheat/sheets
         ("\\.cheat$" . markdown-mode))
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
    (define-key m (kbd "M-<right>") #'markdown-demote))

  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'markdown-mode-hook 'my-untabify-on-save-hook))


(use-package markdown-preview-mode
  :straight t
  ;; Lazily load when called for.
  :bind ("C-c p m" . markdown-preview-mode)
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-preview-stylesheets
        (list
         ;; style very similar to github's for markdown rendering
         "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css"
         ;; style that adds some margins
         "https://petergardfjall.github.io/css/emacs-markdown/github-markdown-body.css"
         ;; style for syntax highlighting of fenced codeblocks
         "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css"))
  (setq markdown-preview-javascript
        (list
         ;; javascript lib for syntax highlighting of fenced code blocks
         "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
         ;; javascript that applies the highlight js lib to the doc
         "https://petergardfjall.github.io/js/emacs-markdown/github-markdown-block-highlight.js")))


;; Major mode for AsciiDoc (.adoc) file editing.
(use-package adoc-mode
  :straight (adoc-mode :type git :host github :repo "bbatsov/adoc-mode"
                       :branch "master")
  :commands (adoc-mode)
  :mode (("\\.adoc$" . adoc-mode))
  :config
  (add-hook 'adoc-mode-hook #'my-highlight-todos)
  ;; Don't want orgtable "help" with formatting tables.
  (add-hook 'adoc-mode-hook (lambda () (orgtbl-mode -1))))

;; Varnish .vcl file editing.
(use-package vcl-mode
  :straight t
  :disabled t
  :mode (("\\.vcl$" . vcl-mode))
  :config
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'vcl-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'vcl-mode-hook #'my-strip-on-save-hook))


;; Dockerfile editing
(use-package dockerfile-mode
  :straight t
  :mode (("^.*Dockerfile$" . dockerfile-mode))
  :config
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'dockerfile-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'dockerfile-mode-hook #'my-strip-on-save-hook))


;; TOML editing
(use-package toml-mode
  :straight t
  :mode (("\\.toml$" . toml-mode))
  :config
  (add-hook 'toml-mode-hook #'display-line-numbers-mode)
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'toml-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'toml-mode-hook #'my-strip-on-save-hook))


(use-package terraform-mode
  :straight t
  :disabled t
  :mode (("\\.tf$" . terraform-mode))
  :config
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'terraform-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'terraform-mode-hook #'my-strip-on-save-hook))


(use-package protobuf-mode
  :straight t
  :mode (("\\.proto$" . protobuf-mode))
  :config
  (add-hook 'protobuf-mode-hook #'my-highlight-todos)
  (add-hook 'protobuf-mode-hook #'my-enable-line-numbers-mode)
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'protobuf-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'protobuf-mode-hook #'my-strip-on-save-hook))


;; Rust-mode
(use-package rust-mode
  :straight t
  :mode (("\\.rs$" . rust-mode))
  :config
  ;; Set up on-save hooks to have eglot format and organize imports.
  (add-hook 'rust-mode-hook 'my-add-eglot-format-on-save-hook)
  (setq indent-tabs-mode nil)
  ;; automatic formatting
  (setq rust-format-on-save t))


(defun my-c-mode-common ()
  "Apply common settings for 'c-mode' and 'c++-mode'."
  (setq c-basic-offset   4
        tab-width        4
        indent-tabs-mode nil)
  ;; enable use of clang-format
  (use-package clang-format
    :straight t
    :config
    ;; style to use when calling `clang-format-buffer`
    (setq clang-format-style "WebKit"))

  ;; add buffer-local save hooks
  (add-hook 'before-save-hook #'clang-format-buffer nil t))

(defun my-c-mode ()
  "Apply settings for 'c-mode'."
  (message "c-mode config ...")
  (my-c-mode-common))

(defun my-c++-mode ()
  "Apply settings for 'c++-mode'."
  (message "c++-mode config ...")
  (my-c-mode-common))


;; C and C++ setup.
(use-package cc-mode
  :straight (:type built-in)
  :hook cc-mode
  :config
  (add-hook 'c-mode-hook   #'my-c-mode)
  (add-hook 'c++-mode-hook #'my-c++-mode))


;; cmake setup.
(use-package cmake-mode
  :straight t
  :mode (("CMakeLists.txt$" . cmake-mode)
         ("\\.cmake$" . cmake-mode))
  :config
)


;; remove "ElDoc" from modeline
(use-package eldoc
  :straight (:type built-in)
  :diminish eldoc-mode)


;; can be used for working with .groovy and Jenkinsfile
(use-package groovy-mode
  :straight t
  :disabled t
  :mode (("\\.groovy$" . groovy-mode)
         ("\\.gvy$" . groovy-mode)
	 ("\\.gy$" . groovy-mode)
	 ("^Jenkinsfile$" . groovy-mode)))


;; emacs mode to edit GraphQL schemas and queries (automtically enabled when
;; opening .graphql and .gql files)
(use-package graphql-mode
  :straight t
  :mode (("\\.gql$" . graphql-mode)
         ("\\.graphql$" . graphql-mode))
  :hook ((graphql-mode . prettier-mode)))


;; `which-key' is a minor mode for Emacs that displays available key bindings
;; following your currently entered incomplete command.
(use-package which-key
  :straight (:type built-in)
  :diminish
  :config
  ;; Show popup at bottom of frame.
  (which-key-setup-side-window-bottom)
  ;; Delay popup appearance after starting to type a command.
  (setq which-key-idle-delay 1.0)
  (which-key-mode))


;; Built-in browse-url.el package.
(use-package browse-url
  :straight (:type built-in)
  ;; Lazily load when called for.
  :bind (("C-c u o" . browse-url-xdg-open))) ;; "URL open"


;; Make `orgtbl-mode' available in text buffers: "C-c |" creates a table.
(use-package org-table
  :commands orgtbl-mode
  :init
  (add-hook 'text-mode-hook #'orgtbl-mode))

;; org-mode
(use-package org
  :straight (:type built-in)
  ;; Lazily load when a .org file is opened.
  :mode ("\\.org$" . org-mode)
  ;; Lazily load when called for.
  :bind (("C-c o o" . my-org-open)
         ("C-c o l" . org-store-link)
         ("C-c o c" . org-capture)
         ("C-c o a" . org-agenda))
  :config
  ;;; Key-bindings.
  (let ((m org-mode-map))
    ;; x as in "check as done".
    (define-key m (kbd "C-c o x") #'org-archive-subtree)
    (define-key m (kbd "C-c o >") #'org-clock-in)
    (define-key m (kbd "C-c o <") #'org-clock-out)
    (define-key m (kbd "C-c C-s") #'org-schedule)
    (define-key m (kbd "C-c C-d") #'org-deadline)
    ;; jump to heading with live preview
    (define-key m (kbd "C-c o h") #'consult-outline))
  ;;;
  ;;; Basic configuration and appearence.
  ;;;
  ;; Default location to look for org files.
  (setq org-directory "~/org")
  ;; When entering org files start in folded `OVERVIEW' state?
  ;; Can also be configured per file with `#+STARTUP`.
  (setq org-startup-folded nil)
  ;; Always run in org-indent-mode (level by indent rather than asterisks).
  (setq org-startup-indented t)
  ;; character(s) to indicate a folded section
  (setq org-ellipsis "...")
  ;; Extend the org-mode markup to be fontified like markdown:
  ;; - surround with "`": verbatim/code face
  ;; - surround with "**": bold
  ;; - surround with "*": italic
  ;; - surround with "_": underline
  ;; - surround with "~": strike-through
  ;; This should work both for single-line sentences and (fill-column) folded
  ;; sentences.
  (font-lock-add-keywords 'org-mode
			  '(("`[^`]+`" 0 'org-verbatim prepend)
			    ("\\*\\*[^\\*]*\\*\\*" . 'bold)
			    ("\\*[^\\*]*\\*" . 'italic))
			  'append)
  (setq org-emphasis-alist '(("_" underline)
                             ("~" (:strike-through t))))
  ;;;
  ;;; Task/todo management.
  ;;;
  ;; Possible todo workflow states (use S-{left,right} to move throught states).
  (setq org-todo-keywords '((sequence "TODO" "STARTED" "BLOCKED" "|" "DONE" "SKIPPED")))
  ;; Save timestamp when a todo changes state to DONE.
  (setq org-log-done 'time)
  ;; Location where archived entries/subtrees are to be stored. Items are placed
  ;; under H1-headlines "Archived from `<file>'" where `<file>' is the file from
  ;; which the entry was archived.
  (setq org-archive-location "~/org/archive.org::* Archived from %s")
  ;; Add archived items as first entry under destination heading.
  (setq org-archive-reversed-order t)

  ;;;
  ;;; Agenda.
  ;;;
  ;; Files to include when compiling the agenda.
  (setq org-agenda-files '("~/org/work.org" "~/org/archive.org"))
  ;; Start week with Monday.
  (setq org-agenda-start-on-weekday 1)
  (setq calendar-week-start-day 1)

  ;;;
  ;;; Capture.
  ;;;
  ;; Default file to place captured notes unless specified in capture template.
  (setq org-default-notes-file (concat org-directory "/captured.org"))
  ;; Templates to select from on org-capture
  ;; - "%i": initial content (marked region  when capture was called).
  ;; - "%a": annotation, normally the link created with org-store-link.
  ;; - "%l": like %a, but only insert literal link.
  ;; - "%f": file visited by buffer from where org-capture was called.
  ;; - "%F": like %f but full path.
  ;; - "%?": after completing template, position point here.
  (setq org-capture-templates
        '(("w" "work" entry (file+headline "~/org/work.org" "Inbox")
           "* TODO %?")))

  (defun my-org-open ()
    "Interactively open a file in `org-directory`."
    (interactive)
    ;; using temp buffer avoids changing `default-directory` in current buffer
    (with-temp-buffer
      (let ((default-directory (format "%s/" org-directory)))
	(call-interactively #'find-file)))))


(use-package highlight-indent-guides
  :straight t
  :diminish
  :hook ((emacs-lisp-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character))


(use-package ruby-mode
  :straight (:type built-in)
  :mode (("\\.rb$"  . ruby-mode))
  :config)


;;; Finalization

(defun my-elapsed-time ()
  "Get elapsed time (float seconds) since Emacs was started."
  (float-time (time-subtract (current-time) emacs-start-time)))

;; Output the time at which the loading of the init-file itself completed.
(message "Loaded %s after %.3fs." load-file-name (my-elapsed-time))

;; Output the time at which the loading of all init-hooks completed.
(add-hook 'after-init-hook
          (lambda () (message "init-hooks done after %.3fs." (my-elapsed-time))) t)

(provide 'init)

;;; init.el ends here
