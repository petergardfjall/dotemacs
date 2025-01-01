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
(require 'my-documentation)
(require 'my-data-formats)
(require 'my-global-keymap)
(require 'my-navigation)
(require 'my-orgmode)
(require 'my-progmodes)
(require 'my-projects)
(require 'my-recording)
(require 'my-search)
(require 'my-shell)
(require 'my-version-control)




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




;; transparently open compressed files
(use-package auto-compression-mode
  :straight (:type built-in)
  :defer 5
  :config
  (auto-compression-mode t))

;;;
;;; Development/coding
;;;

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
  (add-to-list 'major-mode-remap-alist '(js-mode     . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(ruby-mode   . ruby-ts-mode))
  (add-to-list 'major-mode-remap-alist '(sql-mode    . sql-ts-mode))
  (add-to-list 'major-mode-remap-alist '(toml-mode   . toml-ts-mode))
  ;; Specify which tree-sitter language grammar defintions to use.
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
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
  :commands (eglot eglot-ensure)
  :diminish (eldoc-mode)
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (cmake-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure))
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
  ;; `autoload' whenever `prettier-mode' is invoked.
  :commands (prettier-mode))

;; Varnish .vcl file editing.
(use-package vcl-mode
  :straight t
  :disabled t
  :mode (("\\.vcl$" . vcl-mode))
  :config
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'vcl-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'vcl-mode-hook #'my-strip-on-save-hook))




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


;; Built-in browse-url.el package.
(use-package browse-url
  :straight (:type built-in)
  ;; Lazily load when called for.
  :bind (("C-c u o" . browse-url-xdg-open))) ;; "URL open"


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
