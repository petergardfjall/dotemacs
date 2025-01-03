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

(autoload 'my-treesit-install-language "my-treesit-functions.el"
  "Install/update a treesit grammar for LANG." t)

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
(require 'my-debug)
(require 'my-global-keymap)
(require 'my-navigation)
(require 'my-orgmode)
(require 'my-progmodes)
(require 'my-projects)
(require 'my-recording)
(require 'my-search)
(require 'my-shell)
(require 'my-version-control)




;; transparently open compressed files
(use-package auto-compression-mode
  :straight (:type built-in)
  :defer 5
  :config
  (auto-compression-mode t))

;;;
;;; Development/coding
;;;

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
