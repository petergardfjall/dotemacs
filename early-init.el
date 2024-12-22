;;; early-init.el --- Pre-init file emacs configuration.
;;; Commentary:
;;;
;;; Executed during Emacs startup earlier than the normal init file is
;;; processed.  This file is loaded before the package system and GUI is
;;; initialized, so it can be used to customize variables that affect frame
;;; appearance as well as the package initialization process.
;;;
;;; Code:

;; Time for starting to load this file.
(defconst my-early-init-start-time (current-time))

;;
;; Use straight.el over package.el for package management.
;;

;; Disable package.el in favor of straight.el.
(setq package-enable-at-startup nil)

;; Customize when straight rebuilds packages: (1) on `M-x straight-check-all` or
;; (2) when a file under straight/repos/ is saved.
;; By default straight uses a bulk `find(1)` command on emacs startup (the
;; `'find-at-startup` option), which adds about a second to the startup time.
(defvar straight-check-for-modifications '(find-when-checking check-on-save))


;;
;; Inhibit certain UI features before we have initialized the windowing
;; environment.
;;
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
;; Hide vertical scrollbar on right
(scroll-bar-mode -1)
;; Hide tool-bar (icons, such as open file, cut, paste, etc)
(tool-bar-mode -1)
;; title bar: emacs27@miniac:/dir/path
(setq frame-title-format
      (concat invocation-name "@" (system-name) ":" default-directory))


;; Avoid garbage collection performance-penalty on startup by temporarily
;; bumping the memory threshold to `most-positive-fixnum'. A more sensible value
;; is set further downin the `emacs-startup-hook' a few lines below.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Bumping the GC threshold is beneficial for language servers.
            (setq gc-cons-threshold (* 8 1024 1024) ;; 8 MiB (default: 800kB)
                  gc-cons-percentage 0.1))) ;; Re-set default.

;; Bumping the subprocess read chunk size is beneficial for language servers.
(setq read-process-output-max (* 4 1024 1024)) ;; 4 MiB (default: 64kB)
(setq process-adaptive-read-buffering nil)


(defconst my-early-init-duration
   (float-time (time-subtract (current-time) my-early-init-start-time))
   "The elapsed time since start of early-init.el loading.")

(provide 'early-init)

;;; early-init.el ends here
