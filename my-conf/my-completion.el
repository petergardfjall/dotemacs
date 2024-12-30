;;; package --- Completion-related configuration.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)

;; Basic settings for `completing-read' (minibuffer completion) and, to some
;; extent, also `complete-at-point' (buffer completion).
;;
;; Also see:
;; https://www.masteringemacs.org/article/understanding-minibuffer-completion
(use-package minibuffer
  :straight (:type built-in)
  :config
  ;; Determine how to match minibuffer input text against completion candidates.
  (setq completion-styles '(substring basic))

  ;; Ignore case on various forms of `completing-read' (minibuffer completion).
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t))


;; Icomplete is a built-in UI for `completing-read' ("minibuffer
;; completion"). We configure it to use vertical candidate display in the
;; minibuffer. Also see:
;; https://www.masteringemacs.org/article/understanding-minibuffer-completion
(use-package icomplete
  :straight (:type built-in)
  :config
  ;; Display candidates in a vertical list.
  (icomplete-vertical-mode 1)
  ;; Move point through list rather than rotate first entry.
  (setq icomplete-scroll t)
  (setq icomplete-show-matches-on-no-input t)
  ;; Make more responsive.
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-compute-delay 0.0)
  ;; Don't bring up help dialog on failure to complete.
  (setq completion-auto-help nil)
  ;; Control how matches are ordered in *Completions* buffer on calls to
  ;; `minibuffer-completion-help'.
  (setq completions-format 'one-column)
  ;; Truncate long completion candidate lines in the minibuffer.
  (add-hook 'icomplete-minibuffer-setup-hook
            (lambda () (setq-local truncate-lines t)))
  ;; Implement page-wise scrolling since that's not yet provided.
  (defun my-icomplete-page-up ()
    (interactive)
    (let* ((shown-candidates (- (window-total-height) 1))
           (scroll (/ shown-candidates 2)))
      (dotimes (_ scroll) (icomplete-backward-completions))))
  (defun my-icomplete-page-down ()
    (interactive)
    (let* ((shown-candidates (- (window-total-height) 1))
           (scroll (/ shown-candidates 2)))
      (dotimes (_ scroll) (icomplete-forward-completions))))
  ;;
  ;; Key bindings.
  ;;
  (let ((m icomplete-minibuffer-map))
    (define-key m (kbd "<return>") #'icomplete-force-complete-and-exit)
    ;; For `find-file' this allows forcing creation of a file whose name matches
    ;; one of the completion candidates.
    (define-key m (kbd "C-<return>") #'icomplete-ret)
    (define-key m (kbd "<SPC>") nil))
  (let ((m minibuffer-local-completion-map))
    ;; Open *Completions* buffer. Can be useful when there are a lot of
    ;; candidates and `icomplete' won't allow paging through results.
    (define-key m (kbd "?")       #'minibuffer-completion-help)
    (define-key m (kbd "<prior>") #'my-icomplete-page-up)
    (define-key m (kbd "<next>")  #'my-icomplete-page-down)))


;; Adds richer annotations for minibuffer completions for any `completing-read'
;; compatible framework (such as icomplete, selectrum and vertico).
(use-package marginalia
  :straight t
  ;; The :init configuration is always executed (Not lazy!)
  :init
  (marginalia-mode)
  (defun my-project-buffer-annotator (cand)
    (let* ((buffer (get-buffer cand)))
      (when-let* ((buffer-file (buffer-file-name buffer))
                  (buffer-proj (project-current nil buffer-file))
	          (project-dir (project-root buffer-proj))
	          (project-short (project-name buffer-proj)))
        (let ((project-rel-dir (file-name-directory (file-relative-name buffer-file project-dir))))
	  (marginalia--fields
	   (project-short :truncate 0.4 :face 'marginalia-value)
	   (project-rel-dir :truncate 0.4 :face 'marginalia-documentation))))))
  ;; update annotator-registry to use my custom annotator for buffers
  (add-to-list 'marginalia-annotator-registry
               '(buffer my-project-buffer-annotator none))
  :config
  (let ((m minibuffer-local-map))
    (define-key m (kbd "M-A") #'marginalia-cycle)))


;; Orderless provides another type of `completion-styles' for `completing-read'
;; where space-separated words can be input as search terms.
(use-package orderless
  :straight t
  :init
  ;; Note: we do not just add `orderless' to the default
  ;; `completion-styles'. Instead we apply it very selectively to certain
  ;; completion categories. In particular, we avoid `orderless' with eglot
  ;; completions, where it appears to not always play well.
  (setq completion-category-overrides
        '(;; Use orderless for `switch-to-buffer'.
          (buffer (styles orderless))
          ;; Use basic completion and then orderless for `open' (C-x C-f).
          (file (styles basic orderless))
          ;; Use orderless for `project-find-file'.
          (project-file (styles orderless))
          ;; Use orderless for `describe-variable' (C-h v).
          (variable (styles orderless))
          ;; Use orderless for `describe-function' (C-h f).
          (function (styles orderless))
          ;; Use orderless for `execute-extended-command' (M-x).
          (command (styles orderless))
          ;; Use orderless for different variants of `consult-grep'.
          (consult-grep (styles orderless))
          ;; Use orderless for different variants of `consult-line'.
          (consult-location (styles orderless)))))


;; Company is a UI for `completion-at-point' ("in-buffer completion"). It shows
;; completion candidates in a popup overlay. Completions candidates are provided
;; by `completion-at-point-functions' such as `eglot-completion-at-point'.
(use-package company
  :straight t
  :diminish
  :init
  (global-company-mode)
  :config
  ;; Minimum prefix length before triggering auto-completion. Note that
  ;; completion can be summoned at any time with 'C-<tab>'
  ;; (`completion-at-point').
  (setq company-minimum-prefix-length 3)
  ;; Decrease delay until completion popup shows.
  (setq company-idle-delay 0.1)
  ;; Only use company with `complation-at-point-functions'.
  (setq company-backends '(company-capf))
  ;; Maximum number of candidates to display.
  (setq company-tooltip-limit 15)
  ;; miniumum popup width (in characters)
  (setq company-tooltip-minimum-width 20)
  ; Start autocompletion only after typing.
  (setq company-begin-commands '(self-insert-command))
  ;; Align annotations (e.g. function signatures) to the right tooltip border.
  (setq company-tooltip-align-annotations t)
  ;; Summon the completion popup.
  (define-key company-mode-map (kbd "C-<tab>") #'company-complete))


(provide 'my-completion)
;;; my-completion.el ends here.
