;;; package --- Package configuration related to projects.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)

(use-package project
  :straight (:type built-in)
  :config
  ;; Ignore .venv directory on calls to `project-find-file'.
  (add-to-list 'vc-directory-exclusion-list ".venv" t)
  (add-to-list 'vc-directory-exclusion-list "tmp" t)
  (global-set-key (kbd "C-c f f") #'project-find-file))


(use-package projtree
  ;; :load-path "~/dev/git/emacs-projtree"
  :straight (emacs-projtree
             :type git :host github :repo "petergardfjall/emacs-projtree" :branch "main")
  :commands (projtree-mode)
  :bind (("<f8>" . projtree-mode))
  :config
  (setq projtree-profiling-enabled nil)
  (setq projtree-show-git-status t))


(use-package wsp
  :straight (emacs-wsp
             :type git :host github :repo "petergardfjall/emacs-wsp")
  ;; Lazily load when called for.
  :bind (("C-x w o"   . wsp-workspace-open)
	 ("C-x w k"   . wsp-workspace-close)
	 ("C-x w c"   . wsp-workspace-current)
	 ("C-x w d"   . wsp-workspace-delete)
	 ("C-x w p a" . wsp-project-add)
	 ("C-x w p d" . wsp-project-delete)
	 ("C-x w p s" . wsp-project-switch)
	 ("C-x w p c" . wsp-project-close)
	 ("C-x w p k" . wsp-project-close-current)
	 ("C-x w p K" . wsp-project-close-other)))


(provide 'my-projects)
;;; my-projects.el ends here.
