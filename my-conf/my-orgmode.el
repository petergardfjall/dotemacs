;;; package --- `org-mode'-related configuration.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)


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

(provide 'my-orgmode)
;;; my-orgmode.el ends here.
