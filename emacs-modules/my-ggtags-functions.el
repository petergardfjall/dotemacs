;;; package --- Assorted ggtags-related functions.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;
(require 'ggtags)
(require 'project)

(message "loading %s ..." load-file-name)

(defun my-ggtags-create ()
  "Create `GTAGS' in the root directory of the current buffer's project."
  (interactive)
  (require 'project)
  (if-let (proj-root (or (project-root (project-current)) default-directory))
      (progn
        (message "Generating tags in %s ..." proj-root)
        (ggtags-create-tags proj-root))
    (error "Did not find a project root dir in which to generate tags")))

(defun my-ggtags-find-definition  ()
  "Replacement for `ggtags-find-definition' that will always prompt.
This function always prompts whereas the default behavior is to just
search for symbol at point if there is one)."
  (interactive)
  ;; can read 'definition, 'symbol, 'reference, 'id, 'path
  (let ((tag (ggtags-read-tag 'definition t "Find definition [GTAGS]")))
    (ggtags-find-definition tag)))

(defun my-ggtags-find-reference  ()
  "Replacement for `ggtags-find-reference' that will always prompt.
This function always prompts whereas default behavior is to just search
for symbol at point if there is one."
  (interactive)
  ;; can read 'definition, 'symbol, 'reference, 'id, 'path
  (let ((ref (ggtags-read-tag 'reference t "Find reference [GTAGS]")))
    (ggtags-find-reference ref)))

(provide 'my-ggtags-functions)
;;; my-ggtags-functions.el ends here.
