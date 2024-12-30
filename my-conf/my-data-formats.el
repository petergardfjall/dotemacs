;;; package --- Configuration of data format modes.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)


;; Major mode for json file editing.
(use-package json-ts-mode
  :straight (:type built-in)
  :mode (("\\.json$" . json-ts-mode))
  :config
  (setq
   json-ts-mode-indent-offset 2
   indent-tabs-mode nil))


;; TOML editing
(use-package toml-ts-mode
  :straight t
  :mode (("\\.toml$" . toml-ts-mode))
  :config
  ;; Add buffer-local save hook only for buffers in this mode.
  (add-hook 'toml-ts-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'toml-ts-mode-hook #'my-strip-on-save-hook))


;; Major mode for yaml file editing.
(use-package yaml-ts-mode
  :straight t
  :mode (("\\.yaml\\(.gotmpl\\)?$" . yaml-ts-mode)
         ("\\.yml\\(.gotmpl\\)?$" . yaml-ts-mode))
  :config
  (setq indent-tabs-mode nil) ; no tabs for indentation
  (add-hook 'yaml-mode-hook #'my-highlight-todos))


(provide 'my-data-formats)
;;; my-data-formats.el ends here.
