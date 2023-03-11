;; dired settings
;; Version Date : 2023-03-06
;; Packages: Package free!

;; allow dired to delete or copy dir
;; 'top means ask every time
;; 'always means no asking
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)
(defun xah-dired-mode-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 0))

(add-hook 'dired-mode-hook 'xah-dired-mode-setup)

(require 'dired )

(if (< emacs-major-version 28)
    (progn
      (define-key dired-mode-map (kbd "RET")
        'dired-find-alternate-file) ; was dired-advertised-find-file
      (define-key dired-mode-map (kbd "^")
        (lambda () (interactive)
          (find-alternate-file ".."))) ; was dired-up-directory
      )
  (progn
    (setq dired-kill-when-opening-new-dired-buffer t)))

(setq dired-listing-switches "-lt")
