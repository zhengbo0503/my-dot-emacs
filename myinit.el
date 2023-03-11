(load "~/Dropbox/elisp/settings/emacs-basic-settings.el") ;; V
(load "~/Dropbox/elisp/settings/loadpackage.el") ;; V
(load "~/Dropbox/elisp/settings/dired-settings.el") ;; V
(load "~/Dropbox/elisp/settings/latex-settings.el")
(load "~/Dropbox/elisp/settings/emacs-backup-settings.el")
(load "~/Dropbox/elisp/settings/org-settings.el")
(load "~/Dropbox/elisp/settings/matlab-mode-settings.el")
(load "~/Dropbox/elisp/settings/ibuffer-settings.el")
(load "~/Dropbox/elisp/settings/misc-pkgs.el")
(load "~/Dropbox/elisp/settings/my-functions.el")
(load "~/Dropbox/elisp/settings/org-settings.el")

;; I don't know where to put this function
(global-set-key (kbd "C-s") 'swiper-isearch)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (setq truncate-lines -1)))

(add-hook 'dired-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (setq truncate-lines -1)))
