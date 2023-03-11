;; IBUFFER
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)
(global-set-key (kbd "C-x C-c") nil)
(global-set-key (kbd "C-x C-c") 'ibuffer)

(defun ibuffer-visit-buffer (&optional single)
  "Visit the buffer on this line. If optional argument SINGLE is non-nil then  also ensure there is only one window." 
  (interactive "P")
  (let ((buf (ibuffer-current-buffer t)))
    (bury-buffer (current-buffer))
    (switch-to-buffer buf)
    (when single
      (delete-other-windows))))
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("LaTeX" (or (mode . latex-mode)
                            (mode . LaTeX-mode)
                            (mode . bibtex-mode)
                            (mode . reftex-mode)))
		       ("dired" (or (mode . dired-mode)))
               ("Emacs Lisp" (or (mode . emacs-lisp-mode)
                                 (name . "^.\\*emacs.\\*$")))
		       ("MATLAB" (mode . matlab-mode))
		       ("Org" (mode . org-mode))
		       ("Text" (mode . text-mode))
		       ("Web" (mode . html-mode))
		       ("Emacs*" (or
                          (name . "^\\*Apropos\\*$")
                          (name . "^\\*Buffer List\\*$")
                          (name . "^\\*Compile-Log\\*$")
                          (name . "^\\*Help\\*$")
                          (name . "^\\*info\\*$")
                          (name . "^\\*Occur\\*$")
                          (name . "^\\*scratch\\*$")
                          (name . "^\\*Messages\\*$")))))))
(setq ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook
          #'(lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-expert t)
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
           "Open ibuffer with cursor pointed to most recent buffer name"
           (let ((recent-buffer-name (buffer-name)))
             ad-do-it
             (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)
