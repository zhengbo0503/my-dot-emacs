;; Org setting ;
;; Version Date : 2023-03-06
;; packages: Package free

;; (require 'org)
(setq-default fill-column 80)
(diminish 'abbrev-mode)
(eval-after-load "org" '(diminish 'orgstruct-mode "OrgS"))
(setq frame-title-format "%f - %p"); Titlebar contains buffer name
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq org-agenda-files (list "~/Dropbox/org/todo.org"))
(setq org-hide-emphasis-markers t)

;; latexmk
(setq org-latex-pdf-process (list "latexmk -pdf %f"))

;; notes
(setq org-default-notes-file (concat org-directory "/org/work.org"))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done nil)  ;; Add time stamp when move to DONE state.
(setq org-completion-use-ido t)
(setq org-completion-use-iswitchb nil)
(setq org-agenda-start-on-weekday nil)

;; Todo 
(setq org-todo-keywords
      '((sequence "TODO(t!)" "IN-PROGRESS(p!)" "|" "DONE(d!)")))
(setq org-todo-keyword-faces
      '(("TODO" . "red") ("IN-PROGRESS" . "magenta") ("DONE" . "green")))
(setq org-mobile-inbox-for-pull "/Users/clement/Dropbox/org/todo.org")

;; Turn on indentation and auto-fill mode for Org files
(org-indent-mode)
(variable-pitch-mode 1)
(auto-fill-mode 0)
(visual-line-mode 1)
(diminish org-indent-mode)

(defun efs/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.05)
                  (org-level-2 . 1.0)
                  (org-level-3 . 1.0)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(setq org-ellipsis " ▾")
(efs/org-font-setup)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   ))

(push '("conf-unix" . conf-unix) org-src-lang-modes)


;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/Dropbox/elisp/orginit.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
