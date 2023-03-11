;; Settings without need of packages
;; Version Date : 2023-03-06
;; Packages : Package Free!

;;;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

;;;; miscallenous settings
(setq gc-cons-threshold (* 50 1000 1000))
(setq inhibit-splash-screen t)       ; Don't want splash screen.
(setq inhibit-startup-message t)     ; Don't want any startup message.
(scroll-bar-mode 1)                  ; Turn off scrollbars.
(tool-bar-mode 0)                    ; Turn off toolbars.
(fringe-mode 0)                      ; Turn off left and right fringe cols.
(setq visible-bell 1)                ; Turn off sound.
(context-menu-mode)                  ; Right click instead of middle button.
(setq org-support-shift-select t)
(setq confirm-nonexistent-file-or-buffer nil)
(fset 'yes-or-no-p 'y-or-n-p)        ; Change yes/no questions to y/n type.
(setq default-directory "~/Dropbox/")
(setq sentence-end-double-space nil)
(setq mode-line-compact t)
(setq mouse-highlight nil)
(setq cursor-type 'box)
(modify-all-frames-parameters (list (cons 'cursor-type 'box)))
(blink-cursor-mode -1)
(column-number-mode 1)
(setq scroll-preserve-screen-position 1)
(global-font-lock-mode t) 
(global-visual-line-mode 1)
(setq frame-resize-pixelwise t)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-auto-revert-mode t)
(delete-selection-mode 1)
(show-paren-mode 1) ;; show parenthesis
(setq show-paren-style 'parenthesis)
(setq suggest-key-bindings nil) ;; stop suggesting key bindings
(setq use-package-verbose t)
(display-battery-mode 1)
(global-set-key (kbd "C-x j") 'find-file-at-point)

;; only show line numbers in "code" environment
(add-hook 'prog-mode-hook #'display-line-numbers-mode) 
(add-hook 'Latex-mode-hook #'display-line-numbers-mode) 


;;;; Config <TAB>
;; indent
(setq-default tab-always-indent 'complete)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;; smart tab
(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if 
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (hippie-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "^ *")
         (indent-for-tab-command)
         (hippie-expand nil)
          ))))
(global-set-key (kbd "TAB") 'smart-tab)
(define-key global-map (kbd "RET") 'newline-and-indent)

;;;; comment regions, bind to s-/
(defun comment-current-line-dwim ()
  "Comment or uncomment the current line."
  (interactive)
  (save-excursion
    (if (use-region-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (push-mark (beginning-of-line) t t)
      (end-of-line)
      (comment-dwim nil))))
(global-set-key (kbd "s-/") 'comment-current-line-dwim)

;; word warp
(setq-default word-wrap t)
(setq-default fill-column 75)
(defun toggle-fill-column ()
    "Toggle setting fill column between 72 and 75"
    (interactive)
    (setq fill-column (if (= fill-column 75) 72 75)))

;;;; Font
;; Set the default frame font 
(set-face-attribute 'default nil :font "Fira Code Retina" :height 160)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 160)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font "Cantarell"
                    :height 160
                    :weight 'regular)

;;;; initial window and default window
(setq default-frame-alist
      (if (display-graphic-p)
          '((tool-bar-lines . 0)
            (width . 90)
            (height . 47))
        '((tool-bar-lines . 0)
          (fringe-mode . 0)
          )))

;; Xah color theme honeydew
;; (add-to-list 'default-frame-alist '(foreground-color . "black"))
;; (add-to-list 'default-frame-alist '(background-color . "honeydew"))
;; (add-to-list 'default-frame-alist '(cursor-color . "black")) 
;; (set-foreground-color "black")
;; (set-background-color "honeydew") 
;; (set-cursor-color "#023020")
;; (set-face-background 'region "#EBE097")
;; (global-hl-line-mode 1) ;; highlight current line
;; (set-face-background 'hl-line "#CBF0BD")

(add-to-list 'default-frame-alist '(foreground-color . "white"))
(add-to-list 'default-frame-alist '(background-color . "black"))
(add-to-list 'default-frame-alist '(cursor-color . "yellow")) 
(set-foreground-color "white")
(set-background-color "black") 
(set-cursor-color "yellow")
(set-face-background 'region "#8b8682")
(global-hl-line-mode -1) ;; highlight current line

