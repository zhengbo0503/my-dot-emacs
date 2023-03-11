;; IDO
(setq recentf-save-file "~/.recentf")
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(setq recentf-auto-cleanup 120) ; Must use custom?
(global-set-key (kbd "M-0") 'recentf-open-files)

(defun xsteve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-0") 'xsteve-ido-choose-from-recentf)

(setq resize-mini-windows t)    ; grow and shrink as necessary
(setq max-mini-window-height 3) ; grow up to max of 3 lines
(add-hook 'minibuffer-setup-hook
      (lambda () (setq truncate-lines nil)))
(add-to-list 'load-path "~/Dropbox/org/myinit/ibuffer-vc")
(use-package ibuffer-vc)

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)  ;; Makes no difference!
(setq ido-create-new-buffer 'always)
(setq ido-enable-tramp-completion nil)
(setq ido-max-directory-size 1000000)
(setq ido-use-filename-at-point 'guess)  ;; Great on URL!
(setq ido-use-url-at-point t)
(setq ido-use-virtual-buffers t)         ;; Uses old buffers from recentf.
;; This doesn't seem to have any effect.  Time order is better, anyway?
(setq ido-file-extensions-order '(".tex" ".m" ".bib" ".txt" ".emacs"))
;; For Mac: ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Just press ~ to go home when in ido-find-file.
(add-hook 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       (if (looking-back "/")
           (insert "~/")
         (call-interactively 'self-insert-command))))))
