;; Backup 
(setq delete-old-versions t ; delete excess backup files silently
      delete-by-moving-to-trash t
      make-backup-files t    ; backup of a file the first time it is saved.
      backup-by-copying t    ; don't clobber symlinks
      version-control t      ; version numbers for backup files
      delete-old-versions t  ; delete excess backup files silently
      delete-auto-save-files nil  ; keep autosave when saving file
      kept-old-versions 0    ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 10   ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t    ; auto-save every buffer that visits a file
      auto-save-timeout 30   ; secs idle time before auto-save (default: 30)
      auto-save-interval 200 ; keystrokes between auto-saves (default: 300)
      vc-make-backup-files t ; backup versioned files, not done by default
      )
(progn
  (if (not (file-exists-p "/Users/clement/emacs_backups/"))
      (make-directory "/Users/clement/emacs_backups/" t))
  (setq backup-directory-alist
        `((".*" . , "/Users/clement/emacs_backups/")))
  (setq auto-save-file-name-transforms   ;; Needed, else goes in curr dir!
        `((".*" , "/Users/clement/emacs_backups/" t))))
