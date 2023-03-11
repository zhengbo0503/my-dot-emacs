;; 2023-03-01T23:45:04+00:00
;; This file defines my own functions

(defun insert-latex-template-ZZ ()
  "Insert latex template to the current LaTeX file
  Version Date : 2023-03-02" 
  (interactive)
  (insert-file "~/Dropbox/tex/template/do.tex")
  (message "%s""Insertion complete from ~/Dropbox/tex/template/do.tex")
  )

(defun open-in-vscode-ZZ ()
  "open current buffer in vscode
  Version Date : 2023-03-02"
  (interactive)
  (let (($path (if (buffer-file-name) (buffer-file-name) default-directory )))
    (shell-command
     (format "open -a Visual\\ Studio\\ Code.app \"%s\"" $path)))
  )

(defun move-down-five-lines-ZZ ()
  "move down 10 lines
  Version Date : 2023-03-02"
  (interactive)
  (next-line)
  (next-line)
  (next-line)
  (next-line)
  (next-line)
  )

(defun move-up-five-lines-ZZ ()
  "move down 5 lines
  Version Date : 2023-03-02"
  (interactive)
  (previous-line)
  (previous-line)
  (previous-line)
  (previous-line)
  (previous-line)
  )
(global-set-key (kbd "s-<up>") 'move-up-five-lines-ZZ)
(global-set-key (kbd "s-<down>") 'move-down-five-lines-ZZ)


(defun open-tex-template-file-ZZ ()
  "open the latex template for us to modify
  Version Date : 2023-03-02"
  (interactive)
  (switch-to-buffer (find-file-noselect "~/Dropbox/tex/template/do.tex"))
  )

;; notice this function is only valid in MACOS, moreover, due to the function
;; does not recognize spaces, therefore you need to make an alias of your adobe
;; reader, and then point to the alias. Namely, you "path_to_application"
;; shouldn't include any space, otherwise this will failed.
(defun open-in-acrobat-ZZ (arg)
  "Open PDF of current LaTeX buffer in Acrobat.
  Version Date : 2023-03-04"
  (interactive "p")
  (shell-command
   (concat "open -a /Applications/adobe_reader.app "
   (file-name-sans-extension (buffer-file-name)) ".pdf" )
   ))
(global-set-key (kbd "<escape> oa") 'open-in-acrobat-ZZ) ;; ctrl+shift+F5

(defun open-in-finder-ZZ ()
  "Show current file in desktop.
 (Mac Finder, File Explorer, Linux file manager)
This command can be called when in a file buffer or in `dired'.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version: 2020-11-20 2022-04-20 2022-08-19"
  (interactive)
  (let (($path (if (eq major-mode 'dired-mode)
                   (if (eq nil (dired-get-marked-files))
                       default-directory
                     (car (dired-get-marked-files)))
                 (if buffer-file-name buffer-file-name default-directory))))
    (cond
     ((string-equal system-type "windows-nt")
      (shell-command (format "PowerShell -Command invoke-item '%s'" (expand-file-name default-directory )))
      ;; (let (($cmd (format "Explorer /select,%s"
      ;;                     (replace-regexp-in-string "/" "\\" $path t t)
      ;;                     ;; (shell-quote-argument (replace-regexp-in-string "/" "\\" $path t t ))
      ;;                     )))
      ;;   (shell-command $cmd))
      )
     ((string-equal system-type "darwin")
      (shell-command
       (concat "open -R " (shell-quote-argument $path))))
     ((string-equal system-type "gnu/linux")
      (call-process shell-file-name nil nil nil
                    shell-command-switch
                    (format "%s %s"
                            "xdg-open"
                            (file-name-directory $path)))
      ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. eg with nautilus
      ))))
(global-set-key (kbd "<escape> of") 'open-in-finder-ZZ) ;; Esc f

(defun open-in-terminal-ZZ ()
  "Open the current dir in a new terminal window.
On Microsoft Windows, it starts cross-platform PowerShell pwsh. You
need to have it installed.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version: 2020-11-21 2021-07-21 2022-08-04"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (let (($cmdstr
           (format "pwsh -Command Start-Process pwsh -WorkingDirectory %s" (shell-quote-argument default-directory))))
      (shell-command $cmdstr)))
   ((string-equal system-type "darwin")
    (shell-command (concat "open -a terminal " (shell-quote-argument (expand-file-name default-directory)))))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil)) (start-process "" nil "x-terminal-emulator" (concat "--working-directory=" default-directory))))
   ((string-equal system-type "berkeley-unix")
    (let ((process-connection-type nil)) (start-process "" nil "x-terminal-emulator" (concat "--working-directory=" default-directory))))))
(global-set-key (kbd "<escape> ot") 'open-in-terminal-ZZ) ;; Esc f


(defun copy-file-path-ZZ (&optional DirPathOnlyQ)
  "Copy current buffer file path or dired path.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
If in dired, copy the current or marked files.
If a buffer is not file and not dired, copy value of `default-directory'.
URL `http://xahlee.info/emacs/emacs/emacs_copy_file_path.html'
Version Date : 2023-03-04"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if DirPathOnlyQ
         (progn
           (message "Directory copied: %s" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: %s" $fpath)
         $fpath )))))

(defun new-empty-buffer-ZZ ()
  "Create a new empty buffer
New buffer named untitled
It returns the buffer (for elisp programming)
Version Date : 2023-03-05"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))
;; new buffer
(setq initial-major-mode (quote emacs-lisp-mode))
;; initial buffer use my empty buffer 
(global-set-key (kbd "<escape> n") 'new-empty-buffer-ZZ)
(global-set-key (kbd "<escape> b") 'dired-up-directory)
