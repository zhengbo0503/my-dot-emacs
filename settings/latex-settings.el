;; AucTeX & RefTeX & cdLaTeX
;; Version Date : 2023-03-06
;; Packages:
;; auctex
;; auctex-autocomplete
;; cdlatex

(add-to-list 'load-path "~/Dropbox/elisp/pkgs/auto-complete")
(require 'auto-complete)

(use-package auctex
  :defer t
  :ensure t)
(use-package auto-complete-auctex)

;; cdlatex and misc
(add-to-list 'load-path "~/Dropbox/elisp/pkgs/cdlatex")
(require 'cdlatex)
(setq cdlatex-simplify-sub-super-scripts nil)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
;; (cdlatex-mode)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'display-line-numbers-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq font-latex-fontify-sectioning 1.0)
(setq font-latex-fontify-sectioning 'color)

(add-hook 'TeX-mode-hook #'(lambda ()
  (remove-hook 'find-file-hooks (car find-file-hooks) 'local)))
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(global-set-key (kbd "C-c [") 'reftex-citation) ;; For all modes.

;; function for full citation instead of just bibkey
(defun my-cite-hook ()
  (local-set-key (kbd "C-c m") 'my-cite))

;; View and open latex
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-method 'synctex)
(progn
  (setq TeX-view-program-list
        '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")
          ("Preview" "open -a Preview.app %o"))))
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (assq-delete-all 'output-pdf TeX-view-program-selection)
            (add-to-list 'TeX-view-program-selection '(output-pdf "Skim"))))

;; open server in order to do SyncTeX
;; (use-package server
;;   :config
;;   (unless (server-running-p)
;;     (server-start)))
(server-mode)

;; helm require the following packages
(add-to-list 'load-path "~/Dropbox/elisp/pkgs")
(require 'async)
(require 'parsebib)
(require 'dash)
(require 'f)
(require 's)
(require 'f-shortdoc)
(add-to-list 'load-path "~/Dropbox/elisp/pkgs/biblio.el")
(require 'biblio)

;; Helm bibtex
(add-to-list 'load-path "~/Dropbox/elisp/pkgs/helm") ;; provide helm-config
(require 'helm-config)
(add-to-list 'load-path "~/Dropbox/elisp/pkgs/helm-bibtex")
(require 'helm-bibtex)

;; load my bib files 
(setq bibtex-completion-bibliography '
      ("/Users/clement/Dropbox/bibtex/ref.bib"
       "/Users/clement/Dropbox/bibtex/misc.bib"))
(setq bibtex-completion-library-path '("/Users/clement/Dropbox/pdf_papers/"
                                       "/Users/clement/Dropbox/pdf_books/" )) 
(setq bibtex-completion-pdf-symbol "⌘")
(setq bibtex-completion-notes-path "/Users/clement/Dropbox/reading_notes/")
(setq bibtex-completion-notes-extension ".tex")
;; open file from helm-bibtex
(setq bibtex-completion-pdf-open-function
      (setq bibtex-completion-open-pdf
            (lambda (fpath)
              (call-process "open" nil 0 nil "-a" "/Applications/Skim.app"
                            fpath))))
(setq helm-bibtex-pdf-open-function
    (lambda (fpath)
    ;; (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath)
    (call-process "open" nil 0 nil fpath)
    ))

;;;; RefTeX
(setq reftex-default-bibliography
      (quote
       ("ref.bib" "misc.bib" "strings.bib")))
;; work.bib is solely for entries I want to include but not cite.

(setq reftex-ref-macro-prompt nil)
(setq reftex-label-alist
'(
("algorithm"   ?a "alg."  "~\\ref{%s}" t   ("Algorithm"))
("corollary"   ?c "cor."  "~\\ref{%s}" t   ("Corollary"))
("definition"  ?d "def."  "~\\ref{%s}" nil ("Definition" "Definitions"))
(nil           ?e ""      "~\\eqref{%s}" nil nil )  ; equation, eqnarray
(nil           ?i ""      "~\\ref{%s}" nil nil )  ; item
("example"     ?z "ex."   "~\\ref{%s}" t   ("Example" "Examples"))
("figure"      ?f "fig."  "~\\ref{%s}" t   ("Figure" "Figures"))
("lemma"       ?l "lem."  "~\\ref{%s}" t   ("Lemma"))
("assumption"  ?m "ass."  "~\\ref{%s}" t   ("Assumption"))
("problem"     ?x "prob." "~\\ref{%s}" t   ("Problem"))
("proposition" ?p "prop." "~\\ref{%s}" t   ("Proposition"))
("code"        ?n "line." "~\\ref{%s}" nil nil) ;; Doesn't work!
(nil           ?s "sec."  "~\\ref{%s}" nil nil)
("table"       ?t "table."  "~\\ref{%s}" t   ("Tables" "Tables"))
("theorem"     ?h "thm."  "~\\ref{%s}" t   ("Theorem" "Theorems"))
	)
)
(setq reftex-insert-label-flags (quote ("s" "seftacihlpz")))
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (LaTeX-add-environments
	     '("algorithm" LaTeX-env-label)
	     '("corollary" LaTeX-env-label)
	     '("definition" LaTeX-env-label)
	     '("example" LaTeX-env-label)
	     '("lemma" LaTeX-env-label)
	     '("mylist2" nil)
	     '("problem" LaTeX-env-label)
	     '("proposition" LaTeX-env-label)
	     '("theorem" LaTeX-env-label)
	     )
	    )
	  )
(setq LaTeX-eqnarray-label ""
      LaTeX-equation-label ""
      LaTeX-figure-label "fig"
      LaTeX-table-label "table"
      LaTeX-indent-level 0  ; default 2
      LaTeX-item-indent 0   ; default 2
)

(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
(global-set-key (kbd "C-x &") nil) 
(global-set-key (kbd "C-c &") 'reftex-view-crossref)
  
;; get-bibtex-from-doi
(defun get-bibtex-from-doi (doi)
 "Get a BibTeX entry from the DOI"
 (interactive "MDOI: ")
 (let ((url-mime-accept-string "text/bibliography;style=bibtex"))
   (with-current-buffer 
     (url-retrieve-synchronously 
       (format "http://dx.doi.org/%s" 
       	(replace-regexp-in-string "http://dx.doi.org/" "" doi)))
     (switch-to-buffer (current-buffer))
     (goto-char (point-max))
     (setq bibtex-entry 
     	  (buffer-substring 
          	(string-match "@" (buffer-string))
              (point)))
     (kill-buffer (current-buffer))))
 (insert (decode-coding-string bibtex-entry 'utf-8))
 (bibtex-fill-entry))
  
;; let auctex recognize ur custom macro 
(setq font-latex-match-italic-command-keywords
      '(
        ;; ("cite" "[{")
        ("ycite" "{")
        ("mc" "{")
        ("mb" "{")
        ("tp" "{")
        ("ctp" "{")
        ("comment" "{")
        ))
  
;; I don't know what it is 
 
(add-hook 'LaTeX-mode-hook
        (lambda ()
            (local-set-key (kbd "<S-f5>")
                (lambda () (interactive) (TeX-command-menu "LaTeX"))) 
            (local-set-key (kbd "C-M-[") 'LaTeX-find-matching-begin)
            (local-set-key (kbd "C-M-]") 'LaTeX-find-matching-end)
            ))
 
(defun fill-sentence ()
(interactive)
(save-excursion
    (or (eq (point) (point-max)) (forward-char))
    (forward-sentence -1)
    (let ((beg (point))
        (ix (string-match "LaTeX" mode-name)))
    (forward-sentence)
    (if (and ix (equal "LaTeX" (substring mode-name ix)))
        (LaTeX-fill-region-as-paragraph beg (point))
        (fill-region-as-paragraph beg (point))))))
(defvar bp/bibtex-fields-ignore-list
'("keywords" "abstract" "file" "issn" "eprint" "issue_date"
    "articleno" "numpages" "acmid"))
(defun bp/bibtex-clean-entry-hook ()
  (save-excursion
    (let (bounds)
      (when (looking-at bibtex-entry-maybe-empty-head)
        (goto-char (match-end 0))
        (while (setq bounds (bibtex-parse-field))
          (goto-char (bibtex-start-of-field bounds))
          (if (member (bibtex-name-in-field bounds)
                      bp/bibtex-fields-ignore-list)
              (kill-region (caar bounds) (nth 3 bounds))
            (goto-char (bibtex-end-of-field bounds))))))))
(add-hook 'bibtex-clean-entry-hook 'bp/bibtex-clean-entry-hook)
(global-set-key (kbd "<f7>") 'fill-sentence)
  
;; Bibtex mode
(require 'bibtex)
(setq bibtex-string-files '("strings.bib"))
(setq bibtex-string-file-path (concat (getenv "HOME") "/Dropbox/bibtex"))
(setq bibtex-field-delimiters 'double-quotes)
(setq bibtex-autokey-names-stretch 0);  Use up to 4 names in total
(setq bibtex-autokey-name-length nil);    Max no chars to use.
(setq bibtex-autokey-titlewords 0);     Don't use title in key.
(setq bibtex-autokey-titlewords-stretch 0);
(setq bibtex-text-indentation 0) ; No indentation for content.
(setq bibtex-entry-format
    `(page-dashes required-fields
        numerical-fields whitespace last-comma delimiters
        unify-case sort-fields))
(setq bibtex-entry-delimiters 'braces)


;; Nick Higham
(defun bibtex-generate-autokey ()
(let* ((bibtex-autokey-names nil)
        (bibtex-autokey-year-length 2)
        (bibtex-autokey-name-separator "\0")
        (names (split-string (bibtex-autokey-get-names) "\0"))
        (year (bibtex-autokey-get-year))
        (name-char
        (cond ((= (length names) 1)(min 4 (length (car names))))
        ;; Handle surnames with three or fewer characters.
                ((= (length names) 2) 2)
                (t 1)))
        (existing-keys (bibtex-parse-keys)) key)
    (setq names (mapconcat (lambda (x)(substring x 0 name-char))
                            names ""))
    (setq names (substring names 0 (min (length names) 4)))
    ;; Use only first four initials for papers with five or more authors.
    (setq key (format "%s%s" names year))
    (let ((ret key))
    (loop for c from ?a to ?z
            while (assoc ret existing-keys)
            do (setq ret (format "%s%c" key c)))
    ret)))

(defun my-cite()
    (interactive)
    (let ((reftex-cite-format "%a, %t, %j %v, %p, %e: %b, %u, %s, %y %<"))
                            (reftex-citation)))

(setq LaTeX-reftex-ref-style-auto-activate nil)
(setq LaTeX-reftex-cite-format-auto-activate nil)


;; I prefer closing brace on its own line after cleaning BibTeX entry.
(setq bibtex-clean-entry-hook 'mybibtex-clean-extra)
(defun mybibtex-clean-extra ()
"Move final right brace to a line of its own."
(progn (bibtex-end-of-entry) (left-char) (newline-and-indent)
        (insert "      ")))

;; These seem to work in LaTeX mode too, so no need to distinguish?
(defun my-tex-mode-hook ()
;; f5 saves file then runs LaTeX with no need to hit return.
;; http://stackoverflow.com/questions/1213260/one-key-emacs-latex-compilation
(local-set-key (kbd "<f5>") (kbd "C-x C-s C-c C-c C-j")))

(defun my-ref()
  (interactive)
  (insert "\\ref{}")
  (backward-char))

(defun my-eqref()
  (interactive)
  (insert "\\eqref{}")
  (backward-char))
(local-set-key (kbd "C-c r") 'my-ref)
(local-set-key (kbd "C-c e") 'my-eqref)
(local-set-key (kbd "C-c m") 'my-cite)
(add-hook 'TeX-mode-hook 'my-tex-mode-hook)
;; Add return after C-c C-j.
(add-hook 'LaTeX-mode-hook
        (lambda ()
            (local-set-key "\C-c\C-j"
                    (lambda () (interactive)
                    (LaTeX-insert-item) (TeX-newline)))))

;; Command to run BibTeX directly. 
(add-hook 'LaTeX-mode-hook
        (lambda ()
            (local-set-key (kbd "C-c C-g")
                (lambda () (interactive) (TeX-command-menu "BibTeX")))))

;; remove supersuper script
'(tex-font-script-display (quote (-0.0 0.0)))
'(tex-suscript-height-ratio 1.0)
(setq tex-fontify-script nil)
(fset 'tex-font-lock-suscript 'ignore)
(setq font-latex-fontify-script nil)

