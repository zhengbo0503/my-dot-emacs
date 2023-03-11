;; matlab mode settings
;; Version Date : 2023-03-06
;; Packages: Ambiguious, no idea how it load matlab.el

(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list
 'auto-mode-alist
 '("\\.m$" . matlab-mode))
(setq matlab-indent-function nil)

(setq matlab-shell-command "matlab")
(setq matlab-shell-command "/Applications/MATLAB/MATLAB_R2022b.app/bin/matlab")
(setq matlab-shell-command-switches (list "-nodesktop"))

(defun matlab-doc-ZZ (name)
  "open the matlab documentation for specific matlab function
if there is no entry entered, it will open the matlab help
center.
Version Date : 2023-03-05"
  (interactive "sWhat is your function name: ")
  (if (= (length name) 0)
      (browse-url (concat "https://mathworks.com/help/matlab/index.html"))
    (browse-url (concat "https://mathworks.com/help/matlab/ref/" name ".html")))
  )
;; https://mathworks.com/help/matlab/ref/eigs.html
