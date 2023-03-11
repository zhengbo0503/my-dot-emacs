;; bind key
(require 'bind-key)

;; compat
(add-to-list 'load-path "~/Dropbox/elisp/pkgs/compat")
(require 'compat)

;; consult
(add-to-list 'load-path "~/Dropbox/elisp/pkgs/consult")
(require 'consult)

;; which-key
(add-to-list 'load-path "~/Dropbox/elisp/pkgs/emacs-which-key")
(require 'which-key)
(setq guide-key/highlight-command-regexp "rectangle")
(which-key-mode)
(which-key-setup-minibuffer)
(which-key-setup-side-window-bottom)

;; hydra
(add-to-list 'load-path "~/Dropbox/elisp/pkgs/hydra")
(require 'hydra)
(global-set-key (kbd "<f3>") 'hydra-bib-etc/body)
(defhydra hydra-bib-etc (:font Consolas-18 :color blue :columns 3)
  "Main Menu"
  ("<f3>" helm-bibtex "helm-bibtex")
  ("t" (dired "/Users/clement/Dropbox/tex") "tex")
  ("r" (dired "/Users/clement/Dropbox/reading_notes") "reading-notes")
  ("m"   (dired "~/Dropbox/matlab") "matlab")
  ("i" get-bibtex-from-doi "get-bibtex-from-doi"))


(require 'vertico)
(vertico-mode +1)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(require 'prescient)
(prescient-persist-mode +1)
(require 'vertico-prescient)
(vertico-prescient-mode +1)

;; consult-flyspell
(require 'consult-flyspell)
(setq ispell-dictionary "american")

;; The electric package
(require 'electric)
(electric-pair-mode 1)

;; yasnippet
(add-to-list 'load-path "~/Dropbox/elisp/pkgs/yasnippet")
(require 'yasnippet)
(add-to-list 'load-path "~/Dropbox/elisp/pkgs/yasnippet-snippets")
(require 'yasnippet-snippets)
(yas-global-mode 1)

(require 'marginalia)
(setq marginalia-annotators
        '(marginalia-annotators-heavy
          marginalia-annotators-light))
(marginalia-mode 1)

;; ripgrep
(add-to-list 'load-path "~/Dropbox/elisp/pkgs/ripgrep")
(require 'ripgrep)

;; swiper-like isearch
(add-to-list 'load-path "~/Dropbox/elisp/pkgs/swiper")
(require 'swiper)
