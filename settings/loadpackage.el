;;;;---------------------package control

(require 'package)
(setq package-archives
      '(
        ("gnu-elpa" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

;; Install automatically if not already present.
(add-to-list 'load-path "~/Dropbox/elisp/pkgs/use-package")
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)
(eval-when-compile
  (setq use-package-enable-imenu-support t)
  (require 'use-package))

;; diminish //// this is important, hence we load earilier
(add-to-list 'load-path "~/Dropbox/elisp/pkgs/diminish")
(require 'diminish)
