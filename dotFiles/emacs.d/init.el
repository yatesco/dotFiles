;; set up the various repositories
(setq package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))
(package-initialize)
;; use use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;; provide access to our modules
(add-to-list 'load-path "~/.emacs.d/elisp")

;; setup some sensible defaults
(require 'cy-defaults)
(require 'cy-functions)
;; now the various packages
(require 'cy-clojure)
(require 'cy-yaml)
(require 'cy-company)
(require 'cy-evil)
(require 'cy-fish)
(require 'cy-ivy)
(require 'cy-mail)
(require 'cy-projectile)
(require 'cy-ui)
(require 'cy-solarized-light-theme)
;; (require 'cy-solarized-dark-theme)
;; (require 'cy-zenburn-theme)
(require 'cy-version-control)
(require 'cy-yasnippets)

;; finally, install the keybindings
(require 'cy-keybindings)

;; always recompile for extra speed next time ;-)
(defun cy/recompile-elc ()
  (interactive)
  ;; byte-recompile-directory doesn't support exclusions
  ;; TODO - how to get a list and then loop through that?
  (byte-recompile-directory (expand-file-name "~/.emacs.d/elisp") 0)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/my-snippets") 0)
  (byte-recompile-file (expand-file-name "~/.emacs.d/init.el") 0))
;; disabling due to https://github.com/yatesco/dotEmacs/issues/1
;; (cy/recompile-elc)

;; let everyone know we are done
(message (concat "Emacs loaded in " (emacs-init-time) ", go wild!"))
(message "Go wild!")
