;;; cy-defaults.el --- Sensible defaults

;; Copyright © 2011-2017 Colin Yates
;;
;; Author: Colin Yates
;; URL: https://colinyates.co.uk

;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Sensible defaults

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; stop polluting my .init.el
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p "~/.emacs.d/custom.el")
  (with-temp-buffer (write-file "~/.emacs.d/custom.el")))
(load custom-file)

;; 'y' or 'n' instead of yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; handle backups sensibly
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t)

;; silence!
(setq visible-bell 1)

;; load some useful tools
(use-package which-key
  :diminish which-key-mode
  :config
    (setq which-key-side-window-max-width 0.5
	which-key-popup-type 'side-window
	which-key-side-window-location 'right
	which-key-idle-delay 0.4)
  (which-key-mode))

;; almost everything wants smartparens
(use-package smartparens
  :diminish smartparens-mode
  :defer t
  :config
  (require 'smartparens-config)
  (smartparens-global-strict-mode 1)
  (use-package evil-smartparens
    :diminish evil-smartparens-mode
    :config
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
  
  ;; also, make delimiters look purty
  ;; to highlight the block between the parameters set the following
  ;; (setq show-paren-style 'expression)
  (show-paren-mode))
;; Always start smartparens mode in prog-mode. This will cause
;; use-package to kick in and apply the :config
(add-hook 'prog-mode-hook #'smartparens-mode)

;; configure expand-region - really useful for evaluating around the
;; point
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "§") 'er/expand-region))

;; focus on Help windows automatically
(setq help-window-select t)

;; get rid of the hideous splash screen
(setq inhibit-startup-screen t)

(provide 'cy-defaults)

;;; cy-defaults.el ends here
