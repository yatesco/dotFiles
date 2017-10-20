;;; cy-evil.el --- VIM to the rescue

;; Copyright © 2011-2017 Colin Yates
;;
;; Author: Colin Yates
;; URL: https://colinyates.co.uk
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Modal editing rocks.

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

;; VIM for the win
(use-package evil
  :init
  ;; preserve tab in the terminal
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1))

;; add in some useful evil plugins
(use-package evil-nerd-commenter)
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; let special modes (e.g. the Warnings buffer) start in emacs
(with-eval-after-load 'evil
  (evil-set-initial-state 'special-mode 'insert))

(provide 'cy-evil)

;;; cy-evil.el ends here
