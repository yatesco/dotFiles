;;; cy-clojure.el --- Clojure configuration

;; Copyright © 2011-2017 Colin Yates
;;
;; Author: Colin Yates
;; URL: https://colinyates.co.uk
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Clojure configuration

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

(use-package clojure-mode
  :defer t
  :config
  (use-package clojure-mode-extra-font-locking :ensure t)
  ;; Temporarily commented out due to https://github.com/yatesco/dotFiles/issues/26
  ;; (use-package clj-refactor)
  ;; flycheck-joker to the (quality assurance) rescue
  (use-package flycheck-joker)
  (use-package clojure-snippets))

(with-eval-after-load 'clojure
  ;; as recommended https://github.com/clojure-emacs/clj-refactor.el
  (add-hook 'clojure-mode-hook (lambda ()
				 (clj-refactor-mode 1)
				 (yas-minor-mode 1)
				 (cljr-add-keybindings-with-prefix "C-c C-m"))))

(provide 'cy-clojure)

;;; cy-clojure.el ends here
