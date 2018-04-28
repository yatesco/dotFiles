;;; cy-leuven-theme.el --- Configure the user interface

;; Copyright © 2011-2017 Colin Yates
;;
;; Author: Colin Yates
;; URL: https://colinyates.co.uk
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Glitz and sparkle for the UI!

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

;; -------------------------------
;; theming
(use-package powerline)
;; make sure that zenburn theme is loaded before the airline theme
;; otherwise the ivy input field is unreadable
;; (use-package zenburn-theme
;;   :config
;;   (load-theme 'zenburn t))
;;(add-to-list 'default-frame-alist '(background-color . "gray00"))
(load-theme 'leuven t)

(use-package airline-themes
  :config
  (load-theme 'airline-cool 1))

(provide 'cy-leuven-theme)

;;; cy-leuven-theme.el ends here

