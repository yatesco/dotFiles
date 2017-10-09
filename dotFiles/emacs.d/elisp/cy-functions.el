;;; cy-functions.el --- Utility functions

;; Copyright © 2011-2017 Colin Yates
;;
;; Author: Colin Yates
;; URL: https://colinyates.co.uk
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utility functions

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

;; --------------------------------------------------
;; Utilities for accessing the various .el files NOTE: a 'module' is
;; an abstraction which follows the convention of
;; one-module-per-file. The name of the module-name is essentially the
;; filename minus the 'cy-' and '.el' bits.

;; WARNING - HERE BE DRAGONS!

;; -- private
(defun cy/expand-to-module-file (module-name)
  (expand-file-name (concat "cy-" module-name ".el") "~/.emacs.d/elisp"))

(defun cy/edit-module (module-name)
  (message (concat "editing " module-name))
  (find-file (cy/expand-to-module-file module-name)))

(defun cy/module-exists-p (module-name)
  (file-exists-p (cy/expand-to-module-file module-name)))

(defun cy/create-module (module-name)
  (interactive)
  (message (concat "Creating " module-name))
  (let ((file (cy/expand-to-module-file module-name)))
    (write-region "" nil file)
    (find-file file)
    (evil-insert-state)
    (yas-expand-snippet (yas-lookup-snippet "header" nil 'lisp-mode))))

(defun cy/edit-or-create-module (module-name)
  (if (cy/module-exists-p module-name)
      (cy/edit-module module-name)
    (cy/create-module module-name)))

(defun cy/extract-module-name (filename)
  "Extract the module, assuming 'cy-<module>.el'"
  (substring filename 3 (- (length filename) 3)))

(defun cy/list-modules ()
  (let ((files (directory-files (expand-file-name "~/.emacs.d/elisp") nil "^cy-.*el$")))
    (mapcar 'cy/extract-module-name files)))

;; -- public
(defun cy/edit-defaults-el ()
  (interactive)
  (cy/edit-module "defaults"))

(defun cy/edit-init-el ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))

(defun cy/edit-keybindings-el ()
  (interactive)
  (cy/edit-module "keybindings"))

(with-eval-after-load "counsel"
    (defun cy/counsel-select-module ()
      "Forward to `describe-function'."
      (interactive)
      (ivy-read "Select module "
		(cy/list-modules)
		:preselect (counsel-symbol-at-point)
		:require-match nil
		:sort t
		:action 'cy/edit-or-create-module
		;; (lambda (x)
		;; 	      (find-file (expand-file-name x "~/.emacs.d/elisp")))
		:caller 'cy/counsel-select-module))
)

(provide 'cy-functions)

;;; cy-functions.el ends here
