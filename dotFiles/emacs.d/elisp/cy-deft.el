;;; cy-deft.el --- Note taking for the masses

;; Copyright © 2011-2017 Colin Yates
;;
;; Author: Colin Yates
;; URL: https://colinyates.co.uk
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Note taking for the masses

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
(use-package deft
  :diminish deft-mode
  :commands (deft)
  :config
  (setq
   ;; for now, let's just use org
   deft-extensions '("org")
   deft-default-extension "org"
   deft-directory "~/deft"
   ;; search folders recursively
   deft-recursive t
   ;; use the search text as the filename for new files
   deft-use-filter-string-for-filename t
   ;; add an explicit #+TITLE to org docs
   deft-org-mode-title-prefix t)

  (with-eval-after-load 'evil
    (evil-set-initial-state 'deft-mode 'insert)
    (evil-define-key 'normal deft-mode-map (kbd "q") 'cy/deft-quit-session)))

;; Start deft in fullscreen
(defun cy/deft-start ()
  (interactive)
  (window-configuration-to-register :deft-fullscreen)
  (deft)
  (delete-other-windows))

;; Restore previous window configuration
(defun cy/deft-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :deft-fullscreen))


(provide 'cy-deft)

;;; cy-deft.el ends here
