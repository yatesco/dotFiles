;;; cy-ivy.el --- Use the excellent ivy (and associated tools)

;; Copyright © 2011-2017 Colin Yates
;;
;; Author: Colin Yates
;; URL: https://colinyates.co.uk
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Use the excellent ivy (and associated tools)

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

(use-package ivy
  :defer t
  :diminish ivy-mode
  :bind (:map ivy-minibuffer-map
	      ("C-k" . ivy-previous-line)
	      ("C-j" . ivy-next-line))
  :init
  (setq
   ;; include recentf and bookmarks
   ivy-use-virtual-buffers t
   ;; recursion is always useful
   enable-recursive-minibuffers t)
  :config
  (ivy-mode 1)
  (setq
   ;; don't start counsel-M-x with a leading `^'
   ivy-initial-inputs-alist nil)
  ;; counsel-ivy doesn't sort M-x by most frequently used, let's pull in
  ;; smex to do that bring in the swoop
  (use-package smex)
  (use-package swiper)

  ;; allow nuclear editing as well!
  ;; TIP: `C-c C-o' in search result then `C-c C-q'
  (use-package wgrep :ensure t)
  )

(provide 'cy-ivy)

;;; cy-ivy.el ends here
