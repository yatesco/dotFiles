;;; cy-ui.el --- Configure the user interface

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

;; get rid of the decoration
(menu-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(tool-bar-mode -1)

;; hide some uninteresting modes in the modeline that aren't loaded by (use-package)
(diminish 'undo-tree-mode)

;; this could validly live here or in in cy-evil.
(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer
      :ensure t
      :config
      (setq evil-motion-state-cursor 'box)  ; █
      (setq evil-visual-state-cursor 'box)  ; █
      (setq evil-normal-state-cursor 'box)  ; █
      (setq evil-insert-state-cursor 'bar)  ; ⎸
      (setq evil-emacs-state-cursor  'hbar) ; _
      (etcc-on)))

(provide 'cy-ui)

;;; cy-ui.el ends here

