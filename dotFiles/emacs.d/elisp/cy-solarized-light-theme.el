;;; cy-solarized-light-theme.el --- Configuration for the solarized light theme

;; Copyright © 2011-2017 Colin Yates
;;
;; Author: Colin Yates
;; URL: https://colinyates.co.uk
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration for the solarized light theme

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

;; So, er, there isn't actually any code here. It turns out emacs
;; _sucks_ at handling terminal colours properly so for the best
;; experience run Solarized Light in your (iterm) terminal and have
;; _no_ emacs theme.

;; (use-package color-theme-solarized
;;   :ensure t
;;   :config
;;   (setq solarized-termcolors 256)
;;   (add-hook 'after-make-frame-functions
;; 	    (lambda (frame)
;; 	      (let ((mode (if (display-graphic-p frame) 'light 'light)))
;; 		(set-frame-parameter frame 'background-mode mode)
;; 		(set-terminal-parameter frame 'background-mode mode))
;; 	      (load-theme 'solarized t))))

(provide 'cy-solarized-light-theme)

;;; cy-solarized-light-theme.el ends here
