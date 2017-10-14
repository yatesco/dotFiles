;;; cy-keybindings.el --- Consistent keybindings

;; Copyright © 2011-2017 Colin Yates
;;
;; Author: Colin Yates
;; URL: https://colinyates.co.uk
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Consitent keybindings

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
;; SPC is the global leader
;; let's use the excellent 'general'
(use-package general
  :ensure t
  :config
  (general-evil-setup)) 

;; global (i.e. none mode specific)
(general-nvmap :prefix "SPC"
  "<SPC>" 'counsel-M-x
  ";" 'evilnc-comment-or-uncomment-lines
  "TAB" 'mode-line-other-buffer	; jump between buffers
  "/" 'counsel-projectile-ag
  "g" '(magit-status :which-key "Magit status")
  "t" '(treemacs-projectile-toggle :which-key "Treemacs")
  "s" 'swiper
  "u" '(undo-tree-visualize :which-key "Undo tree")
  "w" 'ace-window
  
  "a" '(:ignore t :which-key "Applications")
  "ad" 'deft
  "am" '(cy/mu4e-start :which-key "mu4e")
  
  "b" '(:ignore t :which-key "Buffers")
  "bb" '(ivy-switch-buffer :which-key "switch buffers")
  "bk" 'kill-this-buffer
  "bR" 'revert-buffer
  
  "c" 'org-capture
  
  "fed" 'cy/edit-defaults-el
  "fei" 'cy/edit-init-el
  "fek" 'cy/edit-keybindings-el
  "fem" 'cy/counsel-select-module

  ;; TODO - this needs to be put into a more specific "sp" mode
  ;; "H" '(:ignore t :which-key "Hydras") 
  ;; "Hl" '(hydra-lisp/body :which-key "Lisp")

  "o" 'counsel-imenu

  "p" (general-simulate-keys
       "C-c p" t
       "Projectile"
       general-SPC-simulates-C-c-p)

  ) 

;; -------------------------------------------------- 
;; emacs-lisp bindings
(general-define-key :prefix ","
		    :states '(normal motion visual) 
		    :keymaps 'emacs-lisp-mode-map
		    "eb" 'eval-buffer
		    "ed" 'eval-defun)

;; clojure? bindings
(general-define-key :prefix ","
		    :states '(normal motion visual) 
		    :keymaps 'clojure-mode-map

		    "eb" 'cider-eval-buffer
		    "ed" 'cider-eval-last-sexp
		    )

;; ;; a hydra for navigating lisp
;; (defhydra hydra-lisp (:color pink
;; 			     :hint nil)
;;   "
;; ^Move^         ^Barf/Slurp^      
;; ^^^^^^^^----------------------------
;; _h_: left      _(<_ slurp left
;; _j_: down      _(>_: barf left
;; _k_: up        _>)_: slurp forward 
;; _l_: right     _<)_: barf forward"
;;   ("h" sp-up-sexp)
;;   ("j" sp-previous-sexp)
;;   ("k" sp-next-sexp)
;;   ("l" sp-down-sexp)
;;   ("(<" sp-backward-slurp-sexp)
;;   ("(>" sp-backward-barf-sexp)
;;   (">)" sp-forward-slurp-sexp)
;;   ("<)" sp-forward-barf-sexp)

;;   ("q" quit-window "quit" :color blue))

(provide 'cy-keybindings)

;;; cy-keybindings.el ends here
