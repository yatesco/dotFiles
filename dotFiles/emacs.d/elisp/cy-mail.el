;;; cy-mail.el --- Email (mu4e)

;; Copyright © 2011-2017 Colin Yates
;;
;; Author: Colin Yates
;; URL: https://colinyates.co.uk
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Email (mu4e)

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

;; mu4e isn't available on melpa so add it locally.
;; this assumes it was `brew install mu --with-emacs'
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e/")
(setq mu4e-enable-mode-line t
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/local/bin/msmtp"
      mu4e-enable-notifications t
      mu4e-alert-set-default-style 'notifier
      message-kill-buffer-on-exit t
      mu4e-mu-binary "/usr/local/bin/mu"
      mu4e-view-show-images t
      mu4e-get-mail-command "mbsync -a"
      mu4e-html2text-command "w3m -dump -T text/html"
      mu4e-update-interval 300
      mu4e-hide-index-messages t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-sendmail-f-is-evil 't)
(require 'mu4e)
;; VIM for the win
(use-package evil-mu4e
  :config
  ;; make 'b' in the headers view change bookmarks
  (evil-define-key evil-mu4e-state mu4e-headers-mode-map "b" #'mu4e-headers-search-bookmark))

;; configure the contexts
(setq mu4e-contexts
      `( ,(make-mu4e-context
	   :name "Personal"
	   :enter-func (lambda () (mu4e-message "Switch to the colinyates.co.uk context"))
	   ;; leave-func not defined
	   :match-func (lambda (msg)
			 (when msg
			   (mu4e-message-contact-field-matches msg
							       :to "colin@colinyates.co.uk")))
	   :vars '(  ( user-mail-address       . "colin@colinyates.co.uk"  )
		     ( user-full-name     . "Colin Yates" )))
	 ,(make-mu4e-context
	   :name "Work"
	   :enter-func (lambda () (mu4e-message "Switch to the Work context"))
	   ;; leave-fun not defined
	   :match-func (lambda (msg)
			 (when msg
			   (mu4e-message-contact-field-matches msg
							       :to "colin.yates@qficonsulting.com")))
	   :vars '(  ( user-mail-address . "colin.yates@qficonsulting.com")
		     ( user-full-name     . "Colin Yates" )))
	 ,(make-mu4e-context
	   :name "Gmail"
	   :enter-func (lambda () (mu4e-message "Switch to the gmail context"))
	   ;; leave-fun not defined
	   :match-func (lambda (msg)
			 (when msg
			   (mu4e-message-contact-field-matches msg
							       :to "colin.yates@gmail.com")))
	   :vars '(  ( user-mail-address . "colin.yates@gmail.com")
		     ( user-full-name     . "Colin Yates" )))))


;; only ask us for a context if we are composing a new message
(setq mu4e-context-policy nil
      mu4e-compose-context-policy 'ask-if-none)

;; let's have some sensible bookmarks
(setq mu4e-bookmarks
  `(("flag:unread AND NOT flag:trashed AND maildir:/work/*" "Unread work" ?u)
    ("flag:unread AND NOT flag:trashed AND NOT from:ucb AND NOT maildir:/work/*" "Unread non-work (ignoring lists)" ?m)
    ("flag:unread AND NOT flag:trashed" "All unread" ?a)))

;; Start mu4e in fullscreen
(defun cy/mu4e-start ()
  (interactive)
  (window-configuration-to-register :mu4e-fullscreen)
  (mu4e)
  (delete-other-windows))

;; Restore previous window configuration
(defun cy/mu4e-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :mu4e-fullscreen))

;; Naughty, but let's do the binding here rather than in *Keybindings
;; rather than split up this contiguous chunk of logic
(define-key mu4e-main-mode-map (kbd "q") 'cy/mu4e-quit-session)
(global-set-key (kbd "<f12>") 'cy/mu4e-start)

(provide 'cy-mail)

;;; cy-mail.el ends here
