;;; status-bar.el --- setting up the status bar
;;
;; Copyright (C) 2012 neutrico.pl
;;
;; Name: Emacs PDT
;; Author: Marcin Antczak <marcin.antczak@neutrico.pl>
;; Version: 0.0.1
;; Keywords: mouse frames
;; Package: emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;;; Commentary:

;;; Code:

(require 'cl)

;;---------------------------------------------------------------------------
;; Global stuff


;;---------------------------------------------------------------------------
;; Customization

(defgroup status-bar nil
  "Status Bar mode"
  :group 'editing
  :prefix "pdt-")

(defcustom status-bar--mode-hook nil
  "Hook called when Status Bar minor mode is activated or deactivated."
  :type 'hook
  :group 'status-bar
  :version "0.0.1")

;;---------------------------------------------------------------------------
;; Status Bar mode
(define-minor-mode status-bar-mode
  "Toggle the status bar in all graphical frames (Status Bar mode)."
  :group 'status-bar
  :global t
  :init-value t
  :lighter "Status-Bar"
  (let ((val (if status-bar-mode 1 0)))
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'status-bar-lines val))
    ;; If the user has given `default-frame-alist' a `status-bar-lines'
    ;; parameter, replace it.
    (if (assq 'status-bar-lines default-frame-alist)
	(setq default-frame-alist
	      (cons (cons 'status-bar-lines val)
		    (assq-delete-all 'status-bar-lines
				     default-frame-alist)))))
  (and status-bar-mode
       (= 1 (length (default-value 'status-bar-map))) ; not yet setup
       (status-bar-setup)))

(defvar status-bar-map (make-sparse-keymap)
  "Keymap for the status bar.
Define this locally to override the global status bar.")


(defun powerbar-setup ()
  (message "status-bar")
)

(provide 'status-bar)

;;; status-bar.el ends here
