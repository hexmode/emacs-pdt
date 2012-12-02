;;; pdt.el --- PHP Development Tools for GNU Emacs
;;
;; Copyright (C) 2012 neutrico.pl
;;
;; Name: Emacs PDT
;; Author: Marcin Antczak <marcin.antczak@neutrico.pl>
;; Version: 0.0.1
;; Keywords: php
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
;;
;; This package provides tolls for PHP development.
;;
;;; Code:

(defconst pdt-mode-version-number "0.0.1"
  "PDT version number.")

(eval-when-compile
  (require 'cl))

(require 'easymenu)
(require 'help-mode)
(require 'pdt-help)

;;;; Global stuff


;;;; Customization

(defgroup pdt nil
  "pdt"
  :group 'editing
  :prefix "pdt-")

(defcustom pdt-modes
  '(php-mode)
  "Major modes `pdt-mode' can run on."
  :type '(repeat symbol)
  :group 'pdt)

(defcustom pdt-use-menu 'abbreviate
  "Display a PDT menu in the menu bar."
  :type '(choice (const :tag "Full"  full)
                 (const :tag "Abbreviate" abbreviate)
                 (const :tag "No menu" nil))
  :group 'pdt)

(defcustom pdt-mode-hook nil
  "List of functions to be executed on entry to `pdt-mode'."
  :type 'hook
  :group 'pdt)

;;;; Internal variables
(defvar pdt--minor-mode-menu nil
  "Holds the PDT menu.")

;;;; PDT commands

;;;; PDT mode



(defun pdt-setup()
  (run-hooks 'pdt-mode-hook)
)

(defun pdt-test ()
  (message "test"))

(defun pdt--init-minor-keymap ()
  "Set up the `pdt-mode' keymap."
  (let ((map (make-sparse-keymap)))
    (when pdt-use-menu
      (easy-menu-define pdt--minor-mode-menu
	map
	"Menu used when `pdt-mode' is active."
	'("PDT"
	  "----"
	  "test"
	  ["Test" pdt-test
	    :help "PDT test"]
)))))

(defvar pdt-minor-mode-map (pdt--init-minor-keymap)
  "The keymap used when `pdt-mode' is active.")

(define-minor-mode pdt-minor-mode
  "PHP Development Tools minor mode"
  nil
  :lighter " PDT"
  :keymap pdt-minor-mode-map
  :group 'pdt
  (if pdt-minor-mode
      (progn
	(pdt-setup))))

(provide 'pdt)

;;; pdt.el ends here
