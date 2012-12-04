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

(require 'cl)
(require 'easymenu)
(require 'help-mode)
(require 'pdt-doc)
(require 'pdt-help)

;;---------------------------------------------------------------------------
;; Global stuff


;;---------------------------------------------------------------------------
;; Customization

(defgroup pdt nil
  "PHP Development Tools"
  :group 'editing
  :prefix "pdt-")

(defcustom pdt-minor-mode-hook nil
  "Hook called when PDT minor mode is activated or deactivated."
  :type 'hook
  :group 'pdt
  :version "0.0.1")

;;;; Internal variables
(defvar pdt-minor-mode nil
  "Non-nil if using PDT mode as a minor mode of some other mode.
Use the command `pdt-minor-mode' to toggle or set this variable.")

;; (defvar pdt-minor-mode-menu nil
;;   "Holds the PDT menu.")

(defun pdt-init-minor-mode-keymap ()
  "Set up the `pdt-mode' keymap."
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c@\C-h"	'pdt-test)
    map))

(defvar pdt-minor-mode-map (pdt-init-minor-mode-keymap)
  "The keymap used when `pdt-mode' is active.")

(easy-menu-define pdt-minor-mode-menu pdt-minor-mode-map
  "Menu used when `pdt-mode' is active."
  '("PDT"
    ["Test" pdt-test
     :help "PDT test"]
	;; ["Search documentation" pdt-search-documentation
	;;  :help "Online PHP documentation"]
))

;;---------------------------------------------------------------------------
;; PDT commands
(defun pdt-test ()
  (message "test"))

;;---------------------------------------------------------------------------
;; PDT mode

(define-minor-mode pdt-minor-mode
  "PHP Development Tools minor mode"
  nil
  :group 'pdt
  :lighter " PDT"
  :keymap pdt-minor-mode-map
  (if pdt-minor-mode
      (progn
	(easy-menu-add pdt-minor-mode-menu))))

(provide 'pdt)

;;; pdt.el ends here
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
