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

(eval-when-compile
  (require 'cl))

(require 'pdt-help)
(require 'easymenu)

;;;; Global stuff


;;;; Customization

(defcustom pdt-modes
  '(php-mode)
  "Major modes `pdt-mode' can run on."
  :type '(repeat symbol)
  :group 'pdt)

;;;; Internal variables

;;;; PDT commands

;;;; PDT mode



(defun pdt-setup()

)

(define-minor-mode pdt-mode
  "PHP Development Tools minor mode"
  nil
  :lighter " PDT"
  :keymap pdt-minor-mode-map
  :group 'pdt
  (if pdt-mode
      (progn
	(pdt-setup))))

(provide 'pdt)

;;; pdt.el ends here
