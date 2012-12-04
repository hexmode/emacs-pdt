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

(defgroup pdt-doc nil
  "Customizations for PDT doc."
  :group 'pdt
  :prefix "pdt-doc-")

(defcustom pdt-doc-default-author (cons "" "")
  "Default author to insert in doc blocks."
  :group 'pdt-doc
  :type '(cons (string :tag "Name") (string :tag "Email Address")))

(defcustom pdt-doc-default-copyright nil
  "Default copyright to insert in doc blocks."
  :group 'pdt-doc
  :type '(string :tag "Copyright"))

(defcustom pdt-doc-default-license nil
  "Default license to insert in doc blocks."
  :group 'pdt-doc
  :type '(string :tag "License"))

(defcustom pdt-doc-default-version nil
  "Default version string to insert in doc blocks."
  :group 'pdt-doc
  :type '(string :tag "Version"))

(defcustom pdt-doc-default-php-version nil
  "Default PHP version to insert in doc blocks."
  :group 'pdt-doc
  :type '(string :tag "PHP Version"))

(defcustom pdt-doc-default-link nil
  "Default link required by PEAR standard."
  :group 'pdt-doc
  :type '(string :tag "Link"))

(defcustom pdt-doc-default-category ""
  "Default category required by the PEAR standard.  May be a
function name."
  :group 'pdt-doc
  :type '(string :tag "Category"))

(defcustom pdt-doc-default-package ""
  "Default package required by the PEAR standard.  May be a
function name."
  :group 'pdt-doc
  :type '(string :tag "Package"))

(defcustom pdt-doc-default-subpackage ""
  "Default subpackage required by the PEAR standard.  May be a
function name."
  :group 'pdt-doc
  :type '(string :tag "Subpackage"))

(provide 'pdt-doc)
