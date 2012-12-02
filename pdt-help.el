;;; pdt-help.el --- PHP Development Tools for GNU Emacs
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

;; Use popup to show tooltips and context help
;; https://github.com/auto-complete/popup-el
(require 'popup)

(defcustom pdt-php-manual-url "http://www.php.net/manual/en/"
  "URL at which to find PHP manual.
You can replace \"en\" with your ISO language code."
  :type 'string
  :group 'pdt)

(defcustom pdt-manual-search-url "http://www.php.net/"
  "URL at which to search for documentation on a word."
  :type 'string
  :group 'pdt)

(defcustom pdt-php-manual-path ""
  "Path to the directory which contains the PHP manual."
  :type 'string
  :group 'pdt)

(provide 'pdt-help)

;;; pdt-help.el ends here
