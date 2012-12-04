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

(defun pdt-search-local-documentation ()
  "Search the local PHP documentation (i.e. in `pdt-manual-path')
for the word at point.  The function returns t if the requested
documentation exists, and nil otherwise."
  (interactive)
  (flet ((pdt-function-file-for (name)
                                (expand-file-name
                                 (format "function.%s.html"
                                         (replace-regexp-in-string "_" "-" name))
                                 pdt-manual-path)))
    (let ((doc-file (pdt-function-file-for (current-word))))
      (and (file-exists-p doc-file)
           (browse-url doc-file)
           t))))

;; Define function documentation function
(defun pdt-search-documentation ()
  "Search PHP documentation for the word at point.  If
`pdt-manual-path' has a non-empty string value then the command
will first try searching the local documentation.  If the
requested documentation does not exist it will fallback to
searching the PHP website."
  (interactive)
  (flet ((pdt-search-web-documentation ()
                                       (browse-url (concat pdt-search-url (current-word)))))
    (if (and (stringp pdt-manual-path)
             (not (string= pdt-manual-path "")))
        (or (pdt-search-local-documentation)
            (pdt-search-web-documentation))
      (pdt-search-web-documentation))))

;; Define function for browsing manual
(defun pdt-browse-manual ()
  "Bring up manual for PHP."
  (interactive)
  (browse-url pdt-manual-url))

;; Define shortcut
(define-key pdt-mode-map
  "\C-c\C-f"
  'pdt-search-documentation)

(provide 'pdt-help)

;;; pdt-help.el ends here
