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

(require 'ede)


;;; Code:
(defvar ede-wp-theme-project-list nil
  "List of projects created by option `ede-wp-theme-project'.")

(defun ede-wp-theme-file-existing (dir)
  "Find an WordPress Theme declaration project in the list of WP projects.
DIR is the directory to search from."
  (let ((projs ede-android-project-list)
	(ans nil))
    (while (and projs (not ans))
      (let ((root (ede-project-root-directory (car projs))))
	(when (string-match (concat "^" (regexp-quote root)) dir)
	  (setq ans (car projs))))
      (setq projs (cdr projs)))
    ans))

;;;###autoload
(defun ede-wp-theme-load (dir &optional rootproj)
  "Return an WordPress Theme Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  (or (ede-android-file-existing dir)
      ;; Doesn't already exist, so lets make one.
      (let* ((ad (ede-android-project-data dir))
	     (proj (ede-android-project
		    (car ad)
		    :name (car ad)
		    :version (car (cdr ad))
		    :directory (file-name-as-directory dir)
		    :file (expand-file-name "AndroidManifest.xml"
					    dir)
		    :package (car (cdr (cdr ad))))))
	(ede-add-project-to-global-list proj)
	)))

;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "wp-theme"
		       :name "Wordpress Theme Project"
		       :file 'pdt-wp-theme-project
		       :proj-file "style.css"
		       :load-type 'ede-wp-theme-load
		       :class-sym 'ede-wp-theme-project
		       :new-p t
		       :safe-p t))

;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "wp-plugin"
		       :name "Wordpress Plugin Project"
		       :file 'pdt-wp-plugin-project
		       :proj-file "style.css"
		       :load-type 'ede-wp-plugin-load
		       :class-sym 'ede-wp-plugin-project
		       :new-p t
		       :safe-p t))

;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "drupal"
		       :name "Drupal Project"
		       :file 'pdt-drupal-project
		       :proj-file "style.css"
		       :load-type 'ede-drupal-load
		       :class-sym 'ede-drupal-project
		       :new-p t
		       :safe-p t))

;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "php"
		       :name "PHP Project"
		       :file 'pdt-php-project
		       :proj-file "style.css"
		       :load-type 'ede-php-load
		       :class-sym 'ede-php-project
		       :new-p t
		       :safe-p t))

;;; CLASSES
;;
;; These are the classes for the EDE project system for tracking an android project.
(defclass ede-wp-theme-target-misc (ede-target)
  ()
  "EDE WP Theme Project target for Misc files.
All directories with filesshould have at least one target.")

;;;###autoload
(defclass ede-wp-theme-project (ede-project eieio-instance-tracker)
  ((tracking-symbol :initform 'ede-wp-theme-project-list)
   (keybindings :initform (("S" . ede-wp-theme-visit-strings)))
   (menu :initform
	 (
	  [ "Visit style.css" ede-wp-theme-visit-strings ]
	  [ "Edit Projectfile" ede-edit-file-target
	    (ede-buffer-belongs-to-project-p) ]
	  "--"
	  [ "Update Version" ede-update-version ede-object ]
	  [ "Version Control Status" ede-vc-project-directory ede-object ]
	  "--"
	  [ "Rescan Project Files" ede-rescan-toplevel t ]
	  ))
   (package :initarg :package
	    :initform "com"
	    :type string
	    :documentation "The package extracted from the Manifest.")
   )
  "Project for Wp-Theme applications.")

(defmethod initialize-instance ((this ede-wp-theme-project)
				&rest fields)
  "Make sure the targets slot is bound."
  (call-next-method)
  (unless (slot-boundp this 'targets)
    ;; @TODO - All wp-theme projects are the same, so we can probably
    ;; prepopulate this whole thing right off.
    (oset this :targets nil))
  ;; In case the defaults change, force the known configurations
  ;; of wp-theme to be setup here.
  (oset this configurations '("debug" "install" "release"))
  (oset this configuration-default "debug")
  ;; If our manifest file doesn't exist, then the user has called
  ;; ede-new, and we need to call wp-theme to fill in our template directory.
  (when (not (file-exists-p (oref this :file)))
    (if (y-or-n-p (format "No style.css file exists in %s.  Create?"
			  (file-name-directory (oref this :file))))
	(call-interactively 'cedet-wp-theme-create-project)
      ;; Else, possible problems
      (message "You may run into problems in this project if there is no manifest file.")))
  )

(defun ede-wp-theme-resource-file (where what)
  "Find the file associated with the resource in WHERE that is WHAT.
WHERE is something like menu or layout, and what is the name of the resource."
  (let* ((root (ede-project-root-directory ede-object-project))
	 (fdir (expand-file-name where (expand-file-name "res" root))))
    (expand-file-name (concat what ".css") fdir)))

(defun ede-wp-theme-visit-strings ()
  "Visit the strings resource for the current project."
  (interactive)
  (find-file (ede-wp-theme-resource-file "values" "style")))




(defun pdt-wp-theme-project (type &optional name)
  "Create a new project starting from project type TYPE.
Optional argument NAME is the name to give this project."
  (interactive
   (list "Wordpress Theme"
	;; (completing-read "Project Type: "
	;; 		  (object-assoc-list
	;; 		   'name
	;; 		   (let* ((l ede-project-class-files)
	;; 			  (cp (ede-current-project))
	;; 			  (cs (when cp (object-class cp)))
	;; 			  (r nil))
	;; 		     (while l
	;; 		       (if cs
	;; 			   (if (eq (oref (car l) :class-sym)
	;; 				   cs)
	;; 			       (setq r (cons (car l) r)))
	;; 			 (if (oref (car l) new-p)
	;; 			     (setq r (cons (car l) r))))
	;; 		       (setq l (cdr l)))
	;; 		     (when (not r)
	;; 		       (if cs
	;; 			   (error "No valid interactive sub project types for %s"
	;; 				  cs)
	;; 			 (error "EDE error: Can't fin project types to create")))
	;; 		     r)
	;; 		   )
	;; 		  nil t)
))
  (require 'ede/custom)
  ;; Make sure we have a valid directory
  (when (not (file-exists-p default-directory))
    (error "Cannot create project in non-existent directory %s" default-directory))
  (when (not (file-writable-p default-directory))
    (error "No write permissions for %s" default-directory))
  (unless (ede-check-project-directory default-directory)
    (error "%s is not an allowed project directory in `ede-project-directories'"
	   default-directory))
  ;; Make sure the project directory is loadable in the future.
  (ede-check-project-directory default-directory)
  ;; Create the project
  (let* ((obj (object-assoc type 'name ede-project-class-files))
	 (nobj (let ((f (oref obj file))
		     (pf (oref obj proj-file)))
		 ;; We are about to make something new, changing the
		 ;; state of existing directories.
		 (ede-project-directory-remove-hash default-directory)
		 ;; Make sure this class gets loaded!
		 (require f)
		 (make-instance (oref obj class-sym)
				:name (or name (read-string "Name: "))
				:directory default-directory
				:file (cond ((stringp pf)
					     (expand-file-name pf))
					    ((fboundp pf)
					     (funcall pf))
					    (t
					     (error
					      "Unknown file name specifier %S"
					      pf)))
				:targets nil)))
	 (inits (oref obj initializers)))
    ;; Force the name to match for new objects.
    (object-set-name-string nobj (oref nobj :name))
    ;; Handle init args.
    (while inits
      (eieio-oset nobj (car inits) (car (cdr inits)))
      (setq inits (cdr (cdr inits))))
    (let ((pp (ede-parent-project)))
      (when pp
	(ede-add-subproject pp nobj)
	(ede-commit-project pp)))
    (ede-commit-project nobj))
  ;; Once the project is created, load it again.  This used to happen
  ;; lazily, but with project loading occurring less often and with
  ;; security in mind, this is now the safe time to reload.
  (ede-load-project-file default-directory)
  ;; Have the menu appear
  (setq ede-minor-mode t)
  ;; Allert the user
  (message "Project created and saved.  You may now create targets."))


;; Note: Learned how to add custom menus

(defvar menu-bar-pdt-project-menu (make-sparse-keymap "New"))

(define-key menu-bar-pdt-project-menu [pdt-project]
  '(menu-item "Project..." ede-new
             :help "New Project"))

(define-key menu-bar-pdt-project-menu [separator-pdt-1]
  '(menu-item "--"))

(define-key menu-bar-pdt-project-menu [pdt-drupal-project]
  '(menu-item "Drupal Project" pdt-drupal-project
             :help "New Drupal Project"))

(define-key menu-bar-pdt-project-menu [pdt-wp-plugin-project]
  '(menu-item "WordPress Plugin Project" pdt-wp-plugin-project
             :help "New WordPress Plugin Project"))

(define-key menu-bar-pdt-project-menu [pdt-wp-theme-project]
  '(menu-item "WordPress Theme Project" pdt-wp-theme-project
             :help "New WordPress Theme Project"))

(define-key menu-bar-pdt-project-menu [pdt-php-project]
  '(menu-item "PHP Project" pdt-php-project
             :help "New PHP Project"))

(define-key menu-bar-file-menu [pdt]
       (list 'menu-item "New" menu-bar-pdt-project-menu))

;;----------------------------------------------------------------------------

;; @TODO learn how to add positions to existing submenu, but create top menu
;; if any

;; (defvar menu-bar-js-project-menu (make-sparse-keymap "New"))

;; (define-key menu-bar-js-project-menu [separator-js-1]
;;   '(menu-item "--"))

;; (define-key menu-bar-js-project-menu [js-project]
;;   '(menu-item "Project..." js-project
;;              :help "New Project"))

;; (define-key menu-bar-js-project-menu [js-php-project]
;;   '(menu-item "JavaScript Project" js-php-project
;;              :help "New JavaScript Project"))

;; (define-key menu-bar-file-menu [js]
;;        (list 'menu-item "New" menu-bar-js-project-menu))


;; @TODO - use eclim to integrate with eclipse if available or to import projects
;; to emacs workspace

;; @TODO - create phpmode description file in eclipse compatible syntax

;; .project file

;; .buildpath file
;; use this file to generate tags file with functions from external libraries like WP, Drupal etc.


(provide 'pdt-project)

;;; pdt-project.el ends here
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
