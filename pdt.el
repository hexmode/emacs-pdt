;;; pdt.el --- A better PHP mode with Zend Framework 1 support.

;; Version: 0.0.1
;; Created: 2013.08.17
;; Copyright © 2013 Marcin Antczak
;; Author: Marcin Antczak <marcin.antczak@neutrico.pl>
;;
;; *************************************************************************

;;; *****
;;; About
;;; *****

;; pdt is an emacs mode that makes PHP programming easier and faster.
;; Specific support for Zend Framework is also included.

;;; *****
;;; Usage
;;; *****

;; Installation:
;; Place the following in your .emacs setup:
;; (add-to-list 'load-path "/path/to/pdt/")
;; (require 'pdt)
;; (pdt-setup)

;; Keybindings:
;; The PDT menu can be used to call most functions.
;; Use C-h m from a pdt buffer to view the keybindings.

;; *************************************************************************

;;; ************
;;; REQUIREMENTS
;;; ************
(require 'cl)
(require 'font-lock)
(require 'cc-mode)
(require 'cc-langs)
(require 'php-completion)
(require 'php-const)
(require 'php-doc)
(require 'php-edit)
(require 'php-font-lock)
(require 'php-format)
(require 'php-funcs)
(require 'php-help)
(require 'php-lineup)
(require 'php-parse)
(require 'php-project)
(require 'php-refactor)
(require 'php-string)
(require 'php-structure)
(require 'php-tags)
(require 'php-test)
(require 'php-utils)
(require 'string-utils)
(require 'php+-zf)

;;; *********
;;; CUSTOMIZE
;;; *********
(defgroup pdt nil
  "Customizations for pdt."
  :group 'languages)

(defcustom php-file-patterns '("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'")
 "List of file patterns for which to automatically invoke `pdt'."
 :type '(repeat (regexp :tag "Pattern"))
 :set (lambda (sym val)
        (set-default sym val)
        (let ((php-file-patterns-temp val))
          (while php-file-patterns-temp
            (add-to-list 'auto-mode-alist
                         (cons (car php-file-patterns-temp) 'pdt))
            (setq php-file-patterns-temp (cdr php-file-patterns-temp)))))
 :group 'pdt)

(defcustom pdt-default-face 'default
  "Default face in `pdt' buffers."
  :type 'face
  :group 'pdt)

(defcustom pdt-php-compile-on-save nil
  "Whether or not to run php-compile on files when they are
saved."
  :type 'boolean
  :group 'pdt)

(defcustom pdt-js-compile-on-save nil
  "Whether or not to run js-compile on files when they are
saved."
  :type 'boolean
  :group 'pdt)

(defcustom pdt-css-compile-on-save nil
  "Whether or not to run css-compile on files when they are
saved."
  :type 'boolean
  :group 'pdt)

(defcustom pdt-show-trailing-whitespace nil
  "Whether or not to turn show trailing whitespace."
  :type 'boolean
  :group 'pdt)

(defcustom pdt-delete-trailing-whitespace nil
  "Whether or not to trailing whitespace from files. If non-nil, the all
trailing whitespace will be deleted from files prior to saving."
  :type 'boolean
  :group 'pdt)

(defcustom pdt-protected-underscore nil
  "Whether or not protected properties and methods begin with an
underscore."
  :type 'boolean
  :group 'pdt)

(defcustom pdt-show-project-in-modeline nil
  "Whether or not to show the buffer's project in the modeline."
  :type 'boolean
  :group 'pdt)

;;; *********
;;; Variables
;;; *********
(defvar pdt-map nil "Keymap for pdt.")
(defvar php-mode-abbrev-table (make-abbrev-table) "Initial abbrev table")

;;; *********
;;; FUNCTIONS
;;; *********

(defun pdt-delete-all-trailing-whitespace ()
  "This is just a wrapper around ``delete-trailing-whitespace''
that checks the value of pdt-delete-trailing-whitespace
first."
  (when pdt-delete-trailing-whitespace
    (delete-trailing-whitespace)))

;; *************
;;; pdt Setup
;;; *************
(defun pdt-compile-on-save ()
  (when pdt-php-compile-on-save
    (php-compile-if-php))
  (when pdt-js-compile-on-save
    (js-compile-if-js))
  (when pdt-css-compile-on-save
    (css-compile-if-css)))

(defun pdt-setup ()
  "Prepares emacs for pdt."
  (add-to-list 'load-path
	       (convert-standard-filename
		(concat (file-name-directory (locate-library "pdt.el"))
			"bundled/"))))

(defun pdt-customize ()
  "Opens the customize buffer for pdt."
  (interactive)
  (customize-group "pdt"))

(defun pdt-reload (&optional only-revert-source)
  "This function reverts all file buffers and reloads the pdt lisp files.
Optional argument `only-revert-source` tells the function to only
revert open pdt source code file buffers. "
  (interactive "P")
  (save-some-buffers)
  (when (fboundp 'ert-delete-all-tests) (ert-delete-all-tests))
  (dolist (subdir '("" "tests/"))
    (dolist (file (remove-if
                   (lambda (x) (string-match "/\\." x))
                   (file-expand-wildcards
                    (concat (file-name-directory 
                             (locate-library "pdt.el")) subdir "*.el"))))
      (load-file file)))
  (let ((rb (current-buffer)))
    (dolist (b (buffer-list))
      (when (and (buffer-file-name b)
                 (or (not only-revert-source)
                     (string-match
                      (file-name-directory (find-lisp-object-file-name 
                                            'pdt 'function))
                      (file-name-directory (buffer-file-name b)))))
        (switch-to-buffer b)
        (when (or (not only-revert-source) (eq major-mode 'emacs-lisp-mode))
          (revert-buffer t t))))
    (switch-to-buffer rb))
  (pdt-define-keys)
  (pdt-define-menu))

(defun pdt-source-line-count ()
  "This function returns the number of lines of code that make up
pdt, not including unittests or bundled packages."
  (interactive)
  (shell-command
   (concat "less " 
           (file-name-directory (find-lisp-object-file-name 'pdt 
                                                            'function))
           "*.el | grep -v '^[[:space:]]*;\\\|^[^[:space:]]*$' | wc -l"))
  (let ((b (current-buffer))
        count)
    (switch-to-buffer (get-buffer "*Shell Command Output*"))
    (setq count (replace-regexp-in-string
                 "[ \n]" ""
                 (buffer-substring-no-properties (point-min) (point-max))))
    (switch-to-buffer b)
    (message count)))

;;; ***************
;;; Keymap and Menu
;;; ***************
(defun pdt-define-keys ()
  (define-key pdt-map "\C-cba" 'php-format-break-at-assignment-operators)
  (define-key pdt-map "\C-cbo" 'php-format-break-at-operators)
  (define-key pdt-map "\C-cbs" 'php-format-break-statement)
  (define-key pdt-map "\C-cb\C-s" 'php-format-break-string)
  (define-key pdt-map "\C-cb." 'php-format-break-at-concats)
  (define-key pdt-map "\C-cb," 'php-format-break-at-commas)
  (define-key pdt-map "\C-cb>" 'php-format-break-at-arrows)
  (define-key pdt-map "\C-cb\\" 'php-format-break-current)
  (define-key pdt-map "\C-cb\C-\\" 'php-format-break-class/interface)
  (define-key pdt-map "\C-cC" 'php-format-clean-up-script)
  (define-key pdt-map "\C-cd" 'php-change-string<->doc)
  (define-key pdt-map "\C-ch" 'php-change-bare-html<->heredoc)
  (define-key pdt-map "\C-ci" 'php-remove-this-concat)
  (define-key pdt-map "\C-c\M-i" 'php-implode-concats-in-statement)
  (define-key pdt-map "\C-cl" 'align-on)
  (define-key pdt-map "\C-cs" 'php-combine-scripts)
  (define-key pdt-map "\C-c'" 'php-change-string-quotes)
  (define-key pdt-map "\C-c\M-'" 'php-force-string-quotes-statement)
  (define-key pdt-map "\C-c(" 'php-find-current-sexp-begin)
  (define-key pdt-map "\C-c)" 'php-find-current-sexp-end)
  (define-key pdt-map "\C-c<" 'php-goto-start-of-script/html)
  (define-key pdt-map "\C-c>" 'php-goto-end-of-script/html)
  (define-key pdt-map [(control return)] 'php-format-break-statement)
  (define-key pdt-map "\C-cf" 'php-completion-lookup-at-point->message)
  (define-key pdt-map "\C-cza" 'zf-insert-action)
  (define-key pdt-map "\C-czb" 'zf-bootstrap)
  (define-key pdt-map "\C-czc" 'zf-controller)
  (define-key pdt-map "\C-czC" 'zf-insert-class)
  (define-key pdt-map "\C-czd" 'php-project-dired-directory)
  (define-key pdt-map "\C-czD" 'zf-create-directory-structure)
  (define-key pdt-map "\C-cze" 'zf-insert-method)
  (define-key pdt-map "\C-czE" 'php-modify-thing)
  (define-key pdt-map "\C-czf" 'zf-form)
  (define-key pdt-map "\C-czh" 'php-mark-current)
  (define-key pdt-map "\C-czi" 'zf-config)
  (define-key pdt-map "\C-czI" 'zf-interface)
  (define-key pdt-map "\C-czj" 'php-jump-to-thing)
  (define-key pdt-map "\C-czk" 'php-kill-current)
  (define-key pdt-map "\C-cz\M-k" 'php-kill-sexp-innard)
  (define-key pdt-map "\C-czl" 'zf-class)
  (define-key pdt-map "\C-czLc" 'zf-library-class)
  (define-key pdt-map "\C-czLi" 'zf-library-interface)
  (define-key pdt-map "\C-czm" 'zf-model)
  (define-key pdt-map "\C-czM" 'zf-create-module)
  (define-key pdt-map "\C-czn" 'zf-dbtable-model)
  (define-key pdt-map "\C-czN" 'zf-insert-interface)
  (define-key pdt-map "\C-czoi" 'zf-open-application-config)
  (define-key pdt-map "\C-czoo" 'php-project-open)
  (define-key pdt-map "\C-czou" 'php-project-open-phpunit-config)
  (define-key pdt-map "\C-czO" 'zf-insert-constant)
  (define-key pdt-map "\C-czpk" 'php-project-close)
  (define-key pdt-map "\C-czpd" 'php-project-show-directory)
  (define-key pdt-map "\C-czpvd" 'php-project-vc-dir)
  (define-key pdt-map "\C-czr" 'zf-insert-property)
  (define-key pdt-map "\C-czRm" 'php-refactor-move-thing-to-buffer)
  (define-key pdt-map "\C-czRM"
    'php-refactor-move-all-things-in-class/interface-to-buffer)
  (define-key pdt-map "\C-czRr" 'php-rearrange-current)
  (define-key pdt-map "\C-czRR" 'php-rearrange-innards)
  (define-key pdt-map "\C-cztt" 'php-compile)
  (define-key pdt-map "\C-cztT" 'php-test-full-project)
  (define-key pdt-map "\C-cztc" 'phpcs)
  (define-key pdt-map "\C-cztg" 'php-compile-again)
  (define-key pdt-map "\C-cztm" 'phpmd)
  (define-key pdt-map "\C-cztu" 'phpunit)
  (define-key pdt-map "\C-cztU" 'phpunit-single-test)
  (define-key pdt-map "\C-czv" 'zf-view-script-via-controller-action)
  (define-key pdt-map "\C-czV" 'zf-view-script)
  (define-key pdt-map "\C-czy" 'php-yank)
  (define-key pdt-map "\C-czz" 'zf-insert-dump)
  (define-key pdt-map "\C-czZ" 'zf-insert-dump-and-die)
  (define-key pdt-map "\C-cz;" 'php-comment-current)
  (define-key pdt-map [(control meta ?\>)] 'php-kill-chain-link)
  (define-key pdt-map [tab] 'php-tab-key)
  (define-key pdt-map [return] 'php-auto-fill)
  (define-key pdt-map "\M-." 'php-find-tag)
  (define-key pdt-map "\M-[" 'php-jump-to-previous-thing)
  (define-key pdt-map "\M-]" 'php-jump-to-next-thing)
  (define-key pdt-map "\M-;" 'php-comment-dwim)
  (define-key pdt-map [(super ?\[)] 'php-hide-innards)
  (define-key pdt-map [(super ?\{)] 'php-hide-class/interface-doc-blocks)
  (define-key pdt-map [(super ?\])] 'php-show-all)
  (define-key pdt-map [(super ?\})] 'php-show-class/interface-doc-blocks)
  (define-key pdt-map [(super ?\l)] 'php-goto-line)
  (define-key pdt-map "\C-c\C-f" 'php-search-documentation)
)

(defun pdt-define-menu ()
  (define-key pdt-map
    [menu-bar pdt]
    (cons "PDT" (make-sparse-keymap "PDT")))

  (define-key (lookup-key pdt-map [menu-bar pdt])
    [create]
    (cons "Create/Open" (make-sparse-keymap "Create")))
  (define-key (lookup-key pdt-map [menu-bar pdt create])
    [module]
    '("Module" . zf-create-module))
  (define-key-after (lookup-key pdt-map [menu-bar pdt create])
    [controller]
    '("Controller" . zf-controller)
    'module)
  (define-key-after (lookup-key pdt-map [menu-bar pdt create])
    [view-via]
    '("View Via Action" . zf-view-script-via-controller-action)
    'controller)
  (define-key-after (lookup-key pdt-map [menu-bar pdt create])
    [view]
    '("View" . zf-view-script)
    'view-via)
  (define-key-after (lookup-key pdt-map [menu-bar pdt create])
    [sep1]
    '("--single-line")
    'view)
  (define-key-after (lookup-key pdt-map [menu-bar pdt create])
    [model]
    '("Model" . zf-model)
    'sep1)
  (define-key-after (lookup-key pdt-map [menu-bar pdt create])
    [dbtable]
    '("Database Table Model" . zf-dbtable-model)
    'model)
  (define-key-after (lookup-key pdt-map [menu-bar pdt create])
    [form]
    '("Form" . zf-form)
    'dbtable)
  (define-key-after (lookup-key pdt-map [menu-bar pdt create])
    [sep2]
    '("--single-line")
    'form)
  (define-key-after (lookup-key pdt-map [menu-bar pdt create])
    [bootstrap]
    '("Bootstrap" . zf-bootstrap)
    'sep2)
  (define-key-after (lookup-key pdt-map [menu-bar pdt create])
    [class-file]
    '("Class" . zf-class)
    'bootstrap)
  (define-key-after (lookup-key pdt-map [menu-bar pdt create])
    [interface-file]
    '("Interface" . zf-interface)
    'class-file)
  (define-key-after (lookup-key pdt-map [menu-bar pdt create])
    [sep4]
    '("--single-line")
    'interface-file)
  (define-key-after (lookup-key pdt-map [menu-bar pdt create])
    [config]
    '("Config" . zf-config)
    'sep4)
  (define-key-after (lookup-key pdt-map [menu-bar pdt create])
    [open-config]
    '("Open application.ini" . zf-open-application-config)
    'config)
  (define-key-after (lookup-key pdt-map [menu-bar pdt create])
    [sep3]
    '("--single-line")
    'open-config)
  (define-key-after (lookup-key pdt-map [menu-bar pdt create])
    [function]
    '("Jump to Function" . php-jump-to-function)
    'sep3)

  (define-key-after (lookup-key pdt-map [menu-bar pdt])
    [insert]
    (cons "Insert" (make-sparse-keymap "Insert"))
    'create)
  (define-key (lookup-key pdt-map [menu-bar pdt insert])
    [action]
    '("Action" . zf-insert-action))
  (define-key-after (lookup-key pdt-map [menu-bar pdt insert])
    [method]
    '("Method" . zf-insert-method)
    'action)
  (define-key-after (lookup-key pdt-map [menu-bar pdt insert])
    [constant]
    '("Constant" . zf-insert-constant)
    'method)
  (define-key-after (lookup-key pdt-map [menu-bar pdt insert])
    [property]
    '("Property" . zf-insert-property)
    'constant)
  (define-key-after (lookup-key pdt-map [menu-bar pdt insert])
    [class]
    '("Class" . zf-insert-class)
    'property)
  (define-key-after (lookup-key pdt-map [menu-bar pdt insert])
    [interface]
    '("Interface" . zf-insert-interface)
    'class)

  (define-key-after (lookup-key pdt-map [menu-bar pdt insert])
    [library]
    `("Library" . ,(make-sparse-keymap "Library"))
    'interface)
  (define-key (lookup-key pdt-map [menu-bar pdt insert library])
    [library-class]
    '("Class" . zf-library-class))
  (define-key-after (lookup-key pdt-map [menu-bar pdt insert library])
    [library-interface]
    '("Interface" . zf-library-interface)
    'library-class)

  (define-key-after (lookup-key pdt-map [menu-bar pdt insert])
    [dump]
    '("Dump" . zf-insert-dump)
    'library)
  (define-key-after (lookup-key pdt-map [menu-bar pdt insert])
    [dump-die]
    '("Dump and Die" . zf-insert-dump-and-die)
    'dump)

  (define-key-after (lookup-key pdt-map [menu-bar pdt])
    [edit]
    (cons "Edit" (make-sparse-keymap "Edit"))
    'insert)

  (define-key-after (lookup-key pdt-map [menu-bar pdt edit])
    [refactor]
    `("Refactor" . ,(make-sparse-keymap "Refactor"))
    'edit)
  (define-key (lookup-key pdt-map [menu-bar pdt edit refactor])
    [move-thing]
    '("Move Thing to Buffer" . php-refactor-move-thing-to-buffer))
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit refactor])
    [move-all-things]
    '("Move All Things in Class/Interface to Buffer" .
      php-refactor-move-all-things-in-class/interface-to-buffer)
    'move-thing)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit refactor])
    [rearrange-current]
    '("Rearrange Current Thing" . php-rearrange-current)
    'move-all-things)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit refactor])
    [rearrange-innards]
    '("Rearrange Innards" . php-rearrange-innards)
    'rearrange-current)

  (define-key-after (lookup-key pdt-map [menu-bar pdt edit])
    [break]
    `("Break" . ,(make-sparse-keymap "Break"))
    'refactor)
  (define-key (lookup-key pdt-map [menu-bar pdt edit break])
    [break-current]
    '("Current Thing" . php-format-break-current))
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit break])
    [break-class/interface]
    '("Current Class/Interface" . php-format-break-class/interface)
    'break-current)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit break])
    [break-statement]
    '("Statement" . php-format-break-statement) 'break-class/interface)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit break])
    [break-string]
    '("String" . php-format-break-string) 'break-statement)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit break])
    [break-at-assignment-operators]
    '("At Assignment Operators" . php-format-break-at-assignment-operators)
    'break-string)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit break])
    [break-at-operators]
    '("At Operators" . php-format-break-at-operators)
    'break-at-assignment-operators)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit break])
    [break-at-concats]
    '("At Concats" . php-format-break-at-concats) 'break-at-operators)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit break])
    [break-at-commas]
    '("At Commas" . php-format-break-at-commas) 'break-at-concats)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit break])
    [break-at-arrows]
    '("At Arrows" . php-format-break-at-arrows) 'break-at-commas)

  (define-key-after (lookup-key pdt-map [menu-bar pdt edit])
    [mark-current]
    '("Mark Current Thing" . php-mark-current) 'break)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit])
    [kill-current]
    '("Kill Current Thing" . php-kill-current) 'mark-current)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit])
    [yank-killed]
    '("Yank Killed Thing" . php-yank) 'kill-current)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit])
    [kill-sexp-innard]
    '("Kill Current Sexp Innard" . php-kill-sexp-innard)
    'yank-killed)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit])
    [cleanup-script]
    '("Clean Up Script" . php-format-clean-up-script)
    'kill-sexp-innard)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit])
    [remove-this-concat]
    '("Remove Single Concatenation" . php-remove-this-concat)
    'cleanup-script)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit])
    [implode-concat]
    '("Implode This Concatenation" . php-implode-concat)
    'remove-this-concat)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit])
    [change-string-quotes]
    '("Toggle String Quoting" . php-change-string-quotes)
    'implode-concat)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit])
    [change-string<->doc]
    '("Toggle String/Doc" . php-change-string<->doc)
    'change-string-quotes)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit])
    [change-bare-html<->heredoc]
    '("Toggle HTML/Heredoc" . php-change-bare-html<->heredoc)
    'change-string<->doc)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit])
    [combine-scripts]
    '("Combine Consecutive Scripts" . php-combine-scripts)
    'change-short-tags)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit])
    [align-on]
    '("Align On" . align-on)
    'combine-scripts)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit])
    [modify-method]
    '("Modify Thing" . php-modify-thing)
    'align-on)
  (define-key-after (lookup-key pdt-map [menu-bar pdt edit])
    [modify-method-args]
    '("Modify Method Arguments" . php-modify-method-argument)
    'modify-method)

  (define-key-after (lookup-key pdt-map [menu-bar pdt])
    [jump]
    '("Jump to Thing" . php-jump-to-thing)
    'edit)

  (define-key-after (lookup-key pdt-map [menu-bar pdt])
    [sep1]
    '("--single-line")
    'edit)

  (define-key-after (lookup-key pdt-map [menu-bar pdt])
    [project]
    (cons "Project" (make-sparse-keymap "Project"))
    'sep1)
  (define-key (lookup-key pdt-map [menu-bar pdt project])
    [show-dir]
    '("Show Project Directory" . php-project-show-directory))
  (define-key-after (lookup-key pdt-map [menu-bar pdt project])
    [create-dir]
    '("Create Project Directory" . zf-create-directory-structure)
    'show-dir)
  (define-key-after (lookup-key pdt-map [menu-bar pdt project])
    [dired]
    '("Open Project Directory in Dired" . php-project-dired-directory)
    'create-dir)
  (define-key-after (lookup-key pdt-map [menu-bar pdt project])
    [project-close]
    '("Close Project" . php-project-close)
    'dired)
  (define-key-after (lookup-key pdt-map [menu-bar pdt project])
    [project-add]
    '("Add Project" . php-project-add)
    'project-close)
  (define-key-after (lookup-key pdt-map [menu-bar pdt project])
    [project-remove]
    '("Remove Project" . php-project-remove)
    'project-add)
  (define-key-after (lookup-key pdt-map [menu-bar pdt project])
    [php-vc-dir]
    '("Project Directory VC" . php-project-vc-dir)
    'project-remove)
  (define-key-after (lookup-key pdt-map [menu-bar pdt project])
    [customize]
    '("Customize Projects" . php-project-customize)
    'php-vc-dir)

  (define-key-after (lookup-key pdt-map [menu-bar pdt])
    [tags]
    (cons "Tags" (make-sparse-keymap "Tags"))
    'project)
  (define-key (lookup-key pdt-map [menu-bar pdt tags])
    [create]
    '("Create Tags File" . php-create-tag-file))
  (define-key-after (lookup-key pdt-map [menu-bar pdt tags])
    [create-with-dirs]
    '("Create Tags File With Extra Directories" . php-create-tag-file-with-dirs)
    'create)
  (define-key-after (lookup-key pdt-map [menu-bar pdt tags])
    [load]
    '("Load Tags File" . load-tags)
    'create-with-dirs)
  (define-key-after (lookup-key pdt-map [menu-bar pdt tags])
    [customize]
    '("Customize Tags" . php-tags-customize)
    'load)

  (define-key-after (lookup-key pdt-map [menu-bar pdt])
    [test]
    (cons "Test" (make-sparse-keymap "Test"))
    'tags)
  (define-key (lookup-key pdt-map [menu-bar pdt test])
    [compile]
    '("Compile" . php-compile))
  (define-key-after (lookup-key pdt-map [menu-bar pdt test])
    [test-full-project]
    '("Test Full Project" . php-test-full-project)
    'compile)
  (define-key-after (lookup-key pdt-map [menu-bar pdt test])
    [compile-again]
    '("Run Test Again" . php-compile-again)
    'test-full-project)
  (define-key-after (lookup-key pdt-map [menu-bar pdt test])
    [lint]
    '("PHP Lint" . php-lint)
    'compile-again)
  (define-key-after (lookup-key pdt-map [menu-bar pdt test])
    [lint-all]
    '("PHP Lint All" . php-lint-all)
    'lint)
  (define-key-after (lookup-key pdt-map [menu-bar pdt test])
    [phpcs]
    '("PHPCS" . phpcs)
    'lint-all)
  (define-key-after (lookup-key pdt-map [menu-bar pdt test])
    [phpcs-all]
    '("PHPCS All" . phpcs-all)
    'phpcs)
  (define-key-after (lookup-key pdt-map [menu-bar pdt test])
    [phpmd]
    '("PHPMD" . phpmd)
    'phpcs-all)
  (define-key-after (lookup-key pdt-map [menu-bar pdt test])
    [phpmd-all]
    '("PHPMD All" . phpmd-all)
    'phpmd)
  (define-key-after (lookup-key pdt-map [menu-bar pdt test])
    [phpunit]
    '("PHPUnit" . phpunit)
    'phpmd-all)
  (define-key-after (lookup-key pdt-map [menu-bar pdt test])
    [phpunit-all]
    '("PHPUnit All" . phpunit-all)
    'phpunit)
  (define-key-after (lookup-key pdt-map [menu-bar pdt test])
    [phpunit-single]
    '("PHPUnit Single Test" . phpunit-single-test)
    'phpunit-all)
  (define-key-after (lookup-key pdt-map [menu-bar pdt test])
    [phpunit-open]
    '("Open PHPUnit Config" . php-project-open-phpunit-config)
    'phpunit-single)
  (define-key-after (lookup-key pdt-map [menu-bar pdt test])
    [phpunit-logging]
    '("Toggle PHPUnit Logging" . phpunit-toggle-logging)
    'phpunit-open)
  (define-key-after (lookup-key pdt-map [menu-bar pdt test])
    [customize]
    '("Customize Testing" . php-test-customize)
    'phpunit-logging)

  (define-key-after (lookup-key pdt-map [menu-bar pdt])
    [sep2]
    '("--single-line")
    'test)

  (define-key-after (lookup-key pdt-map [menu-bar pdt])
    [customize]
    '("Customize Pdt Mode" . pdt-customize)
    'sep2)
  (define-key pdt-map [menu-bar pdt search-documentation]
    '("Search documentation" . php-search-documentation))
)

(defun pdt-set-keymap-and-menu ()
  (setq pdt-map (make-sparse-keymap))
  (pdt-define-keys)
  (pdt-define-menu))

(unless pdt-map
  (pdt-set-keymap-and-menu))

;;; *******
;;; pdt
;;; *******
(define-derived-mode pdt c-mode (concat
                                     "pdt"
                                     (when (and
                                            pdt-show-project-in-modeline
                                            (php-project-nickname))
                                       (concat "[" (php-project-nickname) "]")))
  "Major mode for making developing Zend Framework PHP applications lazier.

\\{pdt-map}"
  (pdt-setup)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (c-add-language 'pdt 'c-mode)
  (set (make-local-variable 'c-basic-offset) 4)
  (set (make-local-variable 'c-opt-cpp-start) php-tags-key)
  (set (make-local-variable 'c-opt-cpp-prefix) php-tags-key)
  (c-set-offset 'cpp-macro 0)
  (set (make-local-variable 'c-block-stmt-1-key) php-block-stmt-1-key)
  (set (make-local-variable 'c-block-stmt-2-key) php-block-stmt-2-key)
  (set (make-local-variable 'c-doc-comment-style)
       '((pdt . javadoc)))

  (php-setup-font-locking)

  ;; Do not force newline at end of file.  Such newlines can cause
  ;; trouble if the PHP file is included in another file before calls
  ;; to header() or cookie().
  (set (make-local-variable 'require-final-newline) nil)
  (set (make-local-variable 'next-line-add-newlines) nil)

  (setq c-special-indent-hook nil)

  (turn-on-font-lock)
  (c-set-offset 'case-label '+)
  (c-set-offset 'cpp-macro 'php-cpp-macro-lineup)
  (c-set-offset 'arglist-intro 'php-arglist-intro-lineup)
  (c-set-offset 'arglist-close 'php-arglist-close-lineup)
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'knr-argdecl 'php-knr-argdecl-lineup)
  (c-set-offset 'knr-argdecl-intro 'php-knr-argdecl-lineup)
  (c-set-offset 'topmost-intro 'php-topmost-intro-lineup)
  (c-set-offset 'topmost-intro-cont 'php-topmost-intro-cont-lineup)
  (c-set-offset 'c 'php-comment-lineup)
  (c-set-offset 'comment-intro 'php-comment-intro-lineup)
  (c-set-offset 'defun-close 'php-defun-close-lineup)
  (c-set-offset 'statement 'php-statement-lineup)
  (c-set-offset 'statement-cont 'php-statement-cont-lineup)
  (c-set-offset 'string 'php-string-lineup)
  (c-set-offset 'brace-list-intro 'php-brace-list-intro-lineup)
  (c-set-offset 'brace-list-entry 'php-brace-list-entry-lineup)
  (c-set-offset 'brace-list-close 'php-brace-list-close-lineup)
  (c-set-offset 'func-decl-cont 'php-func-decl-cont)

  (when (and pdt-show-trailing-whitespace
             (boundp 'show-trailing-whitespace))
    (setq show-trailing-whitespace t))
  (add-hook 'before-save-hook 'pdt-delete-all-trailing-whitespace nil t)
  (add-hook 'after-save-hook 'pdt-compile-on-save nil t)
  (load-tags)
  (php-text-struct-cache-initialize)
  (run-hooks 'pdt-hook))

(defcustom pdt-hook nil
  "List of functions to be executed on entry to `pdt'."
  :type 'hook
  :group 'pdt)

(provide 'pdt)

;;; pdt.el ends here
