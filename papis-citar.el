;; papis-citar.el --- Citar backend for Papis -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jean-Alexandre Barszcz

;; Author: Jean-Alexandre Barszcz <jean-alexandre.barszcz@umontreal.ca>
;; Version: 1.0
;; Package-Requires ((citar "1.2.0") papis)
;; Keywords: Citar, Papis, Bibliography
;; URL: https://github.com/papis/papis.el

;;; Commentary:

;; This package registers a Citar backend locating bibliography files
;; and notes about them. Each callback function runs the 'papis'
;; command at most once.

;; Add this to your configuration:

;; (use-package papis-citar
;;   :after citar
;;   :config (papis-citar-setup))

;; TODO Replace Citar's global bibfiles (`citar-bibliography').
;; Currently, org-cite and Citar need file(s) to get the library data,
;; but Papis has it's own library database. We could 1) automatically
;; update a bibfile from Papis on modification, or 2) update it from
;; emacs on demand by calling the Papis program at each Citar entry
;; point, or 3) skip the file entirely and populate the entries for
;; Citar directly from Papis (see `citar--entries').

;;; Code:

(require 'citar)
(require 'papis)

;;;; Customization

(defgroup papis-citar nil
  "Citar backend for Papis."
  :prefix "papis-citar-"
  :group 'papis
  )

(defcustom papis-citar-open-note-hook nil
  "A hook to run when opening a note.

For example, it can be used to move the cursor to a specific place in
note templates."
  :type 'hook
  :group 'papis-citar)

;;;; Internal functions

;; Those functions are mostly "improved" versions of similar functions
;; in 'papis.el'

(defun papis-citar--cmd (args &optional destination)
  "Run the papis program with arguments ARGS.

An alternative to `papis--cmd' with more flexible output.

ARGS is the list of arguments passed to the Papis program (sub-command
included).

DESTINATION uses the same format as `call-process'."
  ;; TODO Merge with `papis--cmd'
  (let* ((cmd papis-binary-path)
         (libopt (when papis-library (list "-l" papis-library)))
         (args (append libopt args))
         (cmdstr (string-join (cons cmd args) " ")))
    (message cmdstr)
    (apply 'call-process cmd nil destination nil args)))

(defun papis-citar--cmd-to-string (args &optional mixstderr)
  "Run the Papis program with ARGS, and copy the output to a string.

ARGS is the list of arguments passed to the Papis program (sub-command
included).

When MIXSTDERR is t, the returned string also includes the error output."
  (let ((outbuf (generate-new-buffer " *papis-citar-output*")))
    (with-current-buffer outbuf
      (papis-citar--cmd args (list outbuf mixstderr))
      (prog1 (string-trim (buffer-string))
        (kill-buffer)))))

(defun papis-citar--library-documents ()
  "Run `papis export' and return hashtables of documents."
  ;; TODO Merge with `papis-query'
  (let* ((outbuf (generate-new-buffer " *papis-citar-json-export*"))
         (dest (list outbuf nil)))
    (with-current-buffer outbuf
      (save-excursion
        (papis-citar--cmd '("export" "--all" "--format" "json") dest))
      (prog1 (json-parse-buffer :array-type 'list
                                :null-object nil
                                :false-object nil)
        (kill-buffer)))))

(defun papis-citar--doc-notes-path (doc)
  "Return the path to the note file associated with DOC, if it exists."
  (when-let* ((dir (papis-doc-get-folder doc))
              (file (gethash "notes" doc))
              (path (file-name-concat dir file)))
    (and (file-exists-p path) path)))

(defun papis-citar--lib-file-paths (doc)
  "Return the paths to existing library files associated with DOC."
  (when-let ((dir (papis-doc-get-folder doc))
             (files (gethash "files" doc)))
    (mapcan (lambda (file)
              (let ((path (file-name-concat dir file)))
                (and (file-exists-p path) (list path))))
            files)))

;;;; Checking the papis program and its version

(defvar papis-citar--papis-program-version nil
  "Papis program version, if known. See `papis-citar--check-program'.")

(defcustom papis-citar-skip-program-check nil
  "When non-nil, don't check the Papis program version or runnability."
  :type 'boolean
  :group 'papis-citar)

(defun papis-citar-check-program (&optional min-version)
  "Run the Papis program and return its version-list. Error if absent.

Return nil when variable `papis-citar-skip-program-check' is non-nil.

If the obtained version is lower than MIN-VERSION, throw an error.

Idempotence: Save the version to variable
`papis-citar--papis-program-version', to avoid re-running the Papis
program over and over."
  (unless papis-citar-skip-program-check
    (setq papis-citar--papis-program-version
          (or papis-citar--papis-program-version
              (let ((output (papis-citar--cmd-to-string '("--version"))))
                (version-to-list (car (last (string-split output)))))))
    (when-let ((version papis-citar--papis-program-version)
               (min-version (if (stringp min-version)
                                (version-to-list min-version)
                              min-version)))
      (when (version-list-< version min-version)
        (error
         (format "Papis program version %S lower than required %S"
                 version min-version))))
    papis-citar--papis-program-version))

;;;; Callback functions for notes

(defun papis-citar-get-notes (&optional citekeys)
  "Return a table mapping bibliography keys to their note file path.

If the list CITEKEYS is given, only consider the corresponding documents."
  (let ((docs (papis-citar--library-documents)) ; Runs papis
        (table (make-hash-table :test 'equal)))
    (dolist (doc docs table)
      (let ((citekey (papis--get-ref doc)))
        (if (or (not citekeys) (member citekey citekeys))
            (when-let ((path (papis-citar--doc-notes-path doc)))
              (puthash citekey (list path) table)))))))

(defun papis-citar-has-notes ()
  "Return a predicate returning t when CITEKEY has a note file."
  (let ((notes (papis-citar-get-notes))) ; Runs papis
    (unless (hash-table-empty-p notes)
      (lambda (citekey) (and (gethash citekey notes) t)))))

(defun papis-citar-open-note (filename)
  "Open the note file FILENAME, and run `papis-citar-open-note-hook'."
  (find-file filename)
  (run-hooks 'papis-citar-open-note-hook))

(defun papis-citar-create-note (citekey entry)
  "Create a note file from CITEKEY and ENTRY.
Defers the actual note creation
to `citar-note-format-function', so make sure that this is set
appropriately (see `papis-citar-format-note-default')."
  (citar--check-configuration 'citar-note-format-function)
  (funcall citar-note-format-function citekey entry))

(defun papis-citar-format-note-default (citekey &optional _entry)
  "Create and open a note for CITEKEY, using Papis' template."
  ;; TODO Merge with papis--ensured-notes-path
  (let ((created-notes-path
         (papis-citar--cmd-to-string
          (list "edit" "--notes" "--editor" "echo" citekey))))
    (papis-citar-open-note created-notes-path)))

(defconst papis-citar-notes-config
  (list :name "Papis Notes"
        :category 'file
        :items #'papis-citar-get-notes
        :hasitems #'papis-citar-has-notes
        :open #'papis-citar-open-note
        :create #'papis-citar-create-note
        )
  "Citar notes configuration for papis.")

(defun papis-citar-notes-setup ()
  "Assign the right variables to use Papis notes in Citar."
  (papis-citar-check-program)
  (customize-set-variable 'citar-note-format-function
                          #'papis-citar-format-note-default)
  (citar-register-notes-source 'papis-citar
                               papis-citar-notes-config)
  (customize-set-variable 'citar-notes-source
                          'papis-citar))

;;;; Callback functions for bibliography files

(defun papis-citar-get-library-files (&optional citekeys)
  "Return a table mapping bibliography keys to their file paths.

If the list CITEKEYS is given, only consider the corresponding documents."
  (let ((docs (papis-citar--library-documents)) ; Runs papis
        (table (make-hash-table :test 'equal)))
    (dolist (doc docs table)
      (let ((citekey (papis--get-ref doc)))
        (if (or (not citekeys) (member citekey citekeys))
            (when-let ((paths (papis-citar--lib-file-paths doc)))
              (puthash citekey paths table)))))))

(defun papis-citar-has-library-files ()
  "Return predicate testing whether CITEKEY has library files."
  (let ((files (papis-citar-get-library-files)))
    (unless (hash-table-empty-p files)
      (lambda (citekey) (and (gethash citekey files) t)))))

(defconst papis-citar-files-config
  (list :items #'papis-citar-get-library-files
        :hasitems #'papis-citar-has-library-files)
  "Citar library files configuration for papis.")

(defun papis-citar-library-files-setup ()
  "Assign the right variables to use Papis library files in Citar."
  (papis-citar-check-program)
  (customize-set-variable 'citar-file-sources
                          (list papis-citar-files-config)))

;; TODO customize variable `citar-add-file-function'

;;;; General Setup

(defun papis-citar-setup ()
  "Integrate Papis and Citar."
  (papis-citar-notes-setup)
  (papis-citar-library-files-setup))

(provide 'papis-citar)

;;; papis-citar.el ends here
