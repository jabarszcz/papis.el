;; papis-citar.el --- Citar backend for Papis -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jean-Alexandre Barszcz

;; Author: Jean-Alexandre Barszcz <jean-alexandre.barszcz@umontreal.ca>
;; Version: 0.1
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

;;; Code:

(require 'citar)
(require 'papis)

;;;; Callback functions for notes

(defun papis-citar-get-notes (&optional citekeys)
  "Return a table mapping bibliography keys to their note file path.

If the list CITEKEYS is given, only consider the corresponding documents."
  (let ((docs (papis--query-documents)) ; Runs papis
        (table (make-hash-table :test 'equal)))
    (dolist (doc docs table)
      (let ((citekey (papis--doc-ref doc)))
        (if (or (not citekeys) (member citekey citekeys))
            (when-let ((path (papis--doc-notes-path doc)))
              (puthash citekey (list path) table)))))))

(defun papis-citar-has-notes ()
  "Return a predicate returning t when CITEKEY has a note file."
  (let ((notes (papis-citar-get-notes))) ; Runs papis
    (unless (hash-table-empty-p notes)
      (lambda (citekey) (and (gethash citekey notes) t)))))

(defun papis-citar-open-note (filename)
  "Open the note file FILENAME, and run `papis-citar-open-note-hook'."
  (find-file filename)
  (run-hook-with-args 'papis-after-open-note-functions
                      nil ; We don't have a doc
                      nil)) ; The note already exists

(defun papis-citar-create-note (citekey entry)
  "Create and open a note file from CITEKEY and ENTRY.
Defers the actual note creation
to `citar-note-format-function', so make sure that this is set
appropriately (see `papis-citar-note-format-default')."
  (citar--check-configuration 'citar-note-format-function)
  (funcall citar-note-format-function citekey entry)) ; Runs Papis

(defun papis-citar-note-format-default (citekey &optional _entry)
  "Create and open a note for CITEKEY, using Papis' template."
  (let ((created-notes-path
         (papis--ensured-notes-path citekey))) ; Runs Papis
    (find-file created-notes-path)
    (run-hook-with-args 'papis-after-open-note-functions
                        nil ; We don't have a doc
                        t))) ; The note has just been created

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
  (papis-check-program)
  (customize-set-variable 'citar-note-format-function
                          #'papis-citar-note-format-default)
  (citar-register-notes-source 'papis-citar
                               papis-citar-notes-config)
  (customize-set-variable 'citar-notes-source
                          'papis-citar))

;;;; Callback functions for bibliography files

(defun papis-citar-get-library-files (&optional citekeys)
  "Return a table mapping bibliography keys to their file paths.

If the list CITEKEYS is given, only consider the corresponding documents."
  (let ((docs (papis--query-documents)) ; Runs papis
        (table (make-hash-table :test 'equal)))
    (dolist (doc docs table)
      (let ((citekey (papis--doc-ref doc)))
        (if (or (not citekeys) (member citekey citekeys))
            (when-let ((paths (papis--doc-file-paths doc)))
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
  (papis-check-program)
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
