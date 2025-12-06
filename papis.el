;; papis.el --- Use Papis from emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 Alejandro Gallo
;; Copyright (C) 2025 Jean-Alexandre Barszcz

;; Keywords: Papis, Bibliography
;; URL: https://github.com/papis/papis.el

;;; Commentary:

;; TODO

;;; Code:

(require 'ol)
(require 'org-element)
(require 'json)

;;;; Customization

(defgroup papis nil
  "Official Papis package for Emacs."
  :group 'external
  :prefix "papis-"
  :link '(url-link :tag "Github"
          "https://github.com/papis/papis.el"))

(defcustom papis-program "papis"
  "The path to the Papis program.

You might have Papis installed, for instance, in some virtual
environment."
  :type 'string
  :group 'papis)

(defcustom papis-library nil
  "Papis library to be used in commands.

When nil, use the default library configured in the Papis config."
  :type 'string
  :group 'papis)

(defcustom papis-extra-options nil
  "Additional options for papis commands."
  :type '(repeat string)
  :group 'papis)

(defcustom papis-skip-program-check nil
  "When non-nil, don't check the Papis program version or runnability."
  :type 'boolean
  :group 'papis)

(defcustom papis-read-format-function
  #'papis-default-read-format-function
  "Function taking a papis document (hashmap) and outputing a
   string representation of it to be fed into the reader."
  :type 'function
  :group 'papis)

(defcustom papis--query-prompt
  "Papis Query: "
  "The prompt to show users in order to accept a query
  "
  :type 'string
  :group 'papis)

;;;; Functions to run Papis

(defun papis--add-options (args)
  "Append options to the arguments list ARGS passed to Papis.

The additional options are configured in the variable `papis-library'
and the variable `papis-extra-options'."
  (let ((libopt (when papis-library (list "-l" papis-library))))
    (append libopt papis-extra-options args)))

(defun papis--cmd-str (args)
  "Return the string for the command running Papis with ARGS."
  (let* ((cmd papis-program)
         (args (papis--add-options args)))
    (combine-and-quote-strings (cons cmd args))))

(defun papis--run (args &optional destination)
  "Run the papis program with arguments ARGS.

ARGS is the list of arguments passed to the Papis program (sub-command
included).

DESTINATION uses the same format as `call-process'."
  (let ((cmd papis-program)
        (all-args (papis--add-options args)))
    (message (papis--cmd-str args))
    (apply 'call-process cmd nil destination nil all-args)))

(defun papis--run-to-string (args &optional mixstderr)
  "Run the Papis program with ARGS, and copy the output to a string.

ARGS is the list of arguments passed to the Papis program (sub-command
included).

When MIXSTDERR is t, the returned string also includes the error output."
  (let ((outbuf (generate-new-buffer " *papis-output*")))
    (with-current-buffer outbuf
      (papis--run args (list outbuf mixstderr))
      (prog1 (string-trim (buffer-string))
        (kill-buffer)))))

(cl-defun papis--cmd (cmd &optional with-stdout)
  "Run the Papis subcommand CMD.

Return the output as a string when WITH-STDOUT is non-nil."
  (let* ((lib-flags (if papis-library
                        (concat "-l " papis-library)
                      ""))
         (extra (combine-and-quote-strings papis-extra-options))
         (sys (if with-stdout
                  #'shell-command-to-string
                #'shell-command))
         (full-cmd (format "%s %s %s %s" papis-program lib-flags extra cmd)))
    (message full-cmd)
    (funcall sys
             full-cmd)))

;;;; Checking the papis program and its version

(defvar papis-program-version nil
  "Papis program version (as a list), if known. See `papis-check-program'.")

(defun papis-check-program (&optional min-version)
  "Run the Papis program and return its version-list. Error if absent.

When variable `papis-skip-program-check' is non-nil, just return nil
without running Papis.

If the obtained version is lower than MIN-VERSION, throw an error.

Idempotence: Save the version to variable
`papis-program-version', to avoid re-running the Papis
program over and over."
  (interactive)
  (unless papis-skip-program-check
    (unless papis-program-version
      (setq papis-program-version
            (let ((output (papis--run-to-string '("--version"))))
              (version-to-list (car (last (string-split output)))))))
    (when-let ((version papis-program-version)
               (min-version (if (stringp min-version)
                                (version-to-list min-version)
                              min-version)))
      (when (version-list-< version min-version)
        (error
         (format "Papis program version %S lower than required %S"
                 version min-version))))
    papis-program-version))

;;;; Getting document metadata from Papis

(defun papis--query-documents (&optional query doc-folder)
  "Run `papis export' and return the list of documents as hashtables.

Use a QUERY string to let Papis select some documents, or DOC-FOLDER for
it to search in only one directory."
  (let ((outbuf (generate-new-buffer " *papis-json-export*"))
        (cmd `("export" "--format" "json" ,(or query "--all")
               ,@(when doc-folder (list "--doc-folder" doc-folder)))))
    (with-current-buffer outbuf
      (save-excursion (papis--run cmd (list t nil)))
      (prog1 (json-parse-buffer :array-type 'list
                                :null-object nil
                                :false-object nil)
        (kill-buffer)))))

;;;; Papis Documents

(defun papis-doc-get-folder (doc)
  (papis-doc-get doc "_papis_local_folder"))

(defun papis-doc-id (doc)
  (let ((id (papis-doc-get doc "papis_id")))
    (unless id
      (error "Document '%s' does not have an id!"
             doc))
    id))

(defun papis-id-query (doc)
  (format "papis_id:%s" (papis-doc-id doc)))

(defun papis--get-file-paths (doc)
  (mapcar (lambda (f) (concat (papis-doc-get-folder doc) "/" f))
          (papis-doc-get doc "files")))

(defun papis-doc-get (doc key &optional default)
  (gethash key doc default))

(defun papis--get-ref (doc)
  (papis-doc-get doc "ref"))

(defun papis--doc-update (doc)
  (let ((folder (papis-doc-get-folder doc)))
    (papis--cmd (concat "update --doc-folder " folder))))

;;;; Public Papis commands

(defun papis-browse (doc)
  (interactive (list (papis--read-doc)))
  (let ((url
         (cond
           ((papis-doc-get doc "url" nil))
           ((when-let ((doi (papis-doc-get doc "doi" nil)))
              (format "https://doi.org/%s" doi)))
           (t (error "Neither url nor doi found in this document.")))))
    (browse-url url)))

(defun papis-open (doc)
  (interactive (list (papis--read-doc)))
  (let* ((files (papis--get-file-paths doc))
         (file (pcase (length files)
                 (1 (car files))
                 (0 (error "Doc has no files"))
                 (_ (completing-read "file: " files)))))
    (split-window-horizontally)
    (find-file file)))

;;;; Notes

(defcustom papis-edit-new-notes-hook nil
  "Hook for when a new note file is being edited.

   The argument of the hook is the respective document."
  :type 'hook)

(defun papis--default-notes-name ()
  (string-replace "\n" "" (papis--cmd "config notes-name" t)))

(defun papis--notes-path (doc)
  "Return the notes path to the given document.
   This does not make sure that the notes file exists,
   it just gets a path that hsould be there."
  (let ((query (papis-id-query doc)))
    (papis--cmd (format "list --notes %s"
                        query)
                t)))

(defun papis--ensured-notes-path (doc)
  (let ((maybe-notes (papis-doc-get doc "notes"))
        (id-query (papis-id-query doc)))
    (unless maybe-notes
      (setq maybe-notes (papis--default-notes-name))
      ;; will this work on windows? someone cares?
      (papis--cmd (format "edit --notes --editor echo %s" id-query)))
    (string-replace "\n" ""
                    (papis--cmd (format "list --notes %s" id-query)
                                t))))

(defun papis-notes (doc &optional run-hook)
  "Open or create notes for a document DOC.

Whenever RUN-HOOK is non-nil, the hook for the notes will be ran."
  (interactive (list (papis--read-doc)
                     current-prefix-arg))
  (let ((has-notes-p (papis-doc-get doc "notes")))
    (let ((notes-path (papis--ensured-notes-path doc)))
      (when (or (not has-notes-p) run-hook)
        (with-current-buffer (find-file notes-path)
          (run-hook-with-args 'papis-edit-new-notes-hook
                              doc)))
      (find-file notes-path))))

;;;; Editing Papis info files

(define-minor-mode papis-edit-mode
    "General mode for editing papis files"
  :keymap `((,(kbd "C-c C-c") .
              ,(defun papis-edit-update-cache (folder)
                 (interactive (list default-directory))
                 (message "Updating the cache for %s" folder)
                 (papis--cmd (format "cache update --doc-folder %s"
                                     folder))))))

(defun papis-edit (doc)
  (interactive (list (papis--read-doc)))
  (let* ((folder (papis-doc-get-folder doc))
         (info (concat folder "/" "info.yaml")))
    (find-file info)
    (papis-edit-mode)))

;;;; Document completion from the minibuffer

(defun papis-default-read-format-function (doc)
  `(
    ,(format "%s\n\t%s\n\t«%s» +%s %s"
             (papis-doc-get doc "title")
             (papis-doc-get doc "author")
             (papis-doc-get doc "year")
             (or (papis-doc-get doc "tags") "")
             (let ((n (papis-doc-get doc "_note"))) (if n (concat ":note " n) "")))
    .
    ,doc))


(defun papis--org-looking-at-link ()
  (when (eq major-mode 'org-mode)
    (let* ((context (org-element-lineage (org-element-context)
                                         '(link)
                                         t))
           ;; (type (org-element-type context))
           (papis-id (org-element-property :path context)))
      papis-id)))

(defun papis-from-id (papis-id)
  (let* ((query (format "papis_id:%s" papis-id))
         (results (papis--query-documents query)))
    (pcase (length results)
      (0 (error "No documents found with papis_id '%s'"
                papis-id))
      (1 (car results))
      (_ (error "Too many documents (%d) found with papis_id '%s'"
                (length results) papis-id)))))

(defun papis--read-doc (&optional force-query)
  (cond
    ;; if in org mode and in org link, return it
    ((and (not force-query)
          (papis--org-looking-at-link))
     (papis-from-id (papis--org-looking-at-link)))
    ((and (not force-query)
          (when-let* ((filename (buffer-file-name (current-buffer)))
                      (dirname (file-name-directory filename))
                      (yaml.info (file-name-concat dirname "info.yaml")))
            (when (file-exists-p yaml.info)
              (car (papis--query-documents nil dirname))))))
    ((and (not force-query)
          (let* ((results (papis--query-documents (read-string papis--query-prompt
                                                                 nil 'papis)))
                 (formatted-results (mapcar papis-read-format-function results)))
            (cdr (assoc
                  (completing-read "Select an entry: " formatted-results)
                  formatted-results)))))))

;;;; Org-mode hyperlinks for Papis

(require 'ol-doi)
(org-link-set-parameters "papis"
                         :follow (lambda (papis-id)
                                   (papis-open (papis-from-id papis-id)))
                         :export #'ol-papis-export
                         :complete (lambda (&optional _arg)
                                     (format "papis:%s"
                                             (papis-doc-get (papis--read-doc)
                                                            "papis_id")))
                         :insert-description
                         (lambda (link _desc)
                           (let* ((papis-id (string-replace "papis:"  "" link))
                                  (doc (papis-from-id papis-id)))
                             (papis-doc-get doc "title"))))

(defun ol-papis-export (papis-id description format info)
  (let* ((doc (papis-from-id papis-id))
         (doi (papis-doc-get doc "doi"))
         (_url (papis-doc-get doc "url")))
    (cond
      (doi (org-link-doi-export doi description format info)))))

;; Paper sections

(defun papis-org-insert-heading (doc)
  (interactive (list (papis--read-doc)))
  (let ((title (papis-doc-get doc "title"))
        (author (papis-doc-get doc "author"))
        (year (papis-doc-get doc "year"))
        (doi (papis-doc-get doc "doi"))
        (papis-id (papis-doc-get doc "papis_id")))
    (org-insert-heading)
    (insert (format "[[papis:%s][%s]]" papis-id title))
    (org-set-property "PAPIS_ID" papis-id)
    (org-set-property "AUTHOR" author)
    (org-set-property "TITLE" title)
    (org-set-property "YEAR" (format "%s" year))
    (org-set-property "DOI" doi)))

;;;; org-ref

(defun papis-org-ref-get-pdf-filename (key)
    (interactive)
    (let* ((docs (papis--query-documents (format "ref:'%s'" key)))
           (doc (car docs))
           (files (papis--get-file-paths doc)))
      (pcase (length files)
        (1 (car files))
        (_ (completing-read "" files)))))

;; Citations
;; In general it is recommended to use the citation mechanisms of
;; =org-ref=, however, if for some reason you would like to cite
;; directly from =papis=, you can use the function

(defun papis-insert-citation (doc)
  (interactive (list (papis--read-doc)))
  (let* ((ref (papis--get-ref doc)))
    (if (fboundp 'citar-insert-citation)
        (citar-insert-citation (list ref))
      (insert (format "[cite:@%s]" ref)))))

;;;; Dynamic block to tangle a .bib from all references in an org file

(defun papis-org-list-keys ()
  "List citation keys in the org buffer."
  (let ((org-tree (org-element-parse-buffer)))
    (delete-dups
     (org-element-map org-tree 'citation-reference
       (lambda (r) (org-element-property :key r))
       org-tree))))

(defun papis-exec (python-file &optional arguments)
  (let ((fmt "exec %s %s"))
    (papis--cmd (format fmt
                        python-file
                        (or arguments ""))
                t)))

(defvar papis--refs-to-bibtex-script
"
import argparse
import papis.api
from papis.bibtex import to_bibtex

parser = argparse.ArgumentParser(formatter_class=argparse.RawTextHelpFormatter,
                                 description='')
parser.add_argument('refs', help='References', action='store', nargs='*')
args = parser.parse_args()

docs = []

for ref in args.refs:
    docs.extend(papis.api.get_documents_in_lib(library=None, search=ref))

for d in docs:
    print(to_bibtex(d))
")

(defun papis--refs-to-bibtex (refs)
  (let ((py-script (make-temp-file "papis-bibtex-script" nil ".py")))
    (with-temp-buffer
      (insert papis--refs-to-bibtex-script)
      (write-file py-script))
    (papis-exec py-script (string-join refs " "))))

(defun papis-create-papis-bibtex-refs-dblock (bibfile)
  (insert (format "#+begin: papis-bibtex-refs :tangle %s" bibfile))
  (insert "\n")
  (insert "#+end:"))

(defun papis-extract-citations-into-dblock (&optional bibfile)
  (interactive)
  (if (org-find-dblock "papis-bibtex-refs")
      (progn
        (org-fold-show-entry)
        (org-update-dblock))
    (papis-create-papis-bibtex-refs-dblock
     (or bibfile (read-file-name "Bib file: " nil "main.bib")))))

(defun org-dblock-write:papis-bibtex-refs (params)
  (let ((tangle-file (or (plist-get params :tangle)
                         (buffer-file-name)))
        (exports ":exports none"))
    (insert
     (format "#+begin_src bibtex %s :tangle %s\n"
             exports
             tangle-file)))
  (let* ((refs (papis-org-list-keys))
         (queries (mapcar (lambda (r) (format "ref:\"%s\"" r))
                          refs)))
    (insert (papis--refs-to-bibtex queries)))
  (insert "#+end_src\n"))

(provide 'papis)

;;; papis.el ends here
