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

(defcustom papis-completion-format-function
  #'papis-default-completion-format-function
  "Function taking a papis document (hashmap) and outputing a
   string representation of it to be fed into `completing-read'."
  :type 'function
  :group 'papis)

(defvar-keymap papis-command-map
  :doc "Keymap for papis commands"
  "a" #'papis-add
  "b" #'papis-browse
  "e" #'papis-edit
  "n" #'papis-notes
  "o" #'papis-open
  "u" #'papis-cache-update)

(defcustom papis-after-open-note-functions
  #'papis-after-open-note-default
  "An abnormal hook to run after a note is opened.

Abnormal because it must accept arguments: NEW which is non-nil when the
note has just been created, and DOC which contains the hashtable
describing the document.

For example, it can be used to move the cursor to a specific place in
note templates."
  :type 'hook
  :group 'papis)

(defun papis-after-open-note-default (&optional _new)
  "Move point after the first occurence of \"TODO\" in the note."
  (search-forward "TODO"
                  nil ; Don't limit the search
                  t   ; Don't error when not found
                  ))

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

;;;###autoload
(defun papis--run-term (&optional args skip-confirmation)
  "Run the papis program with arguments ARGS in term-mode.

ARGS is the list of arguments passed to the Papis program (sub-command
included).

Confirm the shell command in the minibuffer unless SKIP-CONFIRMATION is
non-nil."
  (interactive)
  (let ((cmdstr (papis--cmd-str args)))
    (if skip-confirmation
        (message cmdstr)
      (setq cmdstr
            (read-shell-command "Run papis (like this): " cmdstr)))
    (term cmdstr)))

;;;; Checking the papis program and its version

(defvar papis-program-version nil
  "Papis program version (as a list), if known. See `papis-check-program'.")

;;;###autoload
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

;;;; Private Papis documents accessors

(defun papis--doc-get (doc key &optional default)
  "Return the value associated with KEY in DOC's metadata, or DEFAULT."
  (gethash key doc default))

(defun papis--doc-folder (doc)
  "Return DOC's directory (relative to the library root)."
  (papis--doc-get doc "_papis_local_folder"))

(defun papis--doc-ref (doc)
  "Return DOC's ref (the citation key)."
  (papis--doc-get doc "ref"))

(defun papis--doc-id (doc)
  "Return DOC's id in Papis."
  (papis--doc-get doc "papis_id"))

(defun papis--doc-notes-path (doc)
  "Return the path to the note file associated with DOC, if it exists."
  (and-let* ((dir (papis--doc-folder doc))
             (file (papis--doc-get doc "notes"))
             (path (file-name-concat dir file)))
    (and (file-exists-p path) path)))

(defun papis--doc-file-paths (doc)
  "Return the paths to existing library files associated with DOC."
  (and-let* ((dir (papis--doc-folder doc))
             (files (papis--doc-get doc "files")))
    (mapcan (lambda (file)
              (let ((path (file-name-concat dir file)))
                (and (file-exists-p path) (list path))))
            files)))

(defun papis--doc-query (doc)
  "Return a string that serves as a query to select DOC in Papis commands."
  (format "papis_id:%s" (papis--doc-id doc)))

;;;; Document completion from the minibuffer

(defun papis-default-completion-format-function (doc)
  (format "%4s %-30.30s: %s"
          (papis--doc-get doc "year")
          (papis--doc-get doc "author")
          (papis--doc-get doc "title")))

(defun papis--org-looking-at-link ()
  "Return the id of the Papis link at point, when in org-mode, or nil."
  (when (eq major-mode 'org-mode)
    (let* ((context (org-element-lineage (org-element-context)
                                         '(link)
                                         t))
           (papis-id (org-element-property :path context)))
      papis-id)))

(defun papis--org-looking-at-ref ()
  "Return the org-cite reference at point, when in org-mode, or nil."
  (when (eq major-mode 'org-mode)
    (let ((context (org-element-lineage (org-element-context)
                                        '(citation-reference)
                                        t)))
      (org-element-property :key context))))

(defun papis--format-candidate (doc)
  (funcall papis-completion-format-function doc))

(defvar papis--doc-history nil
  "The history variable for Papis document input. See `papis--read-doc'.")

(defun papis--completing-read (prompt candidates &optional default)
  "Wrap `completing-read', return the matching value from alist CANDIDATES."
  (let* ((formatted-candidates-alist
          (mapcar (lambda (doc) (cons (papis--format-candidate doc) doc))
                  candidates))
         (formatted-default
          (and default (papis--format-candidate default)))
         (predicate nil) (require-match t) (initial-input nil)
         (completed-str (completing-read prompt formatted-candidates-alist
                                         predicate require-match initial-input
                                         'papis--doc-history
                                         formatted-default)))
    (cdr (assoc completed-str formatted-candidates-alist))))

(defun papis--read-doc ()
  "Let the user choose a document with `completing-read'.

The default document (selected with RET) is set contextually (using the
link/ref at point or the current directory)."
  (let* ((docs (papis--query-documents))
         (papis-id (papis--org-looking-at-link)) ; Point on Papis link?
         (ref (papis--org-looking-at-ref)) ; Point on org-cite ref?
         (match-current-p
          (lambda (doc)
            (or
             ;; Point is on a link to this document
             (and papis-id (equal (papis--doc-id doc) papis-id))
             ;; Point is on a citation to this document
             (and ref (equal (papis--doc-ref doc) ref))
             ;; The current directory is doc's folder
             (file-equal-p (papis--doc-folder doc) default-directory)))))
    (if-let* ((default (seq-find match-current-p docs))
              (formatted-default (papis--format-candidate default))
              (prompt (format "Choose document (default %s): "
                              formatted-default)))
        (papis--completing-read prompt docs default)
      (papis--completing-read "Choose document: " docs))))

;;;; Public Papis commands

;;;###autoload
(defun papis-browse (doc)
  (interactive (list (papis--read-doc)))
  (let ((url
         (cond
           ((papis--doc-get doc "url"))
           ((when-let ((doi (papis--doc-get doc "doi")))
              (format "https://doi.org/%s" doi)))
           (t (error "Neither url nor doi found in this document.")))))
    (browse-url url)))

;;;###autoload
(defun papis-open (doc)
  (interactive (list (papis--read-doc)))
  (let* ((files (papis--doc-file-paths doc))
         (file (pcase (length files)
                 (1 (car files))
                 (0 (error "Doc has no files"))
                 (_ (completing-read "file: " files)))))
    (find-file-other-window file)))

;;;###autoload
(defun papis-add (&optional url)
  (interactive (list (thing-at-point 'url)))
  (papis--run-term (list "add" url)))

;;;; Notes

(defun papis--ensured-notes-path (query)
  "Return the path to the (new) note file for the doc referenced by QUERY.

Let Papis create the note file at the appropriate path from the note
template if it doesn't exist."
  (papis--run-to-string
   ;; will this work on windows?
   (list "edit" "--notes" "--editor" "echo" query)))

;;;###autoload
(defun papis-notes (doc)
  "Open, and create if necessary, the notes file for a document DOC.

See `papis-after-open-note-functions' for customization."
  (interactive (list (papis--read-doc)))
  (let ((has-notes (papis--doc-notes-path doc)))
    (let* ((doc-query (papis--doc-query doc))
           (notes-path (papis--ensured-notes-path doc-query)))
      (find-file notes-path)
      (run-hook-with-args 'papis-after-open-note-functions
                          doc
                          (not has-notes)))))

;;;; Editing Papis info files

;;;###autoload
(defun papis-cache-update (folder)
  "Update Papis' cache for FOLDER, or the whole database if FOLDER is nil.

When called interactively, FOLDER is taken from variable
`default-directory', unless a prefix argument is used to force updating
the whole cache."
  (interactive (list (unless current-prefix-arg default-directory)))
  (papis-check-program "0.14")
  (let ((folder-args (when folder (list "--doc-folder" folder))))
    (papis--run (append '("cache" "update") folder-args))))

;;;###autoload
(define-minor-mode papis-edit-mode
  "Mode for editing papis metadata files."
  :keymap `((,(kbd "C-c C-c") . ,#'papis-cache-update)))

;;;###autoload
(defun papis-edit (doc)
  (interactive (list (papis--read-doc)))
  (let* ((folder (papis--doc-folder doc))
         (info (concat folder "/" "info.yaml")))
    (find-file info)
    (papis-edit-mode)))

;;;; Org-mode hyperlinks for Papis

(defun papis-from-id (papis-id)
  (let* ((query (format "papis_id:%s" papis-id))
         (results (papis--query-documents query)))
    (pcase (length results)
      (0 (error "No documents found with papis_id '%s'"
                papis-id))
      (1 (car results))
      (_ (error "Too many documents (%d) found with papis_id '%s'"
                (length results) papis-id)))))

(require 'ol-doi)
(org-link-set-parameters "papis"
                         :follow (lambda (papis-id)
                                   (papis-open (papis-from-id papis-id)))
                         :export #'ol-papis-export
                         :complete (lambda (&optional _arg)
                                     (format "papis:%s"
                                             (papis--doc-get (papis--read-doc)
                                                            "papis_id")))
                         :insert-description
                         (lambda (link _desc)
                           (let* ((papis-id (string-replace "papis:"  "" link))
                                  (doc (papis-from-id papis-id)))
                             (papis--doc-get doc "title"))))

(defun ol-papis-export (papis-id description format info)
  (let* ((doc (papis-from-id papis-id))
         (doi (papis--doc-get doc "doi"))
         (_url (papis--doc-get doc "url")))
    (cond
      (doi (org-link-doi-export doi description format info)))))

;; Paper sections

;;;###autoload
(defun papis-org-insert-heading (doc)
  (interactive (list (papis--read-doc)))
  (let ((title (papis--doc-get doc "title"))
        (author (papis--doc-get doc "author"))
        (year (papis--doc-get doc "year"))
        (doi (papis--doc-get doc "doi"))
        (papis-id (papis--doc-id doc)))
    (org-insert-heading)
    (insert (format "[[papis:%s][%s]]" papis-id title))
    (org-set-property "PAPIS_ID" papis-id)
    (org-set-property "AUTHOR" author)
    (org-set-property "TITLE" title)
    (org-set-property "YEAR" (format "%s" year))
    (org-set-property "DOI" doi)))

;;;; org-ref

(defun papis-org-ref-get-pdf-filename (key)
    (let* ((docs (papis--query-documents (format "ref:'%s'" key)))
           (doc (car docs))
           (files (papis--doc-file-paths doc)))
      (pcase (length files)
        (1 (car files))
        (_ (completing-read "" files)))))

;; Citations
;; In general it is recommended to use the citation mechanisms of
;; =org-ref=, however, if for some reason you would like to cite
;; directly from =papis=, you can use the function

(defun papis-insert-citation (doc)
  (interactive (list (papis--read-doc)))
  (let* ((ref (papis--doc-ref doc)))
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

(defun papis--exec (python-file &optional arguments)
  (papis-check-program "0.12")
  (papis--run-to-string (list "exec" python-file arguments)))

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
    (papis--exec py-script refs)))

(defun papis-create-papis-bibtex-refs-dblock (bibfile)
  (insert (format "#+begin: papis-bibtex-refs :tangle %s" bibfile))
  (insert "\n")
  (insert "#+end:"))

;;;###autoload
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
