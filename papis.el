(require 'ol)
(require 'json)

(defgroup papis nil
  "Official Papis package for Emacs."
  :group 'external
  :prefix "papis-"
  :link '(url-link :tag "Github"
          "https://github.com/papis/papis.el"))

;; Variables

(defcustom papis-binary-path
  "papis"
  "The binary path for papis.
   You might have papis installed for instance in some
   virtual environment"
  :type 'string
  :group 'papis)

(defcustom papis-read-format-function
  #'papis-default-read-format-function
  "Function taking a papis document (hashmap) and outputing a
   string representation of it to be fed into the reader."
  :group 'papis)

(defcustom papis--query-prompt
  "Papis Query: "
  "The prompt to show users in order to accept a query
  "
  :type 'string
  :group 'papis)

(defcustom papis-library
  nil
  "papis library to be used in commands.
   If it is set to nil then the default library of your system will
   be used.
  "
  :type 'string
  :group 'papis)

;; Document

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

;; Issuing commands to the shell
;;  The main interface with papis commands will be =papis--cmd=
;;  which is a function intended for library writers.

(cl-defun papis--cmd (cmd &optional with-stdout)
  "Helping function to run papis commands"
  (let* ((lib-flags (if papis-library
                        (concat "-l " papis-library)
                      ""))
         (sys (if with-stdout
                  #'shell-command-to-string
                #'shell-command))
         (full-cmd (format "%s %s %s" papis-binary-path lib-flags cmd)))
    (message full-cmd)
    (funcall sys
             full-cmd)))

;; papis-query

;; A papis document object is represented in =papis.el=
;; as a =hashtable=, and the command that turns a query
;; into a list of hashtables is =papis-query=.
;; This is done via the papis' =json= exporter, i.e.,
;; we query python and get a json document with the documents that
;; emacs reads in.

(defun papis--json-string-to-documents (json-file)
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string))
    (json-read-from-string json-file)))

(cl-defun papis-query (&key query id doc-folder)
  "Make a general papis query:
   it returns a list of hashtables where every hashtable is a papis document"
  (when id
    (setq query (papis-id-query id)))
  (papis--json-string-to-documents (papis-json :query query
                                               :doc-folder doc-folder)))
;; papis-open

;; The cornerstone of papis is opening documents, in emacs
;; the command is also available:

(cl-defun papis--update (&key id doc-folder alist)
  (let (sets)
    (dolist (pair alist)
      (push (format "--set %s %S" (car pair) (cdr pair))
            sets))
    (papis--cmd (format "update %s %s"
                        (string-join sets " ")
                        (if doc-folder
                            (format "--doc-folder %S" doc-folder)
                          (format "papis_id:%s" id))))))

(defun papis-browse (doc)
  (interactive (list (papis--read-doc)))
  (let ((url
         (cond
           ((papis-doc-get doc "url" nil))
           ((when-let ((doi (papis-doc-get doc "doi" nil)))
              (format "https://doi.org/%s" doi))
            (t (error "Neither url nor doi found in this document."))))))
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

;; Notes

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
  """
  Create notes for a document or open the note
  DOC is the papis document.
  Whenever RUN-HOOK is non-nil, the hook for the notes
  will be ran.
  """
  (interactive (list (papis--read-doc)
                     current-prefix-arg))
  (let ((has-notes-p (papis-doc-get doc "notes")))
    (let ((notes-path (papis--ensured-notes-path doc)))
      (when (or (not has-notes-p) run-hook)
        (with-current-buffer (find-file notes-path)
          (run-hook-with-args 'papis-edit-new-notes-hook
                              doc)))
      (find-file notes-path))))

;; papis-edit

(define-minor-mode papis-edit-mode
    "General mode for editing papis files"
  :keymap `((,(kbd "C-c C-c") .
              ,(defun papis-edit-update-cache (folder)
                 (interactive (list default-directory))
                 (message "Updating the cache for %s" folder)
                 (papis--cmd (format "cache update --doc-folder %s"
                                     folder)))))

  (defvar-local papis-edit-mode-id nil))

(defun papis-edit (doc)
  (interactive (list (papis--read-doc)))
  (let* ((folder (papis-doc-get-folder doc))
         (info (concat folder "/" "info.yaml")))
    (find-file info)
    (papis-edit-mode)))

;; papis-exec

(defun papis-exec (python-file &optional arguments)
  (let ((fmt "exec %s %s"))
    (papis--cmd (format fmt
                        python-file
                        (or arguments ""))
                t)))

;; papis-export

(progn
  (defmacro papis--make-exporter (format-name)
    `(cl-defun ,(intern (format "papis-%s" format-name))
         (&key query doc-folder)
       (let ((outfile (make-temp-file "papis-")))
         (papis--cmd (format "export --all --format %s %s -o %s"
                             ,(symbol-name format-name)
                             (if doc-folder (format "--doc-folder %S" doc-folder)
                               (format "%S" query))
                             outfile))
         (with-current-buffer (find-file-noselect outfile)
           (prog1 (buffer-string)
             (kill-buffer))))))

  (papis--make-exporter bibtex)
  (papis--make-exporter yaml)
  (papis--make-exporter typist)
  (papis--make-exporter json))

;; Document reader

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
         (results (papis-query :query query)))
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
          (let* ((filename (buffer-file-name (current-buffer)))
                 (dirname (file-name-directory filename))
                 (yaml.info (file-name-concat dirname "info.yaml")))
            (when (file-exists-p yaml.info)
              (car (papis-query :doc-folder dirname))))))
    ((and (not force-query)
          (let* ((results (papis-query :query (read-string papis--query-prompt
                                                           nil 'papis)))
                 (formatted-results (mapcar papis-read-format-function results)))
            (cdr (assoc
                  (completing-read "Select an entry: " formatted-results)
                  formatted-results)))))))

;; papis

(require 'ol-doi)
(org-link-set-parameters "papis"
                         :follow (lambda (papis-id)
                                   (papis-open (papis-from-id papis-id)))
                         :export #'ol-papis-export
                         :complete (lambda (&optional arg)
                                     (format "papis:%s"
                                             (papis-doc-get (papis--read-doc)
                                                            "papis_id")))
                         :insert-description
                         (lambda (link desc)
                           (let* ((papis-id (string-replace "papis:"  "" link))
                                  (doc (papis-from-id papis-id)))
                             (papis-doc-get doc "title"))))

(defun ol-papis-export (papis-id description format info)
  (let* ((doc (papis-from-id papis-id))
         (doi (papis-doc-get doc "doi"))
         (url (papis-doc-get doc "url")))
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

(defun papis-org-ref-get-pdf-filename (key)
    (interactive)
    (let* ((docs (papis-query (format "ref:'%s'" key)))
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

;; and we will need also a way of listing all the keys of the document
;; for further functions. I took this from the good =citar= package

(defun papis-org-list-keys ()
  "List citation keys in the org buffer."
  (let ((org-tree (org-element-parse-buffer)))
    (delete-dups
     (org-element-map org-tree 'citation-reference
       (lambda (r) (org-element-property :key r))
       org-tree))))

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
    (papis-exec py-script (s-join " " refs))))

;; The =papis-bibtex-refs= dynamic block

(defun papis-create-papis-bibtex-refs-dblock (bibfile)
  (insert (format "#+begin: papis-bibtex-refs :tangle %s" bibfile))
  (insert "\n")
  (insert "#+end:"))

(defun papis-extract-citations-into-dblock (&optional bibfile)
  (interactive)
  (if (org-find-dblock "papis-bibtex-refs")
      (progn
        (org-show-entry)
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
