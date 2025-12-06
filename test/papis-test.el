;;; papis-test.el --- Tests for papis.el -*- lexical-binding: nil -*-

;; Copyright (C) 2025 Jean-Alexandre Barszcz

;;; Code:

(require 'papis-test-common)
(require 'el-mock)

(ert-deftest papis-test-example-lib-has-its-docs ()
  "Test that Papis uses the example lib by checking for a specific doc."
  :tags '(runs-papis uses-example-library)
  (papis-do-with-config
   (should (member "example-lib/russell1920introduction"
                   (string-lines
                    (papis--run-to-string (list "list" "--all")))))))

(ert-deftest papis-test-queries-version-once ()
  "`papis-check-program' sets the version and only runs Papis once."
  :tags '(runs-papis)
  (papis-do-with-config
   (let ;; dynamic bindings
       ((papis-program-version nil)
        (papis-skip-program-check nil))
     (should (papis-check-program))
     (should papis-program-version)
     (should (with-mock
              (not-called call-process)
              (papis-check-program))))))

(ert-deftest papis-test-check-program-minimum-version ()
  "`papis-check-program' errors when requiring a large min version."
  (with-mock
   (not-called call-process) ; Don't run Papis; set version manually
   (let ;; dynamic bindings
       ((papis-program-version (version-to-list "0.14.1"))
        (papis-skip-program-check nil))
     (should (papis-check-program "0.11"))
     (should-error (papis-check-program "1000000")))))

(ert-deftest papis-test-check-program-errors-without-program ()
  (let ;; dynamic bindings
      ((papis-program "non-existent-program"))
    (should-error (papis-check-program))))

(defun papis-test-export-json ()
  "Helper for the characterization the Papis export output."
  (papis-do-with-config
   (papis--run-to-string '("export" "--format" "json" "--all"))))

(defconst papis-test-golden-json
  (progn
    ` (let ; example-lib's path is relative to the repo root
          ((default-directory (expand-file-name "..")))
        (print (papis-test-export-json) ; C-x C-e after next line
               (current-buffer)))
"[
  {
    \"_papis_local_folder\": \"example-lib/newton1687philosophiae\",
    \"author\": \"Newton, Isaac\",
    \"author_list\": [
      {
        \"family\": \"Newton\",
        \"given\": \"Isaac\"
      }
    ],
    \"files\": [
      \"newton1687philosophiae.pdf\"
    ],
    \"papis_id\": \"d6aa5c0850042fb30753c2dfae8ac9ad\",
    \"publisher\": \"William Dawson \\\\& Sons\",
    \"ref\": \"newton1687philosophiae\",
    \"time-added\": \"2025-12-03-23:53:24\",
    \"title\": \"Philosophiae naturalis principia mathematica\",
    \"type\": \"book\",
    \"url\": \"https://www.gutenberg.org/ebooks/28233\",
    \"year\": \"1687\"
  },
  {
    \"_papis_local_folder\": \"example-lib/russell1920introduction\",
    \"author\": \"Russell, Bertrand\",
    \"author_list\": [
      {
        \"family\": \"Russell\",
        \"given\": \"Bertrand\"
      }
    ],
    \"files\": [
      \"russell1920introduction.epub\"
    ],
    \"lccn\": \"25003630\",
    \"notes\": \"notes.org\",
    \"papis_id\": \"0b95c29305e4a0489922524c8852ed9a\",
    \"publisher\": \"Allen \\\\& Unwin\",
    \"ref\": \"russell1920introduction\",
    \"series\": \"Library of philosophy\",
    \"time-added\": \"2025-12-03-23:41:12\",
    \"title\": \"Introduction to Mathematical Philosophy\",
    \"type\": \"book\",
    \"url\": \"https://www.gutenberg.org/ebooks/28233\",
    \"year\": \"1920\"
  }
]"
  )
  "The string resulting from a JSON export of the example library.

Update this when it changes. It serves as a form of characterization
test, and also makes it possible to run a few tests without querying
Papis.")

(defun papis-test-normalize-json-result (object)
  "Normalize the results from JSON parsing to compare them with `equal'.

In particular, recurse down the OBJECT to replace hashtables with sorted
assoc lists."
  (cond ((hash-table-p object)
         (sort (map-apply (lambda (k v)
                            (cons k (papis-test-normalize-json-result v)))
                          object)))
        ((proper-list-p object)
         (mapcar #'papis-test-normalize-json-result object))
        (t object)))

(defun papis-test-normalize-docs (docs)
  "Normalize the set of DOCS by normalizing each and sorting them."
  (sort (mapcar #'papis-test-normalize-json-result docs)))

(defun papis-test-query-documents-fake ()
  "Get the set of documents from `papis-test-golden-json'.

Serves to fake `papis--query-documents' without calling Papis."
  (json-parse-string papis-test-golden-json
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(ert-deftest papis-test-check-golden-library-documents ()
  "Check that variable `papis-test-golden-json' is current."
  :tags '(runs-papis uses-example-library)
  (papis-do-with-config
   (should (equal (papis-test-normalize-docs (papis-test-query-documents-fake))
                  (papis-test-normalize-docs (papis--query-documents))))))

(provide 'papis-test)

;;; papis-test.el ends here
