;;; papis-test-common.el --- Common testing functions -*- lexical-binding: nil -*-

;;; Code:

(require 'papis)

;; If you want to use the example library in emacs, set the variables
;; below and make sure that the default-directory points to the
;; repository's root

` (setq papis-extra-options '("-c" "./papis.config")
        papis-library "example-lib"
        papis-export-bibtex-file (make-temp-file "example-lib" nil ".bib")
        org-cite-global-bibliography (list papis-export-bibtex-file)
        org-cite-insert-processor 'basic)  ; <- C-x C-e there

;; For tests, we prefer a fixture that sets the variables temporarily

(defmacro papis-do-with-config (&rest body)
  "Run the forms of BODY with Papis configured for testing."
  `(let ;; dynamic bindings
       ((papis-extra-options '("-c" "./papis.config"))
        (papis-library "example-lib"))
     ,@body))

(defmacro papis-do-with-org-cite-config (&rest body)
  "Run the forms of BODY with Papis and org-cite configured for testing."
  `(let* ;; dynamic bindings
       ((papis-extra-options '("-c" "./papis.config"))
        (papis-library "example-lib")
        (papis-export-bibtex-file (make-temp-file "example-lib" nil ".bib"))
        (org-cite-global-bibliography (list papis-export-bibtex-file))
        (org-cite-insert-processor 'basic))
     (papis-export-bibtex)
     ,@body
     (delete-file papis-export-bibtex-file)))

(provide 'papis-test-common)

;;; papis-test-common.el ends here
