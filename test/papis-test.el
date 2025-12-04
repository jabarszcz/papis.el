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

(provide 'papis-test)

;;; papis-test.el ends here
