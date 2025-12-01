;;; papis-test.el --- Tests for papis.el -*- lexical-binding: nil -*-

;; Copyright (C) 2025 Jean-Alexandre Barszcz

;;; Code:

(require 'papis-test-common)

(ert-deftest papis-test-example-lib-has-its-docs ()
  "Test that Papis uses the example lib by checking for a specific doc."
  :tags '(runs-papis uses-example-library)
  (papis-do-with-config
   (should (member "example-lib/russell1920introduction"
                   (string-lines
                    (papis--run-to-string (list "list" "--all")))))))

(provide 'papis-test)

;;; papis-test.el ends here
