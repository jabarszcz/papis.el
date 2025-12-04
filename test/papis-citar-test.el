;;; papis-citar-test.el --- Tests for papis-citar.el -*- lexical-binding: nil -*-

;; Copyright (C) 2025 Jean-Alexandre Barszcz

;;; Code:

(require 'el-mock)
(require 'papis-citar)
(require 'papis-test-common)

(defmacro papis-citar-do-with-config (&rest body)
  "Run the forms of BODY with Papis and Citar configured for testing."
  `(progn
     (citar-register-notes-source 'papis-citar
                                  papis-citar-notes-config)
     (papis-do-with-org-cite-config
      (let ;; Dynamic bindings
          ((citar-note-format-function #'papis-citar-note-format-default)
           (citar-notes-source 'papis-citar)
           (citar-file-sources (list papis-citar-files-config))
           (citar-bibliography (list papis-export-bibtex-file)))
        ,@body))))

(ert-deftest papis-citar-test-has-notes ()
  (papis-citar-do-with-config
   (let ((has-notes-p (citar-has-notes)))
     (should     (funcall has-notes-p "russell1920introduction"))
     (should-not (funcall has-notes-p "newton1687philosophiae")))))

(ert-deftest papis-citar-test-get-notes ()
  (papis-citar-do-with-config
   (let ((notes-table (citar-get-notes))
         (nek 'non-existent-key))
     (should (equal nek
                    (gethash "newton1687philosophiae" notes-table nek)))
     (should (equal '("example-lib/russell1920introduction/notes.org")
                    (gethash "russell1920introduction" notes-table nek))))))

(ert-deftest papis-citar-test-open-note ()
  :tags '(runs-papis uses-example-library)
  (defvar papis-citar-test-notes-hook-new)
  (papis-citar-do-with-config
   (let* ;; Dynamic bindings
       ((papis-after-open-note-functions
         (lambda (_doc new)
           (setq papis-citar-test-notes-hook-new new)))
        (notes-table (citar-get-notes))
        (notes-file (car (gethash "russell1920introduction" notes-table))))
     (save-window-excursion
       (citar-open-note notes-file)
       (should-not papis-citar-test-notes-hook-new)
       (should (equal (buffer-name (current-buffer)) "notes.org"))
       (kill-buffer)))))

(ert-deftest papis-citar-test-create-note ()
  :tags '(runs-papis uses-example-library)
  (defvar papis-citar-test-notes-hook-new)
  (papis-citar-do-with-config
   (let* ;; Dynamic bindings
       ((papis-after-open-note-functions
         (lambda (_doc new)
           (setq papis-citar-test-notes-hook-new new)))
        (citekey "newton1687philosophiae"))
     (save-window-excursion
       (citar-create-note citekey)
       (should papis-citar-test-notes-hook-new)
       (should (equal (buffer-name (current-buffer)) "notes.org"))
       (kill-buffer))
     ;; Teardown
     (papis--run (list "rm" "--notes" "--force" citekey)))))

(ert-deftest papis-citar-test-open-files ()
  :tags '(runs-papis uses-example-library)
  (papis-citar-do-with-config
   (let ((newton-pdf
          "example-lib/newton1687philosophiae/newton1687philosophiae.pdf"))
     (with-mock
      (mock (find-file (expand-file-name newton-pdf)) :times 1)
      (citar-open-files "newton1687philosophiae")))))

(provide 'papis-citar-test)

;;; papis-citar-test.el ends here
