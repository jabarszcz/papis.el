;;; papis-test-common.el --- Common testing functions -*- lexical-binding: nil -*-

;;; Code:

(require 'papis)

(defmacro papis-do-with-config (&rest body)
  "Run the forms of BODY with Papis configured for testing."
  `(let ;; dynamic bindings
       ((papis-extra-options '("-c" "./papis.config"))
        (papis-library "example-lib"))
     ,@body))

(provide 'papis-test-common)

;;; papis-test-common.el ends here
