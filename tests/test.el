;;; test.el --- ERT Tests for flycheck-android-lint

;; Copyright (C) 2018 Andreas Marschke

;; Author: Andreas Marschke <emacs@andreas-marschke.name>
;; Created: 12.07.2018
;; Version: 0.1
;; Package-Requires: (())
;; Keywords: 
;; URL: https://github.com/andreas-marschk/flycheck-android-lint
;;; Code:

(require 'flycheck-android-lint)
(require 'dash)
(require 'ert)

(defmacro flycheck-android-lint-test-compare-with-basic (&rest body)
  (declare (indent 0) (debug (&rest form)))
  `(let ((expected '((
                      (id . "UseValueOf")
                      (message . "Use `Integer.valueOf(paramList.get(\"id\"))` instead")
                      (category . "Performance")
                      (priority . 4)
                      (summary . "Should use `valueOf` instead of `new`")
                      (explanation . "This is a longer explanation")
                      (errorLine1 . "Integer oid = new Integer(paramList.get(\"id\"));")
                      (errorLine2 . "                  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
                      (file . "src/com/example/app/test/AppTest.java")
                      (line . 10)
                      (column . 10)))))
     ,@body))

(defun values-in-cons (collection index key)
  "Get the value of a cons cell in a nested list of cons cells"
  (cdr (assq key (nth index collection))))

(ert-deftest flycheck-android-lint-parse-values-from-basic-xml()
  (let ((content (with-temp-buffer (insert-file-contents "fixtures/basic.xml") (buffer-string))))
    (flycheck-android-lint-test-compare-with-basic
      (let* ((parsed (flycheck-android-lint-get-issues content)))
        
        (dolist (k '(id message category priority summary explanation errorLine1 errorLine2 file line column))
          (should (equal (values-in-cons expected 0 'k)
                         (values-in-cons parsed 0 'k))))))))

(ert-deftest flycheck-android-lint-parse-issues-to-errors ()
  (let ((content (with-temp-buffer (insert-file-contents "fixtures/basic.xml") (buffer-string))))
    (flycheck-android-lint-test-compare-with-basic
      (let* ((parsed (flycheck-android-lint-get-issues content))
             (expected-issue (nth 0 expected))
             (new-error (flycheck-android-lint-new-error (nth 0 parsed)))
             (expected-error (flycheck-error-new-at (cdr (assoc 'line expected-issue))
                                                    (cdr (assoc 'column expected-issue))
                                                    (cdr (assoc 'category expected-issue))
                                                    (cdr (assoc 'message expected-issue))
                                                    :filename (cdr (assoc 'file expected-issue))
                                                    :id (cdr (assoc 'id expected-issue)))))
        (should (equal new-error expected-error)) ))))
