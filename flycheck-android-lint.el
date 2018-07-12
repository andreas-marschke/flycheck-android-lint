;;; flycheck-android-lint.el --- Flycheck checker for Android projects parsing Android lint XML file

;; Copyright (C) 2018 Andreas Marschke

;; Author: Andreas Marschke <emacs@andreas-marschke.name>
;; Created: 11.07.2018
;; Version: 0.1
;; Package-Requires: (())
;; Keywords: 
;; URL:
;;; Commentary:
;;
;; The checker takes the output from Androids linting XML file and
;; reports them in your project.
;;
;;; Code:

(require 'filenotify)
(require 'flycheck)
(require 'projectile)

;;;; Customization

(defcustom flycheck-android-lint-results-file "build/reports/lint-results.xml"
  "")

(defcustom flycheck-android-lint-minimum-priority 100
  "")

;;;; Utility Methods
(defun flycheck-android-lint-get-issues (string)
  "Parse XML Lint output from STRING and create a list of 
cons cells collections with the keys:

 - id: The Class ID of the issue
 - message: the error message
 - category: Category of the lint issue
 - priority: priority level of error (number)
 - summary: A summarizing description of the issue
 - explanation: A reason for the issue
 - errorLine1: First line where the issue occured
 - errorLine2: Pointer where the issue is (compiler-like ascii art)"
  (let* ((xml (with-temp-buffer
                (insert string)
                (xml-parse-region (point-min) (point-max))))
         (new-list ()))
    (dolist (issue (xml-get-children (car xml) 'issue))
      (let* ((attrs (xml-node-attributes issue))
             (id  (cdr (assq 'id attrs)))
             (severity  (cdr (assq 'severity attrs)))
             (message   (cdr (assq 'message attrs)))
             (category   (cdr (assq 'category attrs)))
             (priority   (cdr (assq 'priority attrs)))
             (summary   (cdr (assq 'summary attrs)))
             (explanation   (cdr (assq 'explanation attrs)))
             (errorLine1  (cdr (assq 'errorLine1 attrs)))
             (errorLine2   (cdr (assq 'errorLine2 attrs)))
             (location (car (xml-get-children issue 'location)))
             (location-attrs (xml-node-attributes location))
             (location-file (cdr (assq 'file location-attrs)))
             (location-line (cdr (assq 'line location-attrs)))
             (location-column (cdr (assq 'column location-attrs))))
        
        (setq new-list
              (cons
               (list
                (cons 'id id)
                (cons 'message message)
                (cons 'category category)
                (cons 'priority (ignore-errors (string-to-number priority)))
                (cons 'summary summary)
                (cons 'explanation explanation)
                (cons 'errorLine1 errorLine1)
                (cons 'errorLine2 errorLine2)
                (cons 'file location-file)
                (cons 'line (ignore-errors (string-to-number location-line)))
                (cons 'column (ignore-errors (string-to-number location-column))))
               new-list))))
    new-list))

(defun flycheck-android-lint-get-by-priority (issues priority)
  "Get all issues higher than PRIORITY"
  (flycheck-android-lint-filter issues 'priority (lamdba (prio) (if (> priority prio)
                                                                    t))))

(defun flycheck-android-lint-filter (issues key match)
  (mapcar (lambda (issue)
            (let ((val (cdr (assoc k issue))))
              (if (funcall match val)
                  issue
                nil)))))

(defun flycheck-android-lint-new-error (issue)
  "Creates a new Flycheck error from an AndroidLint issue"
  (flycheck-error-new-at (cdr (assoc 'line issue))
                         (cdr (assoc 'column issue))
                         (cdr (assoc 'category issue))
                         (cdr (assoc 'message issue))
                         :filename (cdr (assoc 'file issue))
                         :id (cdr (assoc 'id issue))))
;;;; Flycheck functions

;;;;; Start

(defun flycheck-android-lint-start (checker callback)
  "A function to start the syntax checker.

Will add a file-watcher (see 'filenotify) that will listen for changes and deletion
of the `flycheck-android-lint-results-file' relative from the `working-directory'
of the project this flychecker is executed in.

The watcher instance is returned and used in the `:interrupt' event later on.

It also defines a callback for the `watcher' in which it will attempt to read the 
contents of the `flycheck-android-lint-results-file' and parse any issues found by it.

All of the found lint issues will then be translated into `flycheck-error' objects
and sent with the flycheck-callback `finished' STATUS.
"
  (defun parse-errors()
    (let* ((error-list ())
           (lint-result-string (with-temp-buffer (insert-file-contents
                                                  (expand-file-name
                                                   (concat default-directory "/" flycheck-android-lint-results-file)))
                                                 (buffer-string)))
           (lint-result-issues (flycheck-android-lint-get-issues lint-result-string))
           (lint-errors (dolist (issue lint-result-issues error-list)
                          (setq error-list (cons (flycheck-android-lint-new-error issue) error-list)))))
      (message (format "Found lint errors: %s" lint-errors))
      (message (format "Found Results Issues: %s" lint-result-issues))
      (callback (flycheck-report-buffer-checker-status checker 'finished error-list))))
  
  (defun notify-callback (event)
    (let ((lint-results-file (expand-file-name (concat default-directory "/" flycheck-android-lint-results-file)))
          (descriptor (nth 1 event))
          (action (nth 2 event))
          (file (nth 3 event)))
        (message (format "Callback event happened! %s,%s,%s,%s" event descriptor action file))
        (cond
         ((not (string-match-p "deleted" action))
          (parse-errors))
         ((string-match-p "deleted" action)
          (callback "errored" "lint-results.xml was deleted, not checking...")))))
    
  (message (format "Flycheck-Android-Lint Start(): Trying to watch file: %s" (expand-file-name
                                                                              (concat default-directory "/" flycheck-android-lint-results-file))))
    
    ;; Do the initial parsing here
    (condition-case err
        (parse-errors)
      (error (message (format "Flycheck-Android-Lint Start(): An error occured doing initial validation: %s" err))))

    ;; Subsequent builds, may write to the file so we are going to watch for it if possible
    (condition-case err
        (file-notify-add-watch (expand-file-name
                                (concat default-directory "/" flycheck-android-lint-results-file))
                               (list 'change) 'notify-callback)
      (error (message (format "Flycheck-Android-Lint Start(): An error occured: %s" err)))))

;;;;; Interrupt

(defun flycheck-android-lint-interrupt (checker descriptor)
  "Interrupts the current CHECKER by stopping the DESCRIPTOR"
  (if (file-notify-valid-p descriptor)
      (file-notify-rm-watch descriptor)))

;;;;; Working Directory
(defun flycheck-android-lint-working-directory (directory)
  "If we are in a project this will give you the root of the project as working directory"
  (cond
   ((bound-and-true-p projectile-mode)
    (if (projectile-project-p)
        (projectile-project-root)
      nil))))

;;;;; Print Documentation

;; TODO: Add more documentation!
(defun flycheck-android-lint-print-doc (checker)
  ""
  (message (format "Flycheck-Android-Lint Doc-Print: %s" checker))
  "")

;;;;; Verification

(defun flycheck-android-lint-verify (checker)
  (let* (
         ;; File Paths

         ;; build/reports/lint-results.xml
         (full-file (expand-file-name (concat (flycheck-android-lint-working-directory ".") "/" flycheck-android-lint-results-file)))
         ;; build/reports/
         (full-file-dir-path (expand-file-name (file-name-directory full-file)))
         ;; build/
         (parent-file-dir-path (expand-file-name (concat full-file-dir-path "..")))

         ;; Files Exist?
         (full-file-p (file-exists-p full-file))
         (full-file-dir-path-p (file-exists-p full-file-dir-path))
         (parent-file-dir-path-p (file-exists-p parent-file-dir-path))

         ;; Message
         (full-file-message (if full-file-p
                                (format "Found lint-results.xml at: %s" full-file)
                              (format "Could not find lint-results.xml! at: %s" full-file)))
         (full-file-dir-message (if full-file-dir-path-p
                                    (format "Found reports directory at: %s" full-file-dir-path)
                                  (format "Could not find reports directory at: %s" full-file-dir-path)))
         (parent-file-dir-message (if parent-file-dir-path-p
                                      (format "Found build dir at: %s" parent-file-dir-path)
                                    (format "Could not find build dir at: %s" parent-file-dir-path)))

         ;; Faces
         (full-file-face (if full-file-p
                             'success
                           'failure))
         (full-file-dir-face (if full-file-dir-path-p
                                 'success
                               'failure))
         (parent-file-dir-face (if parent-file-dir-path-p
                                   'success
                                 'failure))
         ;; Results
         (full-file-result (flycheck-verification-result-new :label "Found /lint-results.xml?" :message full-file-message :face full-file-face))
         (full-file-dir-result (flycheck-verification-result-new :label "Found /reports/ directory?" :message full-file-dir-message :face full-file-dir-face))
         (parent-file-dir-result (flycheck-verification-result-new :label "Found /build/ directory?" :message parent-file-dir-message :face parent-file-dir-face)))
    (list full-file-result full-file-dir-result parent-file-dir-result)))

;;;;; Predicate

;; TODO: This may need some more checks if we can use projectile or other project stuff safely here
(defun flycheck-android-lint-predicate ()
  (message (format "Flycheck-Android-Lint Predicate: %s" (flycheck-android-lint-verify nil)))
  (if (flycheck-android-lint-verify nil)
      t)
  t)

;;;;; Enabled?

(defun flycheck-android-lint-enabled ()
  (message (format "Flycheck-Android-Lint Enabled: %s" "enabled"))
  t)


;;;; Checker Definition

(flycheck-define-generic-checker 'android-lint
  "A Linter for Android projects where androidLint has run"
  :start 'flycheck-android-lint-start
  :interrupt 'flycheck-android-lint-interrupt
  :print-doc 'flycheck-android-lint-print-doc
  :verify 'flycheck-android-lint-verify
  :predicate 'flycheck-android-lint-predicate
  :enabled 'flycheck-android-lint-enabled
  :working-directory 'flycheck-android-lint-working-directory
  :modes '(dired-mode
           nxml-mode
           xml-mode
           java-mode
           conf-mode
           jdee-java-properties-mode
           gradle-mode
           groovy-mode))

;; :error-filter ;; FUNCTION ;; Should be Easy, we can filter by priority and/or other properties
;; :error-explainer ;; FUNCTION ;; Can use the additional data provided by issues

(add-to-list 'flycheck-checker 'android-lint)

;;;; Providing

(provide 'flycheck-android-lint)
