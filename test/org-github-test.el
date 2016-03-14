;;; org-github-test --- Tests for org-github-test -*- lexical-binding: t -*-

;; Copyright © 2015 Emanuel Evans

;; Author: Emanuel Evans <mail@emanuel.industries>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for org-github

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'ert)
(require 'org-github)
(require 's)

(unless org-github-access-token
  (setq org-github-access-token "fakefakefake"))

(defconst org-github--fixtures-dir
  (concat (file-name-directory (or load-file-name buffer-file-name))
          "fixtures/"))

(defmacro with-stubbed-url-retrieve (&rest body)
  "Stub all web requests to return STUBBED-RESPONSE and execute BODY.

STUBBED-RESPONSE corresponds to a file in the fixtures directory."
  (declare (indent defun))
  `(progn
     (advice-add #'url-retrieve :around
                 (lambda (_oldretrieve url callback &optional cbargs)
                   (with-temp-buffer
                     (let* ((path (replace-regexp-in-string
                                   "https://api\\.github\\.com" "" url))
                            (response-file
                             (concat org-github--fixtures-dir
                                     (concat (or url-request-method "GET")
                                             (replace-regexp-in-string "/" "-" path)
                                             ".response"))))
                       (if (file-exists-p response-file)
                           (insert-file-contents response-file)
                         (error "%s does not exist" response-file)))
                     (apply callback nil cbargs)))
                 '((name . :stubbed-web-request)))
     (unwind-protect
         ,@body
       (advice-remove #'url-retrieve :stubbed-web-request))))

(defmacro with-org-snippet (snippet &rest body)
  "Use SNIPPET to test BODY in a fresh `org-mode' buffer.

The position of the cursor in SNIPPET can be specified by using
square brackets.  The first instance of square brackets in
SNIPPET specifies the cursor position (the brackets will be
erased).  For instance, \"f[o]o\" will position the cursor before
the first \"o\" and erase the brackets."
  (declare (indent 1))
  `(progn
     (with-temp-buffer
       (insert ,snippet)
       (goto-char (point-min))
       (when (looking-at-p "\n")
         (delete-char +1))
       (when (search-forward "<point>" nil 'noerror)
         (delete-char (- 0 (length "<point>"))))
       (org-mode)
       (show-all)
       (org-github-mode)
       ,@body)))

(defun org-github--should-equal-fixture (f)
  "Assert that current buffer is equal to F (a fixture file)."
  (let ((fname (concat org-github--fixtures-dir f)))
    (unless (file-exists-p fname)
      (error "%s doesn't exist" fname))
    (let ((current-contents (replace-regexp-in-string "[ \t]+" "" (buffer-string))))
      (with-temp-buffer
        (insert-file-contents fname)
        (should (string= (replace-regexp-in-string "[ \t]+" ""(buffer-string)) current-contents))))))

(ert-deftest org-github-basic-response ()
  (with-stubbed-url-retrieve
    (org-github--retrieve
     "GET" "/" nil
     (lambda (data)
       (should (equal (cdr (assq 'current_user_url data))
                      "https://api.github.com/user"))))
    (should (equal org-github--rate-limit-remaining 4996))))

(ert-deftest org-github-my-issues ()
  (with-stubbed-url-retrieve
    (save-excursion
      (org-github-my-issues)
      (switch-to-buffer org-github-buffer)
      (should org-github-mode)
      (org-github--should-equal-fixture "user-issues.org")
      (show-all)
      (goto-char (point-min))
      (search-forward "Comments...")
      (org-github-cycle)
      (org-github--should-equal-fixture "user-issues-expanded.org"))))

(ert-deftest org-github-create-new-issue ()
  (with-stubbed-url-retrieve
    (switch-to-buffer "*github-test*")
    (erase-buffer)
    (insert-file-contents (concat org-github--fixtures-dir "new-issue.org"))
    (org-mode)
    (org-github-mode)
    (show-all)
    (goto-char (point-min))
    (search-forward "A problem")
    (org-ctrl-c-ctrl-c)
    (org-github--should-equal-fixture "new-issue-after-create.org")))

(ert-deftest org-github-update-issue ()
  (with-stubbed-url-retrieve
    (switch-to-buffer "*github-test*")
    (erase-buffer)
    (insert-file-contents (concat org-github--fixtures-dir "new-issue.org"))
    (org-mode)
    (org-github-mode)
    (show-all)
    (goto-char (point-min))
    (search-forward "This is")
    (insert " yet")
    (org-ctrl-c-ctrl-c)
    (org-github--should-equal-fixture "new-issue-after-update.org")))

(ert-deftest org-github-update-comment ()
  (with-stubbed-url-retrieve
    (switch-to-buffer "*github-test*")
    (erase-buffer)
    (insert-file-contents (concat org-github--fixtures-dir "user-issues-expanded.org"))
    (org-mode)
    (org-github-mode)
    (show-all)
    (goto-char (point-min))
    (search-forward "A comment")
    (insert " with some more details")
    (org-ctrl-c-ctrl-c)
    (org-github--should-equal-fixture "user-issues-expanded-after-comment-update.org")))

(ert-deftest org-github-comments-header ()
  (with-org-snippet "
* s<point>hosti/org-github
** OPEN [[https://github.com/shosti/org-github/issues/1][This is a sample issue]]
Something
*** Comments..."
    (should-not (org-github--comments-header-p (org-element-at-point))))

  (with-org-snippet "
* shosti/org-github
** OPEN [[https://github.com/shosti/org-github/issues/1][This is a sample issue]]
Something
*** C<point>omments..."
    (should (org-github--comments-header-p (org-element-at-point))))

  (with-org-snippet "
* shosti/org-github
** OPEN [[https://github.com/shosti/org-github/issues/1][This is a sample issue]]
Something
*** C<point>omments
**** someone"
    (should-not (org-github--comments-header-p (org-element-at-point)))))

(ert-deftest org-github-issue-at-point ()
  (seq-do
   (lambda (args)
     (let ((snippet (car args))
           (want (cdr args)))
       (with-org-snippet snippet
         (should (equal want (org-github--issue-at-point (point)))))))

   '(("
* shosti/org-github
:PROPERTIES:
:og-type: repo
:full_name: shosti/org-github
:END:
** <point>An issue"
      . ((title . "An issue")
         (body . nil)
         (labels . [])))

     ("
* shosti/org-github
:PROPERTIES:
:og-type: repo
:full_name: shosti/org-github
:END:
** <point>An issue     :wontfix:"
      . ((title . "An issue")
         (body . nil)
         (labels . ["wontfix"])))

     ("
* shosti/org-github
:PROPERTIES:
:og-type: repo
:full_name: shosti/org-github
:END:
** <point>An issue    :wontfix:something_else:
This is the body.
Not sure what it means?"
      . ((title . "An issue")
         (body . "This is the body.\nNot sure what it means?")
         (labels . ["wontfix" "something else"])))

     ("
* shosti/org-github
:PROPERTIES:
:og-type: repo
:full_name: shosti/org-github
:END:
** OPEN <point>An issue [[https://github.com/shosti/org-github/issues/2][#2]]  :wontfix:
:PROPERTIES:
:og-type: issue
:number: 2
:url: https://api.github.com/repos/shosti/org-github/issues/2
:END:
This is the body.

Not sure what it means?
*** Comments
**** octocat
A comment to mess things up!"
      . ((title . "An issue")
         (body . "This is the body.\n\nNot sure what it means?")
         (labels . ["wontfix"])))

     ("
* shosti/org-github
:PROPERTIES:
:og-type: repo
:full_name: shosti/org-github
:END:
** <point>An issue [[https://github.com/shosti/org-github/issues/2][#2]]  :wontfix:
:PROPERTIES:
:og-type: issue
:number: 2
:url: https://api.github.com/repos/shosti/org-github/issues/2
:END:"
      . ((title . "An issue")
         (body . nil)
         (labels . ["wontfix"]))))))

(ert-deftest org-github-todo-keywords ()
  (with-org-snippet "
* shosti/org-github
** OPEN <point>A problem"
    (should (equal (org-get-todo-state) "OPEN"))
    (org-todo)
    (should (equal (org-get-todo-state) "CLOSED"))))

(ert-deftest org-github-at-new-issue-p ()
  (with-org-snippet "
* shosti/org-github
:PROPERTIES:
:og-type:  repo
:END:
Organize your Github issues with org-mode
** Another <point>problem, this time from org-github!
** This issue was already there
:PROPERTIES:
:og-type:  issue
:END:"
    (should (org-github--at-new-issue-p (point))))

  (with-org-snippet "
* shosti/org-github
Organize your Github issues with org-mode
** Another <point>problem, this time from org-github!"
    (should-not (org-github--at-new-issue-p (point))))

  (with-org-snippet "
* sho<point>sti/org-github
:PROPERTIES:
:og-type:  repo
:END:
Organize your Github issues with org-mode
** Another problem, this time from org-github!"
    (should-not (org-github--at-new-issue-p (point))))

  (with-org-snippet "
* shosti/org-github
:PROPERTIES:
:og-type:  repo
:END:
Organize your Github issues with org-mode
** Another <point>problem, this time from org-github!
:PROPERTIES:
:og-type:  issue
:END:"
    (should-not (org-github--at-new-issue-p (point)))))

(ert-deftest org-github-at-existing-issue-p ()
  (with-org-snippet "
* shosti/org-github
:PROPERTIES:
:og-type:  repo
:END:
Organize your Github issues with org-mode
** Another problem, this time from org-github!
** This issu<point>e was already there
:PROPERTIES:
:og-type:  issue
:END:"
    (should (org-github--at-existing-issue-p (point))))

  (with-org-snippet "
* shosti/org-github
:PROPERTIES:
:og-type:  repo
:END:
Organize your Github issues with org-mode
** This issue was already there
:PROPERTIES:
:og-type:  issue
:END:
This is an <point>issue body."
    (should (org-github--at-existing-issue-p (point))))

  (with-org-snippet "
* shosti/org-git<point>hub
:PROPERTIES:
:og-type:  repo
:END:
Organize your Github issues with org-mode
** This issue was already there
:PROPERTIES:
:og-type:  issue
:END:"
    (should-not (org-github--at-existing-issue-p (point)))))

(ert-deftest org-github-at-new-comment-p ()
  (with-org-snippet "
** Issue
:PROPERTIES:
:og-type:  issue
:END:
*** Comments
**** Someone
A cool comment

With line breaks
**** Someone
Another <point>comment"
    (should (org-github--at-new-comment-p (point))))

  (with-org-snippet "
** Issue
:PROPERTIES:
:og-type:  issue
:END:
*** Comments
**** Someone
:PROPERTIES:
:og-type: comment
:END:
A cool <point>comment"
    (should-not (org-github--at-new-comment-p (point))))

  (with-org-snippet "
** Issue
:PROPERTIES:
:og-type:  issue
:END:
*** <point>Comments
**** Someone
A cool comment"
    (should-not (org-github--at-new-comment-p (point)))))

(ert-deftest org-github-at-existing-comment-p ()
  (with-org-snippet "
**** A <point>comment
:PROPERTIES:
:og-type: comment
:END:"
    (should (org-github--at-existing-comment-p (point))))

  (with-org-snippet "
*** Comments
**** A cool <point>comment "
    (should-not (org-github--at-existing-comment-p (point)))))

(ert-deftest org-github-group-and-sort ()
  (let ((got (org-github--group-and-sort-issues
              '[((name . "repo2issue2")
                 (number . 2)
                 (repository . ((full_name . "owner/repo2"))))
                ((name . "repo1issue1")
                 (number . 1)
                 (repository . ((full_name . "owner/repo1"))))
                ((name . "repo2issue1")
                 (number . 1)
                 (repository . ((full_name . "owner/repo2"))))])))
    (should (equal (seq-map (lambda (group)
                              (seq-map (lambda (issue) (cdr (assq 'name issue)))
                                       (cdr group)))
                            got)
                   '(("repo1issue1") ("repo2issue2" "repo2issue1"))))))

;;; org-github-test.el ends here
