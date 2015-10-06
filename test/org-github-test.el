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
                                     (concat (replace-regexp-in-string "/" "-" path)
                                             ".response"))))
                       (if (file-exists-p response-file)
                           (insert-file-contents response-file)
                         (error "%s does not exist" response-file)))
                     (apply callback nil cbargs)))
                 '((name . :stubbed-web-request)))
     ,@body
     (advice-remove #'url-retrieve :stubbed-web-request)))

(defun org-github--should-equal-fixture (f)
  "Assert that current buffer is equal to F (a fixture file)."
  (let ((fname (concat org-github--fixtures-dir f)))
    (unless (file-exists-p fname)
      (error "%s doesn't exist" fname))
    (let ((current-contents (buffer-string)))
      (with-temp-buffer
        (insert-file-contents fname)
        (should (string= (buffer-string) current-contents))))))

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
      (should org-github-minor-mode)
      (org-github--should-equal-fixture "user-issues.org"))))

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
                 (repository . ((full_name . "owner/repo2")))) ])))
    (should (equal (seq-map #'car got) '("owner/repo1" "owner/repo2")))
    (should (equal (seq-map (lambda (issue) (cdr (assoc 'name issue)))
                            (cdr (assoc "owner/repo1" got)))
                   '("repo1issue1")))
    (should (equal (seq-map (lambda (issue) (cdr (assoc 'name issue)))
                            (cdr (assoc "owner/repo2" got)))
                   '("repo2issue1" "repo2issue2")))))

;;; org-github-test.el ends here
