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
          "fixtures"))

(defmacro with-stubbed-url-retrieve (stubbed-response &rest body)
  "Stub all web requests to return STUBBED-RESPONSE and execute BODY.

STUBBED-RESPONSE corresponds to a file in the fixtures directory."
  (declare (indent 1))
  `(progn
     (advice-add #'url-retrieve :around
                 (lambda (_oldretrieve _url callback &optional cbargs)
                   (with-temp-buffer
                     (insert-file-contents (concat org-github--fixtures-dir "/"
                                                   ,stubbed-response))
                     (apply callback nil cbargs)))
                 '((name . :stubbed-web-request)))
     ,@body
     (advice-remove #'url-retrieve :stubbed-web-request)))

(ert-deftest org-github-basic-response ()
  (with-stubbed-url-retrieve "basic-response"
    (org-github--retrieve
     "GET" "/" nil
     (lambda (data)
       (should (equal (cdr (assq 'current_user_url data))
                      "https://api.github.com/user"))))
    (should (equal org-github--rate-limit-remaining 4996))))

;;; org-github-test.el ends here
