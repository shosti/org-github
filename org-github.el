;;; org-github --- Organize your Github issues with org-mode -*- lexical-binding: t -*-

;; Copyright © 2015 Emanuel Evans

;; Author: Emanuel Evans <mail@emanuel.industries>
;; Maintainer: Emanuel Evans <mail@emanuel.industries>
;; URL: http://github.com/shosti/org-github
;; Version: 0.0.1
;; Created: 27 Sep 2015
;; Package-Requires: ((emacs "24") (seq "1.9"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Organize and edit your Github issues with org-mode.

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

(require 'auth-source)
(require 'json)
(require 'seq)
(require 'url)

(defgroup org-github nil
  "Customization group for org-github."
  :group 'org)

(defcustom org-github-access-token nil
  "OAuth2 token for Github API access.

If nil, org-github will attempt to use an appropriate value from
.authinfo or .netrc."
  :group 'org-github
  :type 'string)

(defcustom org-github-username nil
  "Your Github username.

If nil, org-github will attempt to use an appropriate value from
.authinfo or .netrc."
  :group 'org-github
  :type 'string)

(defvar org-github-buffer "*github*"
  "Buffer to display Github issues.")

(defvar org-github--rate-limit-remaining nil
  "Remaining API requests permitted by rate-limiting.")

;;;###autoload
(define-minor-mode org-github-minor-mode
  "Minor mode for interacting with Github issues through org mode."
  :group 'org
  (setq-local org-todo-keywords
              '((sequence "OPEN" "CLOSED"))))

;;;###autoload
(defun org-github-my-issues ()
  "Show current user issues in a buffer."
  (interactive)
  (message "Retrieving issues...")
  (org-github--retrieve
   "GET" "/user/issues" nil
   (lambda (data)
     (switch-to-buffer-other-window org-github-buffer)
     (erase-buffer)
     (org-mode)
     (org-github-minor-mode)
     (org-github--insert-issues data))))

(defun org-github--group-and-sort-issues (issues)
  "Group ISSUES according to repo and sort by issue number."
  (let ((sorted-issues
         (seq-sort (lambda (issue1 issue2)
                     (< (cdr (assq 'number issue1))
                        (cdr (assq 'number issue2))))
                   issues)))
    (seq-group-by (lambda (issue)
                    (cdr (assq 'full_name
                                (cdr (assq 'repository issue)))))
                  sorted-issues)))

(defun org-github--insert-issues (issues)
  "Insert ISSUES (as returned by the Github API), grouped by repository."
  (seq-do #'org-github--insert-issue-group
          (org-github--group-and-sort-issues issues)))

(defun org-github--insert-issue-group (group)
  "Insert GROUP as an org item.

GROUP should be a pair of (TITLE . ISSUES), where TITLE is the
heading under which to group the issues and ISSUES is a list of
issues as returned by the Github API."
  (let ((title (car group))
        (issues (cdr group)))
    (org-insert-heading)
    (insert title)
    (newline)
    (let ((issue-beg (point)))
      (seq-do #'org-github--insert-issue issues)
      (org-map-region #'org-demote issue-beg (point)))
    (org-global-cycle 2)))

(defun org-github--insert-issue (issue)
  "Insert ISSUE (as returned by the Github API) as a top-level item."
  (insert "* ")
  (insert (upcase (cdr (assq 'state issue))))
  (insert " ")
  (org-insert-link nil (cdr (assq 'html_url issue)) (cdr (assq 'title issue)))
  (let ((body (cdr (assq 'body issue))))
    (when (> (length body) 0)
      (newline)
      (insert body)))
  (newline)
  (when (> (cdr (assq 'comments issue)) 0)
    (insert "** Comments...")
    (newline)))

(defun org-github--get-access-token ()
  "Get the Github API access token for the user."
  (or org-github-access-token
      (setq org-github-access-token
            (ignore-errors
              (funcall
               (plist-get (org-github--netrc-auth) :secret))))
      (user-error "Github access token must be set")))

(defun org-github--get-username ()
  "Get the Github username for the user."
  (or org-github-username
      (setq org-github-username
            (ignore-errors
              (plist-get (org-github--netrc-auth) :user)))
      (user-error "Github username must be set")))

(defun org-github--netrc-auth ()
  "Get the netrc or authinfo information for Github as a plist, if it exists."
  (car (auth-source-search :host "github.com" :type 'netrc)))

(defun org-github--retrieve (method endpoint data callback)
  "Send an API request with METHOD to the Github API at ENDPOINT.

DATA is any data to be sent with the request.
CALLBACK is called with the parsed request results."
  (let ((url-request-method method)
        (url-request-data data)
        (url-request-extra-headers (list
                                    (cons "Authorization"
                                          (concat "token " (org-github--get-access-token)))
                                    (cons "Accept" "application/vnd.github.v3+json"))))
    (url-retrieve (concat "https://api.github.com" endpoint)
                  #'org-github--parse-response
                  (list callback))))

(defun org-github--parse-response (status callback)
  "Parse API response (according to STATUS) and call CALLBACK with parsed results."
  (let ((err (plist-get status :error)))
    (when err
      (error "Github API Error: %s" err)))
  (save-excursion
    (ignore-errors
      (re-search-forward "X-RateLimit-Remaining: \\([0-9]+\\)")
      (setq org-github--rate-limit-remaining
            (string-to-number (match-string 1)))))
  (forward-paragraph)
  (let ((parsed (json-read)))
    (funcall callback parsed)))

(provide 'org-github)

;;; org-github.el ends here
