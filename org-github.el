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
(require 'org)
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

(defvar org-github-mode-map
  (let ((keymap (make-sparse-keymap)))
    (org-defkey keymap [remap org-cycle] #'org-github-cycle)
    keymap)
  "Keymap used by `org-github-mode'.")


;;;###autoload
(define-minor-mode org-github-mode
  "Minor mode for interacting with Github issues through org mode."
  nil nil org-github-mode-map
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
     (org-github-mode)
     (org-github--insert-issues data))))

(defun org-github-cycle (&optional arg)
  "Visibility cycling for Org-mode, with github actions taken into account.

See documentation for `org-cycle' for more details, including ARG
usage."
  (interactive)
  (if (org-github--comments-header-p (org-element-at-point))
      (org-github--insert-comments (org-entry-get-with-inheritance "comments_url")
                                   (line-number-at-pos))
    (org-cycle arg)))

(defun org-github--comments-header-p (elem)
  "Return non-nil if ELEM is a github issue comments header."
  (and (eq (car elem) 'headline)
       (equal (plist-get (cadr elem) :title) "Comments...")))

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

(defun org-github--insert-comments (comments-url line)
  (let ((buffer (current-buffer)))
    (goto-line line)
    (beginning-of-line)
    (search-forward "Comments")
    (kill-line)
    (newline)
    (insert "Loading...")
    (org-github--retrieve "GET" comments-url nil
                          (lambda (comments)
                            (with-current-buffer buffer
                              (goto-line line)
                              (forward-line)
                              (beginning-of-line)
                              (kill-line)
                              (let ((beg (point)))
                                (seq-do #'org-github--insert-comment comments)
                                (org-map-region #'org-demote beg (point))))))))

(defun org-github--insert-comment (comment)
  (org-insert-heading)
  (org-insert-link nil (cdr (assq 'html_url comment)) (cdr (assq 'login (cdr (assq 'user comment)))))
  (newline)
  (insert (org-github--fix-body (cdr (assq 'body comment))))
  (org-github--set-properties comment))

(defun org-github--insert-issues (issues)
  "Insert ISSUES (as returned by the Github API), grouped by repository."
  (show-all)
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
  "Insert ISSUE (as returned by the Github API) as an org item."
  (insert "* ")
  (insert (upcase (cdr (assq 'state issue))))
  (insert " ")
  (org-insert-link nil (cdr (assq 'html_url issue)) (cdr (assq 'title issue)))
  (let ((body (cdr (assq 'body issue))))
    (when (> (length body) 0)
      (newline)
      (insert body)))
  (newline)
  (org-github--set-properties issue '(comments_url))
  (when (> (cdr (assq 'comments issue)) 0)
    (insert "** Comments...")
    (newline)))

(defun org-github--set-properties (object &optional extra-props)
  "Set the properties of the current org item according to OBJECT.

OBJECT should be a Github API response object.  The properties
url, created_at, and updated_at will always be set; in addition,
all of the properties in EXTRA-PROPS (a list of symbols) will
also be set."
  (seq-do (lambda (prop)
            (org-set-property (symbol-name prop) (cdr (assq prop object))))
          (append '(url created_at updated_at) extra-props)))

(defun org-github--fix-body (body)
  (replace-regexp-in-string "\r" "" body))

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

;; TODO: For dev only, remove soon
(defvar org-github-last-response)

(defun org-github--retrieve (method endpoint data callback)
  "Send an API request with METHOD to the Github API at ENDPOINT.

DATA is any data to be sent with the request.
CALLBACK is called with the parsed request results."
  (let ((url-request-method method)
        (url-request-data data)
        (url-request-extra-headers (list
                                    (cons "Authorization"
                                          (concat "token " (org-github--get-access-token)))
                                    (cons "Accept" "application/vnd.github.v3+json")))
        (url (cond ((string-prefix-p "https://" endpoint) endpoint)
                   ((string-prefix-p "/" endpoint) (concat "https://api.github.com" endpoint))
                   (t (error "invalid endpoint: %s" endpoint)))))
    (url-retrieve url #'org-github--parse-response (list callback))))

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
  (setq org-github-last-response (buffer-string))
  (forward-paragraph)
  (let ((parsed (json-read)))
    (funcall callback parsed)))

(provide 'org-github)

;;; org-github.el ends here
