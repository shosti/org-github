;;; org-github.el --- Organize your Github issues with org-mode -*- lexical-binding: t -*-

;; Copyright © 2015 Emanuel Evans

;; Author: Emanuel Evans <mail@emanuel.industries>
;; Maintainer: Emanuel Evans <mail@emanuel.industries>
;; URL: http://github.com/shosti/org-github
;; Version: 0.0.1
;; Created: 27 Sep 2015
;; Package-Requires: ((emacs "24.4") (seq "1.9") (s "1.10.0"))

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
(require 'org-element)
(require 's)
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
  "Keymap for org-github mode.")

;;;###autoload
(define-minor-mode org-github-mode
  "Minor mode for interacting with Github issues through org mode."
  nil nil org-github-mode-map
  :group 'org
  (let ((org-todo-keywords (cons '(sequence "OPEN" "CLOSED") org-todo-keywords)))
    (org-mode-restart))
  (setq org-github-mode t))

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

(defun org-github-update (&optional point)
  "Intelligently update Github issue at POINT."
  (when org-github-mode
    (let ((point (or point (point))))
      (cond ((org-github--at-new-issue-p point)
             (progn (org-github--create-issue point)
                    t))
            ((org-github--at-existing-issue-p point)
             (progn (org-github--update-issue point)
                    t))))))

(add-hook 'org-ctrl-c-ctrl-c-hook #'org-github-update)

(defun org-github-cycle (&optional arg)
  "Visibility cycling for Org-mode, with github actions taken into account.

See documentation for `org-cycle' for more details, including ARG
usage."
  (interactive)
  (if (and org-github-mode (org-github--comments-header-p (org-element-at-point)))
      (org-github--insert-comments)
    (org-cycle arg)))

(defun org-github--group-and-sort-issues (issues)
  "Group ISSUES according to repo and sort by issue number.

The result is an alist of (REPO . ISSUES), where REPO is a
repository object (as returned by Github) and ISSUES is a sorted
list of issues."
  (let ((grouped-issues
         (seq-group-by (lambda (issue)
                         (cdr (assq 'full_name
                                    (cdr (assq 'repository issue)))))
                       issues)))
    (seq-map (lambda (group)
               (let* ((issues (cdr group))
                      (repo (cdr (assq 'repository
                                       (car issues)))))
                 (cons repo
                       (seq-sort (lambda (issue1 issue2)
                                   (> (cdr (assq 'number issue1))
                                      (cdr (assq 'number issue2))))
                                 issues))))
             grouped-issues)))

(defun org-github--create-issue (point)
  "Create Github issue, reading the data at POINT."
  (save-excursion
    (goto-char point)
    (org-back-to-heading)
    (org-up-heading-all 1)
    (let ((type (org-entry-get (point) "og-type"))
          (repo-url (org-entry-get (point) "url"))
          (repo-name (org-entry-get (point) "full_name")))
      (unless (equal type "repo")
        (error "Invalid format for Github issue item"))
      (org-github--post-issue (org-github--issue-at-point point)
                              repo-name
                              (concat repo-url "/issues")))))

(defun org-github--update-issue (point)
  "Update issue at POINT using the Github API."
  (unless (equal (org-entry-get point "og-type") "issue")
    (error "Not at an issue"))
  (let ((buffer (current-buffer))
        (issue (org-github--issue-at-point))
        (url (org-entry-get point "url"))
        (repo-name (org-entry-get-with-inheritance "full_name")))
    (org-github--retrieve
     "PATCH" url (json-encode issue)
     (lambda (issue)
       (with-current-buffer buffer
         (let ((issue-elem (org-github--find-issue-by-number (cdr (assq 'number issue))
                                                             repo-name)))
           (org-github--replace-issue issue-elem issue)))))))

(defun org-github--post-issue (issue repo-name url)
  "Post ISSUE, a new issue for repo REPO-NAME, to URL."
  (let ((buffer (current-buffer)))
    (org-github--retrieve
     "POST" url (json-encode issue)
     (lambda (issue)
       (with-current-buffer buffer
         (let ((issue-elem (org-github--find-issue-by-title (cdr (assq 'title issue))
                                                            repo-name)))
           (org-github--replace-issue issue-elem issue)))))))

(defun org-github--replace-issue (issue-elem issue)
  "Replace ISSUE-ELEM with the data from ISSUE in the current buffer."
  (let ((level (org-element-property :level issue-elem)))
    (org-github--replace-elem issue-elem
                              (org-github--issue->elem issue level 'exclude-comments))))

(defun org-github--replace-elem (old-elem new-elem)
  "Replace OLD-ELEM with NEW-ELEM in the current buffer, preserving subheadings."
  (let ((beg (org-element-property :begin old-elem))
        (end (org-element-property :end old-elem))
        (subheadings (seq-filter (lambda (elem)
                                   (and (consp elem)
                                        (eq (car elem) 'headline)))
                                 old-elem)))
    (unless (and beg end)
      (error "Invalid element"))
    (delete-region beg end)
    (goto-char beg)
    (org-github--insert-elem (append new-elem subheadings))))

(defun org-github--find-repo (repo-name)
  "Find the repo element for REPO-NAME in the current buffer."
  (or (org-element-map (org-element-parse-buffer) 'headline
        (lambda (elem)
          (when (and (equal (org-element-property :OG-TYPE elem) "repo")
                     (equal (org-element-property :FULL_NAME elem) repo-name))
            elem))
        nil 'first-match)
      (error "Could not find repo \"%s\" in the current buffer" repo-name)))

(defun org-github--issue-at-point (&optional point)
  "Return the issue at POINT as an API object."
  (save-excursion
    (let ((point (or point (point))))
      ;; Unfortunately, the elements returned by `org-element-at-point'
      ;; are woefully incomplete, so we have to do a bit of a dance to
      ;; get the full element.
      (goto-char point)
      (let ((issue-elem
             (cond ((org-github--at-new-issue-p point)
                    (org-github--find-issue-by-title
                     (org-github--elem-title (org-element-at-point))
                     (org-entry-get-with-inheritance "full_name")))
                   ((org-github--at-existing-issue-p point)
                    (org-github--find-issue-by-number
                     (org-entry-get point "number")
                     (org-entry-get-with-inheritance "full_name")))
                   (t (error "Not at a github issue")))))
        (org-github--elem->issue issue-elem)))))

(defun org-github--find-issue-by-title (title repo-name)
  "Find the issue element with TITLE for repo REPO-NAME.

Returns an org element.  Search takes place in the current
buffer.  The issue should not be an existing issue with a
number."
  (let ((repo-elem (org-github--find-repo repo-name)))
    (or (org-element-map repo-elem 'headline
          (lambda (elem)
            (when (and (null (org-element-property :OG-TYPE elem))
                       (equal (org-github--elem-title elem) title))
              elem))
          nil 'first-match)
        (error "Could not find issue \"%s\" in the current buffer" title))))

(defun org-github--find-issue-by-number (number repo-name)
  "Find the issue element with NUMBER for repo REPO-NAME.

Returns an org element.  Search takes place in the current
buffer."
  (let ((repo-elem (org-github--find-repo repo-name))
        (number (format "%s" number)))
    (or (org-element-map repo-elem 'headline
          (lambda (elem)
            (when (and (equal (org-element-property :OG-TYPE elem) "issue")
                       (equal (org-element-property :NUMBER elem) number))
              elem))
          nil 'first-match)
        (error "Could not find issue #%s in the current buffer" number))))

(defun org-github--comments-header-p (elem)
  "Return non-nil if ELEM is a github issue comments header."
  (and (eq (car elem) 'headline)
       (equal (plist-get (cadr elem) :title) "Comments...")))

(defun org-github--at-new-issue-p (point)
  "Return non-nil if POINT is currently at a new issue item."
  (let ((current-type (org-entry-get point "og-type"))
        (parent-type (org-entry-get point "og-type" 'inherit)))
    (and (null current-type)
         (equal parent-type "repo"))))

(defun org-github--at-existing-issue-p (point)
  "Return non-nil if POINT is currently at an existing issue item."
  (equal (org-entry-get point "og-type") "issue"))

(defun org-github--insert-comments ()
  "Insert comments the github issue at point."
  (let ((buffer (current-buffer))
        (comments-url (org-entry-get-with-inheritance "comments_url"))
        (issue-number (org-entry-get-with-inheritance "number")))
    (beginning-of-line)
    (search-forward "Comments")
    (kill-line)
    (newline)
    (insert "Loading...")
    (org-github--retrieve "GET" comments-url nil
                          (lambda (comments)
                            (with-current-buffer buffer
                              (org-github--update-comments comments issue-number))))))

(defun org-github--update-comments (comments issue-number)
  "Insert COMMENTS into the org entry for ISSUE-NUMBER in the current buffer."
  (let* ((issue-elem
          (or (org-element-map (org-element-parse-buffer) 'headline
                (lambda (elem)
                  (when (and
                         (equal (org-element-property :OG-TYPE elem)
                                "issue")
                         (equal (org-element-property :NUMBER elem)
                                issue-number))
                    elem))
                nil 'first-match)
              (error "Could not find issue %s in the current buffer"
                     issue-number)))
         (comments-elem
          (or (org-element-map issue-elem 'headline
                (lambda (elem)
                  (let ((title (org-github--elem-title elem)))
                    (when (and (stringp title)
                               (equal title "Comments"))
                      elem)))
                nil 'first-match)
              (error "Could not find comments section for issue %s"
                     issue-number))))
    (let ((beg (org-element-property :begin comments-elem))
          (end (org-element-property :end comments-elem))
          (level (org-element-property :level comments-elem)))
      (delete-region beg end)
      (goto-char beg)
      (org-github--insert-elem (org-github--comments->elem comments level)))))

(defun org-github--insert-issues (issues)
  "Insert ISSUES (as returned by the Github API), grouped by repository."
  (org-github--insert-elem (org-github--issues->elem issues)))

(defun org-github--issues->elem (issues)
  "Return an org-element parse tree representing ISSUES.

ISSUES is an object as returned by the Github API."
  (append '(org-data nil)
          (seq-map #'org-github--issue-group->elem
                   (org-github--group-and-sort-issues issues))))

(defun org-github--issue-group->elem (group &optional level)
  "Return an org-element tree representing GROUP.

Group should be a (REPO . ISSUES) pair, representing a list of
issues for a repo.

LEVEL represents the org level at which the repo should be
inserted (defaulting to level 1)."
  (let* ((level (or level 1))
         (repo (car group))
         (issue-elems (seq-map (lambda (issue)
                                 (org-github--issue->elem issue (1+ level)))
                               (cdr group))))
    `(headline (:title ((link (:raw-link ,(cdr (assq 'html_url repo)))
                              ,(cdr (assq 'full_name repo))))
                       :level ,level)
               (section nil
                        ,(org-github--props->elem
                          (org-github--props repo 'repo '(full_name)))
                        (paragraph nil
                                   ,(cdr (assq 'description repo))))
               ,@issue-elems)))

(defun org-github--issue->elem (issue &optional level exclude-comments)
  "Return an org-element tree representing ISSUE.

ISSUE should be an object returned from the Github API.

LEVEL represents the org level at which the repo should be
inserted (defaulting to level 1)."
  (let ((level (or level 1))
        (props (append (org-github--props issue 'issue '(number comments_url))
                       (when (cdr (assq 'assignee issue))
                         (list
                          (cons "assignee"
                                (cdr (assq 'login (cdr (assq 'assignee issue)))))))))
        (comments-section
         (when (and (not exclude-comments) (> (cdr (assq 'comments issue)) 0))
           `(headline (:title "Comments..."
                              :level ,(1+ level))))))
    `(headline (:title (,(format "%s " (cdr (assq 'title issue)))
                        (link (:raw-link ,(cdr (assq 'html_url issue)))
                              ,(format "#%d" (cdr (assq 'number issue)))))
                       :level ,level
                       :tags ,(org-github--tags issue)
                       :todo-keyword ,(upcase (cdr (assq 'state issue))))
               (section nil
                        ,(org-github--props->elem props)
                        (paragraph nil
                                   ,(org-github--fix-body
                                     (cdr (assq 'body issue)))))
               ,comments-section)))

(defun org-github--elem->issue (elem)
  "Create an issue object for the Github API from ELEM.

ELEM should be an org element."
  (list
   (cons 'title (org-github--prepare-title
                 (org-github--elem-title elem)))
   (cons 'body (org-github--elem-body elem))
   (cons 'labels (org-github--elem-labels elem))))

(defun org-github--comments->elem (comments level)
  "Return an org-element parse tree representing COMMENTS.

COMMENTS should be a list of comments returned from the Github
API.  LEVEL is the header level at which to the comments should
be inserted."
  (let ((comment-elems (seq-map (lambda (comment)
                                  (org-github--comment->elem comment (1+ level)))
                                comments)))
    `(headline (:title "Comments"
                       :level ,level)
               ,@comment-elems)))

(defun org-github--comment->elem (comment level)
  "Return an org-element parse tree representing COMMENT.

LEVEL is the header level at which to the comments should be
inserted."
  (let ((props (org-github--props comment 'comment)))
    `(headline (:title ((link (:raw-link ,(cdr (assq 'html_url comment)))
                              ,(cdr (assq 'login (cdr (assq 'user comment))))))
                       :level ,level)
               (section nil
                        ,(org-github--props->elem props)
                        (paragraph nil
                                   ,(org-github--fix-body
                                     (cdr (assq 'body comment))))))))

(defun org-github--elem-title (elem)
  "Return the title of ELEM as a string."
  (let ((title (org-element-property :title elem)))
    (cond ((stringp title) title)
          ((stringp (car title)) (car title))
          (t (error "Could not interpret element title")))))

(defun org-github--elem-labels (elem)
  "Return labels for ELEM as a vector."
  (seq-into (seq-map (lambda (label)
                       (s-replace "_" " " label))
                     (org-element-property :tags elem))
            'vector))

(defun org-github--elem-body (elem)
  "Extract the text body for ELEM, a Github issue item."
  (let* ((body-section
          (seq-find (lambda (elem)
                      (and (consp elem)
                           (eq (car elem) 'section)))
                    elem))
         (body
          (s-trim
           (s-join "\n"
                   (org-element-map body-section 'paragraph
                     (lambda (para)
                       (car (org-element-contents para)))
                     nil nil 'paragraph)))))
    (unless (or (null body) (string= "" body))
      body)))

(defconst org-github--title-re
  "\\(.*\\)\\[\\[[^]]+\\]\\[#[0-9]+\\]")

(defun org-github--prepare-title (title)
  "Prepare TITLE for the Github API by stripping links."
  (s-trim (if (string-match org-github--title-re title)
              (match-string 1 title)
            title)))

(defun org-github--props->elem (props)
  "Return a property drawar for PROPS.

Props should be an alist of (VAR . VALUE)."
  (let ((prop-elems
         (seq-map (lambda (prop)
                    `(node-property
                      (:key ,(car prop)
                            :value ,(cdr prop))))
                  props)))
    `(property-drawer nil
                      ,@prop-elems)))

(defun org-github--insert-elem (elem)
  "Insert org element ELEM at point."
  (save-excursion
    (insert (org-element-interpret-data elem))
    ;; HACK: This is a rather horrible workaround for a tag
    ;; indentation bug in org-mode. Hopefully the bug will be fixed
    ;; soon.
    (let ((lines-and-tags
           (org-element-map (org-element-parse-buffer) 'headline
             (lambda (elem)
               (let ((beg (org-element-property :begin elem)))
                 (goto-char beg)
                 (cons (line-number-at-pos)
                       (org-element-property :tags elem)))))))
      (seq-do (lambda (line-and-tag)
                (let ((line (car line-and-tag))
                      (tags (cdr line-and-tag)))
                  (when tags
                    (goto-char (point-min))
                    (forward-line line)
                    (sit-for 0)
                    (org-set-tags-to tags))))
              lines-and-tags)))
  (org-cycle-hide-drawers 'all))

(defun org-github--props (object type &optional extra-props)
  "Return a property drawer elem for OBJECT.

OBJECT should be a Github API response object of type
TYPE (e.g. issue or repo, a symbol).  The properties url,
created_at, and updated_at will always be set; in addition, all
of the properties in EXTRA-PROPS (a list of symbols) will be
set."
  (cons (cons "og-type" (symbol-name type))
        (seq-remove #'null
                    (seq-map (lambda (prop)
                               (let ((val (cdr (assq prop object))))
                                 (when val
                                   (cons (symbol-name prop)
                                         (format "%s" val)))))
                             (append '(url created_at updated_at) extra-props)))))

(defun org-github--tags (object)
  "Get tags for OBJECT (returned by the Github API)."
  (seq-map (lambda (tag) (cdr (assq 'name tag)))
           (cdr (assq 'labels object))))

(defun org-github--fix-body (body)
  "Post-process BODY (as returned by the Github API) for use with org-github."
  (when body
    (replace-regexp-in-string "\r" "" body)))

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
                   (t (error "Invalid endpoint: %s" endpoint)))))
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
