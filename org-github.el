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
(require 'url)

(defgroup org-github nil
  "Customization group for org-github."
  :group 'org)

(defcustom org-github-access-token nil
  "OAuth2 token for Github API access.

If nil, org-github will attempt to use an appropriate value from
.authinfo or .netrc."
  :group 'org-jira
  :type 'string)

(defvar org-github--rate-limit-remaining nil
  "Remaining API requests permitted by rate-limiting.")

(defun org-github--get-access-token ()
  "Get the Github API access token for the user."
  (or org-github-access-token
      (setq org-github-access-token
            (ignore-errors
              (funcall
               (plist-get (car (auth-source-search :host "github.com"
                                                   :type 'netrc))
                          :secret))))
      (user-error
       "Github access token must be set")))

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
