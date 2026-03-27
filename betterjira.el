;;; betterjira.el --- A better Jira interface for Emacs -*- lexical-binding: t; -*-

;; Author: esukaj
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (request "0.3.2"))
;; Keywords: tools, jira, project-management

;;; Commentary:

;; BetterJira provides an Emacs interface for interacting with Jira.
;; Configure `betterjira-host' and `betterjira-project-key' before use.
;; Username and API token are read from ~/.authinfo (or ~/.authinfo.gpg),
;; matched by the `machine' field against `betterjira-host'.
;;
;; Example .authinfo entry:
;;   machine mycompany.atlassian.net login you@example.com password YOUR_API_TOKEN

;;; Code:

(require 'auth-source)
(require 'json)
(require 'url)
(require 'sqlite)

;;; --- Customization ---

(defgroup betterjira nil
  "A better Jira interface for Emacs."
  :group 'tools
  :prefix "betterjira-")

(defcustom betterjira-host nil
  "Jira host as it appears in the `machine' field of your .authinfo.
E.g. \"mycompany.atlassian.net\"."
  :type 'string
  :group 'betterjira)

(defcustom betterjira-project-key nil
  "Jira project key, e.g. \"PROJ\"."
  :type 'string
  :group 'betterjira)

(defcustom betterjira-max-results 50
  "Maximum number of issues to fetch."
  :type 'integer
  :group 'betterjira)

;;; --- Auth ---

(defun betterjira--get-credentials ()
  "Retrieve Jira username and API token from authinfo.
Looks up an entry matching `betterjira-host'.
Returns a plist (:host HOST :user USER :token TOKEN) or signals an error."
  (unless betterjira-host
    (error "betterjira-host is not set"))
  (let ((found (car (auth-source-search :host betterjira-host
                                        :require '(:user :secret)
                                        :max 1))))
    (unless found
      (error "No credentials found in authinfo for host: %s" betterjira-host))
    (let ((user   (plist-get found :user))
          (secret (plist-get found :secret)))
      (list :host betterjira-host
            :user user
            :token (if (functionp secret) (funcall secret) secret)))))

(defun betterjira--auth-header ()
  "Return the Base64-encoded Basic Auth header value."
  (let* ((creds (betterjira--get-credentials))
         (user  (plist-get creds :user))
         (token (plist-get creds :token)))
    (concat "Basic " (base64-encode-string (concat user ":" token) t))))

(defconst betterjira--issue-fields
  "summary,status,assignee,priority,parent,description,customfield_10017"
  "Comma-separated fields to request from the Jira API.
customfield_10017 is typically the epic link on Jira Cloud.")

;;; --- API ---

(defun betterjira--api-url (endpoint)
  "Build a full Jira REST API URL for ENDPOINT.
ENDPOINT should start with a slash, e.g. \"/rest/api/2/search\".
The host is retrieved from authinfo."
  (let ((host (plist-get (betterjira--get-credentials) :host)))
    (concat "https://" host endpoint)))

(defun betterjira--fetch-issues (&optional max-results)
  "Fetch issues for `betterjira-project-key'.
Returns the parsed JSON response.  MAX-RESULTS defaults to `betterjira-max-results'."
  (unless betterjira-project-key
    (error "betterjira-project-key is not set"))
  (let* ((max-results (or max-results betterjira-max-results))
         (jql (url-hexify-string
               (format "project = %s ORDER BY created DESC" betterjira-project-key)))
         (fields (url-hexify-string betterjira--issue-fields))
         (url (betterjira--api-url
               (format "/rest/api/3/search/jql?jql=%s&maxResults=%d&fields=%s"
                       jql max-results fields)))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(betterjira--auth-header))
            ("Content-Type"  . "application/json"))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (let ((status-code (string-to-number (match-string 1))))
        (re-search-forward "\n\n")
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (body (json-read)))
          (unless (= status-code 200)
            (let ((errors (or (alist-get 'errorMessages body)
                              (alist-get 'errors body))))
              (error "Jira API error (%d): %s" status-code errors)))
          body)))))

;;; --- Debug ---

(defun betterjira-debug-fetch ()
  "Debug: show the raw URL and full response from Jira."
  (interactive)
  (unless betterjira-project-key
    (error "betterjira-project-key is not set"))
  (let* ((jql (url-hexify-string
               (format "project = %s ORDER BY updated DESC" betterjira-project-key)))
         (fields (url-hexify-string betterjira--issue-fields))
         (url (betterjira--api-url
               (format "/rest/api/3/search/jql?jql=%s&maxResults=5&fields=%s"
                       jql fields)))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(betterjira--auth-header))
            ("Content-Type"  . "application/json"))))
    (message "Request URL: %s" url)
    (with-current-buffer (url-retrieve-synchronously url t)
      (let ((buf (get-buffer-create "*BetterJira-Debug*")))
        (copy-to-buffer buf (point-min) (point-max))
        (pop-to-buffer buf)
        (goto-char (point-min))))))

(defun betterjira-debug-transitions ()
  "Debug: show raw transitions response for the issue at point."
  (interactive)
  (let* ((issue-key (betterjira--issue-key-at-point))
         (url (betterjira--api-url
               (format "/rest/api/3/issue/%s/transitions?expand=transition.fields"
                       issue-key)))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(betterjira--auth-header))
            ("Content-Type"  . "application/json"))))
    (message "Request URL: %s" url)
    (with-current-buffer (url-retrieve-synchronously url t)
      (let ((buf (get-buffer-create "*BetterJira-Debug*")))
        (copy-to-buffer buf (point-min) (point-max))
        (pop-to-buffer buf)
        (goto-char (point-min))))))

;;; --- Cache (SQLite) ---

(defconst betterjira--cache-file
  (expand-file-name "~/.betterjiracache")
  "Path to the SQLite cache database.")

(defvar betterjira--db nil
  "SQLite database connection for the cache.")

(defun betterjira--db ()
  "Return the open SQLite database handle, creating it if needed."
  (unless (and betterjira--db (sqlitep betterjira--db))
    (setq betterjira--db (sqlite-open betterjira--cache-file))
    (sqlite-execute betterjira--db
                    "CREATE TABLE IF NOT EXISTS statuses (
                       project TEXT NOT NULL,
                       name    TEXT NOT NULL,
                       PRIMARY KEY (project, name))"))
  betterjira--db)

(defun betterjira--cache-get-statuses ()
  "Return cached statuses for `betterjira-project-key', or nil if empty."
  (let ((rows (sqlite-select (betterjira--db)
                             "SELECT name FROM statuses WHERE project = ?"
                             (list betterjira-project-key))))
    (mapcar #'car rows)))

(defun betterjira--cache-set-statuses (statuses)
  "Store STATUSES (list of strings) in the cache for `betterjira-project-key'."
  (let ((db (betterjira--db)))
    (sqlite-execute db "DELETE FROM statuses WHERE project = ?"
                    (list betterjira-project-key))
    (dolist (s statuses)
      (sqlite-execute db "INSERT INTO statuses (project, name) VALUES (?, ?)"
                      (list betterjira-project-key s)))))

(defun betterjira--fetch-statuses-from-api ()
  "Fetch all statuses for `betterjira-project-key' from the Jira API.
Returns a list of status name strings."
  (unless betterjira-project-key
    (error "betterjira-project-key is not set"))
  (let* ((url (betterjira--api-url
               (format "/rest/api/3/project/%s/statuses" betterjira-project-key)))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(betterjira--auth-header))
            ("Content-Type"  . "application/json"))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (let ((status-code (string-to-number (match-string 1))))
        (re-search-forward "\n\n")
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (body (json-read)))
          (unless (= status-code 200)
            (error "Jira API error (%d): %s" status-code body))
          (delete-dups
           (mapcar (lambda (s) (alist-get 'name s))
                   (apply #'append
                          (mapcar (lambda (it) (alist-get 'statuses it))
                                  body)))))))))

(defun betterjira--get-statuses ()
  "Return statuses for `betterjira-project-key', using cache when available.
Fetches from the API and caches on first call."
  (or (betterjira--cache-get-statuses)
      (let ((statuses (betterjira--fetch-statuses-from-api)))
        (betterjira--cache-set-statuses statuses)
        statuses)))

(defun betterjira-refresh-statuses ()
  "Force re-fetch statuses from Jira and update the cache."
  (interactive)
  (message "Refreshing statuses for %s..." betterjira-project-key)
  (let ((statuses (betterjira--fetch-statuses-from-api)))
    (betterjira--cache-set-statuses statuses)
    (message "Cached %d statuses: %s" (length statuses)
             (string-join statuses ", "))))

(defun betterjira--fetch-issues-by-status (status &optional max-results)
  "Fetch issues for `betterjira-project-key' filtered by STATUS.
Returns the parsed JSON response.  MAX-RESULTS defaults to `betterjira-max-results'."
  (unless betterjira-project-key
    (error "betterjira-project-key is not set"))
  (let* ((max-results (or max-results betterjira-max-results))
         (jql (url-hexify-string
               (format "project = %s AND status = \"%s\" ORDER BY created DESC"
                       betterjira-project-key status)))
         (fields (url-hexify-string betterjira--issue-fields))
         (url (betterjira--api-url
               (format "/rest/api/3/search/jql?jql=%s&maxResults=%d&fields=%s"
                       jql max-results fields)))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(betterjira--auth-header))
            ("Content-Type"  . "application/json"))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (let ((status-code (string-to-number (match-string 1))))
        (re-search-forward "\n\n")
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (body (json-read)))
          (unless (= status-code 200)
            (let ((errors (or (alist-get 'errorMessages body)
                              (alist-get 'errors body))))
              (error "Jira API error (%d): %s" status-code errors)))
          body)))))

;;; --- Display ---

(defun betterjira--issue-url (key)
  "Return the browse URL for issue KEY."
  (format "https://%s/browse/%s" betterjira-host key))

(defcustom betterjira-status-org-state-alist
  '(("To Do"       . "TODO")
    ("Open"        . "TODO")
    ("Backlog"     . "BACKLOG")
    ("In Progress" . "IN-PROGRESS")
    ("In Review"   . "IN-REVIEW")
    ("Done"        . "DONE")
    ("Closed"      . "DONE")
    ("Resolved"    . "DONE"))
  "Alist mapping Jira status names to Org TODO states.
If a status is not found here, it is uppercased and used directly."
  :type '(alist :key-type string :value-type string)
  :group 'betterjira)

(defun betterjira--status-to-org-state (status)
  "Map a Jira STATUS string to an Org TODO keyword."
  (or (cdr (assoc status betterjira-status-org-state-alist))
      (upcase (replace-regexp-in-string " " "-" (or status "TODO")))))

(defun betterjira--org-todo-keywords ()
  "Return a TODO keyword sequence derived from `betterjira-status-org-state-alist'."
  (let* ((all-states (mapcar #'cdr betterjira-status-org-state-alist))
         (done-states '("DONE"))
         (active-states (seq-uniq (seq-remove (lambda (s) (member s done-states)) all-states))))
    (list (concat (string-join active-states " ")
                  " | "
                  (string-join done-states " ")))))

(defun betterjira--extract-text (adf-node)
  "Recursively extract plain text from an Atlassian Document Format NODE."
  (cond
   ((null adf-node) "")
   ((stringp adf-node) adf-node)
   ((and (listp adf-node) (alist-get 'text adf-node))
    (alist-get 'text adf-node))
   ((and (listp adf-node) (alist-get 'content adf-node))
    (let ((content (alist-get 'content adf-node)))
      (mapconcat #'betterjira--extract-text
                 (if (vectorp content) (append content nil) content)
                 "")))
   ((and (listp adf-node) (alist-get 'type adf-node))
    (let ((type (alist-get 'type adf-node))
          (content (alist-get 'content adf-node)))
      (concat (if (member type '("paragraph" "heading" "bulletList" "orderedList" "listItem"))
                  ""
                "")
              (mapconcat #'betterjira--extract-text
                         (if (vectorp content) (append content nil) content)
                         "")
              (if (member type '("paragraph" "heading" "listItem"))
                  "\n"
                ""))))
   (t "")))

(defun betterjira--fetch-pr-urls (issue-id)
  "Fetch pull request URLs linked to ISSUE-ID via the dev-status API.
Returns a list of (URL . TITLE) cons cells, or nil."
  (let* ((url (betterjira--api-url
               (format "/rest/dev-status/latest/issue/detail?issueId=%s&applicationType=GitHub&dataType=pullrequest"
                       issue-id)))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(betterjira--auth-header))
            ("Content-Type"  . "application/json"))))
    (condition-case nil
        (with-current-buffer (url-retrieve-synchronously url t)
          (goto-char (point-min))
          (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
          (let ((status-code (string-to-number (match-string 1))))
            (re-search-forward "\n\n")
            (when (= status-code 200)
              (let* ((json-object-type 'alist)
                     (json-array-type 'list)
                     (body (json-read))
                     (detail (alist-get 'detail body))
                     (prs (apply #'append
                                 (mapcar (lambda (d)
                                           (alist-get 'pullRequests d))
                                         detail))))
                (mapcar (lambda (pr)
                          (cons (alist-get 'url pr)
                                (alist-get 'name pr)))
                        prs)))))
      (error nil))))

(defun betterjira--format-issue-org (issue &optional level)
  "Format a single ISSUE alist as an Org heading.
LEVEL is the heading depth (number of stars); defaults to 1."
  (let* ((level   (or level 1))
         (stars   (make-string level ?*))
         (indent  (make-string (1+ level) ?\s))
         (key     (alist-get 'key issue))
         (fields  (alist-get 'fields issue))
         (summary (alist-get 'summary fields))
         (status  (alist-get 'name (alist-get 'status fields)))
         (priority-data (alist-get 'priority fields))
         (priority (if priority-data (alist-get 'name priority-data) "None"))
         (assignee-data (alist-get 'assignee fields))
         (assignee (if assignee-data
                       (alist-get 'displayName assignee-data)
                     "Unassigned"))
         (parent-data (alist-get 'parent fields))
         (epic-key  (when parent-data (alist-get 'key parent-data)))
         (epic-summary (when parent-data
                         (alist-get 'summary (alist-get 'fields parent-data))))
         (issue-id (alist-get 'id issue))
         (description-adf (alist-get 'description fields))
         (description (if description-adf
                          (string-trim (betterjira--extract-text description-adf))
                        ""))
         (pr-links (when issue-id (betterjira--fetch-pr-urls issue-id)))
         (subtasks (when key (betterjira--fetch-subtasks key)))
         (org-status (betterjira--status-to-org-state status)))
    (concat (format "%s %s [[%s][%s]] %s\n"
                    stars
                    org-status
                    (betterjira--issue-url key)
                    key
                    (or summary "No summary"))
            (format "%s:PROPERTIES:\n" indent)
            (format "%s:STATUS:   %s\n" indent (or status "Unknown"))
            (format "%s:JIRA-PRIORITY: %s\n" indent priority)
            (format "%s:ASSIGNEE: %s\n" indent assignee)
            (if epic-key
                (format "%s:EPIC:     [[%s][%s]] %s\n"
                        indent
                        (betterjira--issue-url epic-key)
                        epic-key
                        (or epic-summary ""))
              (format "%s:EPIC:     None\n" indent))
            (if pr-links
                (mapconcat (lambda (pr)
                             (format "%s:PR:       [[%s][%s]]\n"
                                     indent
                                     (car pr)
                                     (or (cdr pr) "Link")))
                           pr-links "")
              (format "%s:PR:       None\n" indent))
            (format "%s:END:\n" indent)
            (if (string-empty-p description)
                ""
              (concat "\n"
                      (mapconcat (lambda (line) (concat indent line))
                                 (split-string description "\n")
                                 "\n")
                      "\n"))
            (if subtasks
                (concat (unless (string-empty-p description) "")
                        (mapconcat (lambda (sub)
                                     (let* ((sf (alist-get 'fields sub))
                                            (ss (alist-get 'summary sf))
                                            (st (alist-get 'name (alist-get 'status sf)))
                                            (done (member st '("Done" "Closed" "Resolved"))))
                                       (format "%s- [%s] %s" indent (if done "X" " ") ss)))
                                   subtasks "\n")
                        "\n")
              ""))))

(defun betterjira-list-issues ()
  "Prompt for a status, then fetch and display matching issues in an Org buffer."
  (interactive)
  (let* ((statuses (betterjira--get-statuses))
         (choices  (cons "All" statuses))
         (selected (completing-read "Status: " choices nil t))
         (all-p    (string= selected "All")))
    (message "Fetching %s issues for %s..."
             (if all-p "all" selected) betterjira-project-key)
    (let* ((response (if all-p
                         (betterjira--fetch-issues)
                       (betterjira--fetch-issues-by-status selected)))
           (issues   (alist-get 'issues response))
           (buf      (get-buffer-create "*BetterJira*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "#+TITLE: BetterJira — %s [%s]\n"
                          betterjira-project-key selected))
          (insert (format "#+TODO: %s\n" (car (betterjira--org-todo-keywords))))
          (insert "#+STARTUP: overview\n\n")
          (dolist (issue issues)
            (insert (betterjira--format-issue-org issue)))
          (goto-char (point-min)))
        (org-mode)
        (setq-local buffer-read-only t))
      (pop-to-buffer buf)
      (message "Fetched %d issues." (length issues)))))

(defun betterjira--search-issues (query &optional max-results)
  "Search issues in `betterjira-project-key' matching QUERY string.
Uses JQL `text ~ \"QUERY\"'.  MAX-RESULTS defaults to `betterjira-max-results'."
  (unless betterjira-project-key
    (error "betterjira-project-key is not set"))
  (let* ((max-results (or max-results betterjira-max-results))
         (jql (url-hexify-string
               (format "project = %s AND text ~ \"%s\" ORDER BY created DESC"
                       betterjira-project-key query)))
         (fields (url-hexify-string betterjira--issue-fields))
         (url (betterjira--api-url
               (format "/rest/api/3/search/jql?jql=%s&maxResults=%d&fields=%s"
                       jql max-results fields)))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(betterjira--auth-header))
            ("Content-Type"  . "application/json"))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (let ((status-code (string-to-number (match-string 1))))
        (re-search-forward "\n\n")
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (body (json-read)))
          (unless (= status-code 200)
            (let ((errors (or (alist-get 'errorMessages body)
                              (alist-get 'errors body))))
              (error "Jira API error (%d): %s" status-code errors)))
          body)))))

(defun betterjira-search ()
  "Prompt for a search string and display matching issues in an Org buffer."
  (interactive)
  (let ((query (read-string "Search issues: ")))
    (when (string-empty-p query)
      (error "Search query cannot be empty"))
    (message "Searching for \"%s\" in %s..." query betterjira-project-key)
    (let* ((response (betterjira--search-issues query))
           (issues   (alist-get 'issues response))
           (buf      (get-buffer-create "*BetterJira-Search*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "#+TITLE: BetterJira — %s — Search: %s\n"
                          betterjira-project-key query))
          (insert (format "#+TODO: %s\n" (car (betterjira--org-todo-keywords))))
          (insert "#+STARTUP: overview\n\n")
          (if issues
              (dolist (issue issues)
                (insert (betterjira--format-issue-org issue)))
            (insert "No results found.\n"))
          (goto-char (point-min)))
        (org-mode)
        (setq-local buffer-read-only t))
      (pop-to-buffer buf)
      (message "Found %d issues." (length issues)))))

(defun betterjira--fetch-issue-types ()
  "Fetch issue types for `betterjira-project-key'.
Returns a list of alists with keys `name' and `id'."
  (unless betterjira-project-key
    (error "betterjira-project-key is not set"))
  (let* ((url (betterjira--api-url
               (format "/rest/api/3/issue/createmeta/%s/issuetypes"
                       betterjira-project-key)))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(betterjira--auth-header))
            ("Content-Type"  . "application/json"))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (let ((status-code (string-to-number (match-string 1))))
        (re-search-forward "\n\n")
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (body (json-read)))
          (unless (= status-code 200)
            (error "Jira API error (%d): %s" status-code body))
          (alist-get 'issueTypes body))))))

(defun betterjira--create-issue (summary description issue-type-id)
  "Create a new issue with SUMMARY, DESCRIPTION, and ISSUE-TYPE-ID.
Returns the parsed JSON response from Jira."
  (unless betterjira-project-key
    (error "betterjira-project-key is not set"))
  (let* ((url (betterjira--api-url "/rest/api/3/issue"))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(betterjira--auth-header))
            ("Content-Type"  . "application/json")))
         (payload `((fields
                     (project (key . ,betterjira-project-key))
                     (summary . ,summary)
                     (description
                      (type . "doc")
                      (version . 1)
                      (content . [((type . "paragraph")
                                   (content . [((type . "text")
                                                (text . ,description))]))]))
                     (issuetype (id . ,issue-type-id)))))
         (url-request-data (json-encode payload)))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (let ((status-code (string-to-number (match-string 1))))
        (re-search-forward "\n\n")
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (body (json-read)))
          (unless (member status-code '(200 201))
            (let ((errors (or (alist-get 'errorMessages body)
                              (alist-get 'errors body))))
              (error "Jira API error (%d): %s" status-code errors)))
          body)))))

(defun betterjira-create-issue ()
  "Prompt for issue details and create a new issue in Jira."
  (interactive)
  (let* ((issue-types (betterjira--fetch-issue-types))
         (type-names  (mapcar (lambda (it) (alist-get 'name it)) issue-types))
         (type-name   (completing-read "Issue type: " type-names nil t))
         (type-id     (alist-get 'id (seq-find (lambda (it)
                                                  (string= (alist-get 'name it) type-name))
                                                issue-types)))
         (summary     (read-string "Summary: "))
         (description (read-string "Description: ")))
    (when (string-empty-p summary)
      (error "Summary cannot be empty"))
    (message "Creating issue in %s..." betterjira-project-key)
    (let* ((response (betterjira--create-issue summary description type-id))
           (key      (alist-get 'key response))
           (url      (betterjira--issue-url key)))
      (message "Created %s: %s" key url)
      (when (y-or-n-p (format "Open %s in browser? " key))
        (browse-url url)))))

;;; --- Issue actions from Org buffer ---

(defun betterjira--issue-key-at-point ()
  "Return the Jira issue key for the Org heading at point.
Searches the heading text for a key like PROJ-123."
  (save-excursion
    (org-back-to-heading t)
    (let ((heading (org-get-heading t t t t)))
      (if (string-match "\\b\\([A-Z][A-Z0-9]+-[0-9]+\\)\\b" heading)
          (match-string 1 heading)
        (error "No Jira issue key found in heading: %s" heading)))))

(defun betterjira--get-my-account-id ()
  "Fetch the current user's Jira account ID."
  (let* ((url (betterjira--api-url "/rest/api/3/myself"))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(betterjira--auth-header))
            ("Content-Type"  . "application/json"))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (let ((status-code (string-to-number (match-string 1))))
        (re-search-forward "\n\n")
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (body (json-read)))
          (unless (= status-code 200)
            (error "Jira API error (%d): %s" status-code body))
          (alist-get 'accountId body))))))

(defun betterjira--assign-issue (issue-key account-id)
  "Assign ISSUE-KEY to the user with ACCOUNT-ID."
  (let* ((url (betterjira--api-url
               (format "/rest/api/3/issue/%s/assignee" issue-key)))
         (url-request-method "PUT")
         (url-request-extra-headers
          `(("Authorization" . ,(betterjira--auth-header))
            ("Content-Type"  . "application/json")))
         (url-request-data (json-encode `((accountId . ,account-id)))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (let ((status-code (string-to-number (match-string 1))))
        (unless (= status-code 204)
          (re-search-forward "\n\n")
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (body (json-read)))
            (error "Jira API error (%d): %s" status-code body)))))))

(defun betterjira--fetch-single-issue (issue-key)
  "Fetch a single issue by ISSUE-KEY with standard fields."
  (let* ((fields (url-hexify-string betterjira--issue-fields))
         (url (betterjira--api-url
               (format "/rest/api/3/issue/%s?fields=%s" issue-key fields)))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(betterjira--auth-header))
            ("Content-Type"  . "application/json"))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (let ((status-code (string-to-number (match-string 1))))
        (re-search-forward "\n\n")
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (body (json-read)))
          (unless (= status-code 200)
            (error "Jira API error (%d): %s" status-code body))
          body)))))

(defun betterjira--refresh-heading-at-point (issue)
  "Replace the Org heading at point with refreshed ISSUE data.
Preserves the current heading level."
  (let ((inhibit-read-only t))
    (save-excursion
      (org-back-to-heading t)
      (let ((level (org-current-level))
            (beg (point)))
        (org-end-of-subtree t t)
        (delete-region beg (point))
        (goto-char beg)
        (insert (betterjira--format-issue-org issue level))))))

(defun betterjira-assign-to-me ()
  "Assign the Jira issue at point to the current user and refresh the heading."
  (interactive)
  (let ((issue-key (betterjira--issue-key-at-point)))
    (message "Assigning %s to you..." issue-key)
    (let ((account-id (betterjira--get-my-account-id)))
      (betterjira--assign-issue issue-key account-id)
      (let ((updated-issue (betterjira--fetch-single-issue issue-key)))
        (betterjira--refresh-heading-at-point updated-issue)
        (message "Assigned %s to you." issue-key)))))

(defun betterjira--fetch-transitions (issue-key)
  "Fetch available status transitions for ISSUE-KEY.
Includes field metadata (e.g. whether resolution is required).
Returns a list of transition alists."
  (let* ((url (betterjira--api-url
               (format "/rest/api/3/issue/%s/transitions?expand=transition.fields"
                       issue-key)))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(betterjira--auth-header))
            ("Content-Type"  . "application/json"))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (let ((status-code (string-to-number (match-string 1))))
        (re-search-forward "\n\n")
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (body (json-read)))
          (unless (= status-code 200)
            (error "Jira API error (%d): %s" status-code body))
          (alist-get 'transitions body))))))

(defun betterjira--fetch-resolutions ()
  "Fetch available resolutions from Jira.
Returns a list of alists with `id' and `name' keys."
  (let* ((url (betterjira--api-url "/rest/api/3/resolution"))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(betterjira--auth-header))
            ("Content-Type"  . "application/json"))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (let ((status-code (string-to-number (match-string 1))))
        (re-search-forward "\n\n")
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (body (json-read)))
          (unless (= status-code 200)
            (error "Jira API error (%d): %s" status-code body))
          body)))))

(defun betterjira--transition-needs-resolution-p (transition)
  "Return non-nil if TRANSITION likely requires a resolution.
Checks if the target status category is \"done\"."
  (let* ((to (alist-get 'to transition))
         (category (alist-get 'statusCategory to))
         (key (alist-get 'key category)))
    (string= key "done")))

(defun betterjira--transition-issue (issue-key transition-id &optional resolution-name)
  "Transition ISSUE-KEY to the state identified by TRANSITION-ID.
If RESOLUTION-NAME is non-nil, include it in the transition."
  (let* ((url (betterjira--api-url
               (format "/rest/api/3/issue/%s/transitions" issue-key)))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(betterjira--auth-header))
            ("Content-Type"  . "application/json")))
         (payload (if resolution-name
                     `((transition (id . ,transition-id))
                       (fields (resolution (name . ,resolution-name))))
                   `((transition (id . ,transition-id)))))
         (url-request-data (json-encode payload)))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (let ((status-code (string-to-number (match-string 1))))
        (unless (= status-code 204)
          (re-search-forward "\n\n")
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (body (json-read)))
            (error "Jira API error (%d): %s" status-code body)))))))

(defun betterjira-change-status ()
  "Prompt for a new status and transition the issue at point.
Only shows valid transitions for the issue's current state.
If the transition requires a resolution, prompts for one."
  (interactive)
  (let* ((issue-key   (betterjira--issue-key-at-point))
         (transitions (betterjira--fetch-transitions issue-key))
         (names       (mapcar (lambda (tr) (alist-get 'name tr)) transitions)))
    (unless transitions
      (error "No transitions available for %s" issue-key))
    (let* ((selected (completing-read
                      (format "Transition %s to: " issue-key) names nil t))
           (transition (seq-find (lambda (tr)
                                   (string= (alist-get 'name tr) selected))
                                 transitions))
           (transition-id (alist-get 'id transition))
           (resolution-name
            (when (betterjira--transition-needs-resolution-p transition)
              (let* ((resolutions (betterjira--fetch-resolutions))
                     (res-names (mapcar (lambda (r) (alist-get 'name r))
                                        resolutions)))
                (completing-read "Resolution: " res-names nil t)))))
      (message "Transitioning %s to %s..." issue-key selected)
      (betterjira--transition-issue issue-key transition-id resolution-name)
      (let ((updated-issue (betterjira--fetch-single-issue issue-key)))
        (betterjira--refresh-heading-at-point updated-issue)
        (message "Transitioned %s to %s." issue-key selected)))))

(defun betterjira--org-heading-summary ()
  "Return the plain text summary from the Org heading at point.
Strips TODO keywords and any existing Jira links."
  (save-excursion
    (org-back-to-heading t)
    (let ((heading (org-get-heading t t t t)))
      ;; Strip any existing [[...][KEY]] link
      (if (string-match "\\[\\[[^]]*\\]\\[[^]]*\\]\\]\\s-*" heading)
          (replace-match "" t t heading)
        heading))))

(defun betterjira--org-body-text ()
  "Return the body text of the Org entry at point.
Excludes the heading line and any properties drawer."
  (save-excursion
    (org-back-to-heading t)
    (forward-line 1)
    ;; Skip properties drawer if present
    (when (looking-at "\\s-*:PROPERTIES:")
      (re-search-forward ":END:" nil t)
      (forward-line 1))
    (let ((beg (point))
          (end (save-excursion
                 (org-end-of-subtree t t)
                 (point))))
      (string-trim
       (replace-regexp-in-string
        "^  " ""
        (buffer-substring-no-properties beg end))))))

(defun betterjira-create-issue-from-heading ()
  "Create a Jira issue from the Org heading at point.
Uses the heading text as the summary and the body as the description.
Prompts for issue type, then creates the issue and replaces the
heading in place with the full Jira issue data."
  (interactive)
  (let* ((summary     (betterjira--org-heading-summary))
         (description (betterjira--org-body-text))
         (issue-types (betterjira--fetch-issue-types))
         (type-names  (mapcar (lambda (it) (alist-get 'name it)) issue-types))
         (type-name   (completing-read "Issue type: " type-names nil t))
         (type-id     (alist-get 'id (seq-find (lambda (it)
                                                  (string= (alist-get 'name it) type-name))
                                                issue-types))))
    (when (string-empty-p summary)
      (error "Heading has no summary text"))
    (message "Creating %s \"%s\" in %s..." type-name summary betterjira-project-key)
    (let* ((response (betterjira--create-issue summary description type-id))
           (key      (alist-get 'key response))
           (issue    (betterjira--fetch-single-issue key)))
      (betterjira--refresh-heading-at-point issue)
      (message "Created %s: %s" key (betterjira--issue-url key)))))

(defun betterjira--slugify (text)
  "Convert TEXT to a lowercase kebab-case slug suitable for a branch name."
  (let* ((down (downcase text))
         (clean (replace-regexp-in-string "[^a-z0-9 ]" "" down))
         (trimmed (string-trim clean))
         (slug (replace-regexp-in-string "\\s-+" "-" trimmed)))
    slug))

(defun betterjira-copy-branch-name ()
  "Generate a git branch name from the Jira issue at point and copy it.
Produces a branch like: AISHOPPING-901/deploy-prod-vespa-application"
  (interactive)
  (let* ((issue-key (betterjira--issue-key-at-point))
         (summary (save-excursion
                    (org-back-to-heading t)
                    (let ((heading (org-get-heading t t t t)))
                      ;; Strip [[...][KEY]] link
                      (when (string-match "\\[\\[[^]]*\\]\\[[^]]*\\]\\]\\s-*" heading)
                        (setq heading (replace-match "" t t heading)))
                      heading)))
         (branch (format "%s-%s" issue-key (betterjira--slugify summary))))
    (kill-new branch)
    (message "Copied: %s" branch)))

(defun betterjira--update-issue (issue-key summary description priority-name)
  "Update ISSUE-KEY on Jira with SUMMARY, DESCRIPTION, and PRIORITY-NAME."
  (let* ((url (betterjira--api-url
               (format "/rest/api/3/issue/%s" issue-key)))
         (url-request-method "PUT")
         (url-request-extra-headers
          `(("Authorization" . ,(betterjira--auth-header))
            ("Content-Type"  . "application/json")))
         (payload `((fields
                     (summary . ,summary)
                     (priority (name . ,priority-name))
                     (description
                      (type . "doc")
                      (version . 1)
                      (content . [((type . "paragraph")
                                   (content . [((type . "text")
                                                (text . ,description))]))])))))
         (url-request-data (json-encode payload)))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (let ((status-code (string-to-number (match-string 1))))
        (unless (= status-code 204)
          (re-search-forward "\n\n")
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (body (json-read)))
            (error "Jira API error (%d): %s" status-code body)))))))

(defun betterjira-push-issue ()
  "Push the Org entry at point back to Jira.
Updates the summary (heading), description (body), and priority
on the Jira ticket, then refreshes the entry with the latest data."
  (interactive)
  (let* ((issue-key   (betterjira--issue-key-at-point))
         (summary     (betterjira--org-heading-summary))
         (description (betterjira--org-body-text))
         (priority    (or (org-entry-get (point) "PRIORITY")
                          "Normal")))
    (when (string-empty-p summary)
      (error "Heading has no summary text"))
    (message "Pushing %s to Jira..." issue-key)
    (betterjira--update-issue issue-key summary description priority)
    (let ((issue (betterjira--fetch-single-issue issue-key)))
      (betterjira--refresh-heading-at-point issue)
      (message "Pushed %s to Jira." issue-key))))

(defun betterjira-refresh-issue ()
  "Refresh the Jira issue at point with the latest data."
  (interactive)
  (let* ((issue-key (betterjira--issue-key-at-point)))
    (message "Refreshing %s..." issue-key)
    (let ((issue (betterjira--fetch-single-issue issue-key)))
      (betterjira--refresh-heading-at-point issue)
      (message "Refreshed %s." issue-key))))

(provide 'betterjira)

;;; betterjira.el ends here
