;;; tatami.el --- Emacs client for Futon headless API -*- lexical-binding: t; -*-

;; Copyright (c) 2025

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; Emacs helper for the Futon headless API. Capabilities:
;;  * ensure the server is running (optionally start it)
;;  * retrieve the :me summary
;;  * post turns and return the focus header
;;
;; Usage:
;;   (require 'tatami)
;;   (setq tatami-start-command '("clojure" "-M:run-m"))
;;   (setq tatami-start-directory "/path/to/futon1")
;;   (tatami-me-summary)
;;
;; Configure `tatami-base-url`, `tatami-start-command`, and `tatami-profile`
;; as needed.  Inspect `tatami-last-error` for detailed failure reasons.

;;; Code:

(require 'url)
(require 'json)
(require 'subr-x)
(require 'cl-lib)

(defgroup tatami nil
  "Interact with the Futon headless API."
  :group 'external
  :prefix "tatami-")

(defcustom tatami-base-url "http://localhost:8080"
  "Root URL for the headless API (no trailing slash)."
  :type 'string
  :group 'tatami)

(defcustom tatami-start-command '("clojure" "-M:run-m")
  "Command (list of strings) used to start the headless server."
  :type '(repeat string)
  :group 'tatami)

(defcustom tatami-start-directory nil
  "Directory used when launching the headless server.
When nil, use the current `default-directory` at invocation time."
  :type '(choice (const :tag "Current directory" nil) directory)
  :group 'tatami)

(defcustom tatami-profile nil
  "Optional profile name sent via the X-Profile header."
  :type '(choice (const :tag "Default profile" nil) string)
  :group 'tatami)

(defcustom tatami-startup-wait 15
  "Seconds to wait for the server to become reachable after launch."
  :type 'integer
  :group 'tatami)

(defcustom tatami-data-directory nil
  "Directory that stores the headless API data files.

When nil an absolute path to `data` is inferred relative to
`tatami-start-directory` (or the current `default-directory` when
`tatami-start-directory` is unset)."
  :type '(choice (const :tag "Infer from project" nil) directory)
  :group 'tatami)

(defcustom tatami-verbose nil
  "When non-nil, log request/response bodies in the *headless-api-server* buffer."
  :type 'boolean
  :group 'tatami)

(defvar tatami--server-process nil
  "The process object for a server started by Emacs.")

(defvar tatami--last-error nil
  "Holds the most recent error message.")

(defvar tatami--last-command nil
  "Tracks the command vector used to start the server.")

(defun tatami--write-log (fmt &rest args)
  "Write formatted message into the *headless-api-server* buffer."
  (let ((buffer (get-buffer-create "*headless-api-server*")))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (apply #'format fmt args) "\n"))))

(defun tatami--note-error (fmt &rest args)
  (setq tatami--last-error (apply #'format fmt args)))

(defun tatami-last-error ()
  "Return the last recorded Tatami error message."
  tatami--last-error)

(defun tatami--default-data-directory ()
  (let* ((base (or tatami-data-directory
                   (and tatami-start-directory
                        (expand-file-name "data" tatami-start-directory))
                   (expand-file-name "data" default-directory))))
    (expand-file-name base)))

(defun tatami--make-url (path)
  (concat (string-remove-suffix "/" tatami-base-url) path))

(defun tatami--profile-header ()
  (when tatami-profile
    (cons "X-Profile" tatami-profile)))

(defun tatami--cleanup-buffer (buffer)
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(defun tatami--request (method path &optional data expect-json)
  "Perform an HTTP request and return the response.
METHOD is coerced to an uppercase string.
PATH is appended to `tatami-base-url`.
DATA, when provided, is JSON encoded.
EXPECT-JSON controls whether the body is parsed as JSON.

Errors are signalled with a short message; the detailed payload is preserved via
`tatami-last-error`."
  (let* ((url-request-method (if (stringp method)
                                 method
                               (upcase (format "%s" method))))
         (url-request-data (when data
                             (encode-coding-string (json-encode data) 'utf-8)))
         (url-request-extra-headers
          (append (list (cons "Content-Type" "application/json; charset=utf-8")
                        (cons "Accept-Charset" "utf-8"))
                  (when-let ((hdr (tatami--profile-header))) (list hdr))))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (buffer (url-retrieve-synchronously (tatami--make-url path) t t tatami-startup-wait)))
    ;; If we time out or can't connect, bail out cleanly *before* touching the buffer.
    (unless buffer
      (tatami--note-error "No response from headless API: %s %s (timeout=%ss)"
                          method path tatami-startup-wait)
      (error "No response from headless API"))
    (unwind-protect
        (with-current-buffer buffer
          (let ((status (or url-http-response-status 0)))
            (goto-char (or (and (boundp 'url-http-end-of-headers)
                                url-http-end-of-headers)
                           (search-forward "\n\n" nil t)
                           (point-min)))
            (let* ((body (buffer-substring-no-properties (point) (point-max)))
                   (decoded-body (decode-coding-string body 'utf-8)))
              (when tatami-verbose
                (tatami--write-log "%s %s\nrequest: %s\nresponse: %s"
                                   method
                                   path
                                   (if data (json-encode data) "<empty>")
                                   (string-trim decoded-body)))
              (unless (= status 200)
                (tatami--note-error "API request failed (%s) %s" status decoded-body)
                (error "API request failed (%s)" status))
              (if expect-json
                  (if (string-empty-p decoded-body)
                      nil
                    (json-parse-string decoded-body :object-type 'alist :array-type 'list))
                decoded-body))))
      (tatami--cleanup-buffer buffer))))

(defun tatami--next-sentence (&optional bounds-only)
  "Return the trimmed sentence at point.
When BOUNDS-ONLY is non-nil, return a cons of (START . END) instead of the
string.  If no sentence remains, return nil."
  (let ((sentence-end-double-space nil))
    (skip-chars-forward " \t\n")
    (let ((start (point)))
      (cond
       ((>= start (point-max)) nil)
       (t
        (forward-sentence)
        (let ((end (point)))
          (when (= start end)
            (goto-char (point-max))
            (setq end (point)))
          (if bounds-only
              (cons start end)
            (let ((sentence (string-trim (buffer-substring-no-properties start end))))
              (unless (string-empty-p sentence)
                sentence)))))))))

(defun tatami--collect-sentences (text)
  "Split TEXT into trimmed sentences."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((sentence-end-double-space nil)
          sentences)
      (while (< (point) (point-max))
        (let ((sentence (tatami--next-sentence)))
          (when sentence
            (push sentence sentences))))
      (nreverse sentences))))



(defun tatami-stop-server ()
  "Stop a headless server started by Emacs."
  (interactive)
  (let* ((candidates (remove nil
                             (list tatami--server-process
                                   (get-process "headless-api-server"))))
         (killed 0))
    (dolist (proc (cl-delete-duplicates candidates :test #'eq))
      (when (process-live-p proc)
        (delete-process proc)
        (setq killed (1+ killed))))
    (setq tatami--server-process nil)
    (if (> killed 0)
        (progn
          (tatami--note-error "Server stopped explicitly")
          (message "Tatami server stopped"))
      (message "Tatami server not running"))))

(defun tatami-reset-storage (&optional directory)
  "Delete the headless API data directory.
When called interactively without a prefix argument the directory is inferred
from `tatami-data-directory` / `tatami-start-directory`.  With a prefix
argument prompt for the directory to delete.  The server process is stopped
before deletion when it was started from Emacs."
  (interactive)
  (let* ((default-dir (tatami--default-data-directory))
         (target (expand-file-name (or directory
                                       (if current-prefix-arg
                                           (read-directory-name
                                            "Delete data directory: "
                                            default-dir nil t)
                                         default-dir)))))
    (if (file-directory-p target)
        (when (yes-or-no-p (format "Really delete data directory %s? " target))
          (tatami-stop-server)
          (delete-directory target t)
          (make-directory target t)
          (message "Reset headless data directory %s" target))
      (if (called-interactively-p 'interactive)
          (message "No data directory at %s" target)
        (tatami--note-error "No data directory at %s" target)))))

(defun tatami--process-sentinel (proc event)
  "Internal sentinel to report when PROC terminates.
EVENT is the raw event string from `set-process-sentinel'."
  (let ((trimmed (string-trim (or event ""))))
    (when (eq proc tatami--server-process)
      (setq tatami--server-process nil))
    (cond
     ((and (stringp trimmed) (string-match-p "finished" trimmed))
      (message "Tatami server exited: %s" trimmed))
     ((and (not (process-live-p proc)) (not (string-empty-p trimmed)))
      (tatami--note-error "Server process %s" trimmed)
      (message "Tatami server process stopped: %s" trimmed)))))

(defun tatami-start-server ()
  "Start the headless server if it is not already running."
  (interactive)
  (setq tatami--last-command tatami-start-command)
  (tatami--launch-server t))

(defvar tatami-health-path "/healthz")

(defun tatami--probe (url)
  "Return non-nil if URL is reachable (any HTTP status)."
  (condition-case e
      (with-current-buffer (url-retrieve-synchronously url t t 2)
        (prog1 t (kill-buffer (current-buffer))))
    (error (setq tatami--last-error (error-message-string e)) nil)))

(defun tatami--server-running-p ()
  "Return non-nil if the headless server responds."
  (let ((base (replace-regexp-in-string "/\\'" "" tatami-base-url)))
    (or (tatami--probe (concat base tatami-health-path))
        (tatami--probe (concat base "/")))))

(defun tatami--launch-server (&optional interactive)
  "Ensure a server is running, launching it when necessary.
INTERACTIVE controls messaging."
  (if (tatami--server-running-p)
      (progn (when interactive (message "Headless server already running")) t)
    (let* ((base-dir (or tatami-start-directory default-directory))
           (default-directory (file-name-as-directory (expand-file-name base-dir)))
           (command tatami-start-command)
           (process-name "headless-api-server")
           (buf (get-buffer-create "*headless-api-server*"))
           (data-dir (tatami--default-data-directory))
           (env (append (when tatami-profile
                          (list (format "ALPHA_PROFILE=%s" tatami-profile)))
                        (when data-dir
                          (list (format "BASIC_CHAT_DATA_DIR=%s" data-dir)))
                        process-environment)))
      (unless (and (listp command) (car command))
        (error "`tatami-start-command' must be a non-empty list"))
      (when interactive
        (message "Starting headless server using %s (ALPHA_PROFILE=%s data-dir=%s)"
                 (string-join command " ")
                 (or tatami-profile "default")
                 data-dir))
      ;; Optional: some users prefer this for faster I/O, less PTY weirdness:
      ;; (let ((process-connection-type nil) ...)
      (let ((process-environment env))
        (setq tatami--server-process
              (apply #'start-process process-name buf command)))
      (set-process-query-on-exit-flag tatami--server-process nil)
      (set-process-sentinel tatami--server-process
                            (lambda (_proc _event) ;; keep simple; we poll below
                              nil))
      ;; Robust wait: up to ~15s with exponential backoff.
      (let ((attempts 0)
            (max-attempts 8)   ;; ~1.5 + 3 + 4.5 + ... ≈ 15s
            (sleep 1.5)
            (ready nil))
        (while (and (< attempts max-attempts)
                    (not (setq ready (tatami--server-running-p))))
          (sleep-for sleep)
          (setq attempts (1+ attempts)
                sleep (min 4.5 (* sleep 1.5))))
        (unless ready
          (when interactive
            (let ((tail (with-current-buffer buf
                          (save-excursion
                            (goto-char (point-max))
                            (buffer-substring
                             (max (point-min) (- (point-max) 4000))
                             (point-max))))))
              (message "Failed to contact headless server%s\n--- server log tail ---\n%s"
                       (if tatami--last-error (format ": %s" tatami--last-error) "")
                       (or tail "<no output>")))))
        ready))))

(defun tatami--ensure-server (&optional interactive)
  (or (tatami--server-running-p)
      (tatami--launch-server interactive)))

(defun tatami-me-summary (&optional profile)
  "Retrieve the current :me summary as plain text.
When PROFILE (string) is provided, use it instead of `tatami-profile'."
  (interactive (list (when current-prefix-arg
                       (read-string "Profile: " tatami-profile))))
  (let ((tatami-profile (or profile tatami-profile)))
    (unless (tatami--ensure-server (called-interactively-p 'interactive))
      (error "Headless server is not reachable%s"
             (if tatami--last-error
                 (format ": %s" tatami--last-error)
               "")))
    (let* ((summary (tatami--request "GET" "/api/%CE%B1/me/summary" nil nil))
           (buffer (get-buffer-create "*headless-me-summary*")))
      (with-current-buffer buffer
        (read-only-mode -1)
        (erase-buffer)
        (insert summary)
        (goto-char (point-min))
        (view-mode 1))
      (when (called-interactively-p 'interactive)
        (pop-to-buffer buffer))
      summary)))

(defun tatami-focus-header (&optional profile)
  "Fetch the current focus header as an alist.
When PROFILE is provided, override `tatami-profile'."
  (interactive (list (when current-prefix-arg
                       (read-string "Profile: " tatami-profile))))
  (let ((tatami-profile (or profile tatami-profile)))
    (unless (tatami--ensure-server (called-interactively-p 'interactive))
      (error "Headless server is not reachable%s"
             (if tatami--last-error
                 (format ": %s" tatami--last-error)
               "")))
    (let ((fh (tatami--request "GET" "/api/%CE%B1/focus-header" nil t)))
      (when (called-interactively-p 'interactive)
        (message "Focus header retrieved for profile %s" (or tatami-profile "default")))
      fh)))

(defun tatami-fetch-trails (&optional limit profile)
  "Fetch recent trail entries as an alist.
When LIMIT is provided, cap the number of entries (default 10).
PROFILE overrides `tatami-profile'."
  (interactive (list (when current-prefix-arg
                       (read-number "Trail limit: " 10))
                     (when current-prefix-arg
                       (read-string "Profile: " tatami-profile))))
  (let ((tatami-profile (or profile tatami-profile))
        (limit (or limit 10)))
    (unless (tatami--ensure-server (called-interactively-p 'interactive))
      (error "Headless server is not reachable%s"
             (if tatami--last-error
                 (format ": %s" tatami--last-error)
               "")))
    (let* ((path (format "/api/%CE%B1/trails?limit=%d" limit))
           (resp (tatami--request "GET" path nil t))
           (trails (alist-get 'trails resp)))
      trails)))

(defun tatami--format-trail-entry (trail)
  (let* ((session (alist-get 'session-id trail))
         (turn (alist-get 'turn-id trail))
         (intent (or (alist-get 'intent trail) "<intent missing>"))
         (timestamp (alist-get 'timestamp trail))
         (ts (when timestamp
               (format-time-string "%Y-%m-%d %H:%M:%S"
                                   (seconds-to-time (/ (float timestamp) 1000.0))
                                   t)))
         (fruits (alist-get 'fruits trail))
         (paramitas (alist-get 'paramitas trail))
         (fruit-labels (when (seq fruits)
                         (mapconcat (lambda (item) (format "%s" (alist-get 'fruit/id item))) ", ")))
         (orb-labels (when (seq paramitas)
                       (mapconcat (lambda (item) (format "%s" (alist-get 'paramita/id item))) ", ")))
         (futons (alist-get 'futons trail))
         (protos (alist-get 'prototypes trail))
         (meta (->> (list (when ts (concat "at " ts))
                          (when fruit-labels (concat "fruits " fruit-labels))
                          (when orb-labels (concat "paramitas " orb-labels))
                          (when (seq futons) (concat "futons " (mapconcat (lambda (item) (format "%s" item)) futons ", ")))
                          (when (seq protos) (concat "prototypes " (mapconcat (lambda (item) (format "%s" item)) protos ", "))))
                     (remove #'null)
                     (string-join "; "))))
    (concat "  - " (or session "?") "/" (or turn "?")
            " — " intent
            (when (and meta (not (string-empty-p meta)))
              (format " (%s)" meta)))))

(defun tatami-show-trails (&optional limit profile)
  "Display recent trail entries in a dedicated buffer."
  (interactive (list (when current-prefix-arg
                       (read-number "Trail limit: " 10))
                     (when current-prefix-arg
                       (read-string "Profile: " tatami-profile))))
  (let* ((data (tatami-fetch-trails (or limit 10) profile))
         (trails (or data '()))
         (buffer (get-buffer-create "*Tatami Trails*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "Trails (limit %d)\n\n" (or limit 10)))
      (if (seq trails)
          (dolist (trail trails)
            (insert (tatami--format-trail-entry trail) "\n"))
        (insert "(none)\n"))
      (goto-char (point-min))
      (view-mode 1))
    (when (called-interactively-p 'interactive)
      (pop-to-buffer buffer))
    trails))

(defun tatami-send-turn (text &optional profile)
  "Post TEXT to the /api/α/turns endpoint and return the updated focus header.
When PROFILE is provided, override `tatami-profile'."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Turn text: "))
    (when current-prefix-arg
      (read-string "Profile: " tatami-profile))))
  (let ((tatami-profile (or profile tatami-profile)))
    (unless (tatami--ensure-server (called-interactively-p 'interactive))
      (error "Headless server is not reachable%s"
             (if tatami--last-error
                 (format ": %s" tatami--last-error)
               "")))
    (let ((resp (tatami--request "POST" "/api/%CE%B1/turns" `((text . ,text)) t)))
      (when (called-interactively-p 'interactive)
        (message "Turn submitted; %d entities returned" (length (alist-get 'entities resp))))
      (alist-get 'focus_header resp))))

(defun tatami-send-sentences (text &optional profile)
  "Send each sentence from TEXT as an individual turn.
When PROFILE is provided, override `tatami-profile'.  Returns the focus header
from the final turn posted."
  (interactive
    (list
     (if (use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end))
       (read-string "Text: "))
    (when current-prefix-arg
      (read-string "Profile: " tatami-profile))))
  (let* ((tatami-profile (or profile tatami-profile))
         (sentences (tatami--collect-sentences text))
         (last-header nil)
         (count 0)
         (start-time (current-time)))
    (unless sentences
      (user-error "No sentences to send"))
    (dolist (sentence sentences last-header)
      (setq last-header (tatami-send-turn sentence tatami-profile))
      (setq count (1+ count)))
    (prog1 last-header
      (message "Tatami: sent %d sentence%s in %.2fs"
               count
               (if (= count 1) "" "s")
               (float-time (time-subtract (current-time) start-time))))))

(defalias 'tatami-send-lines #'tatami-send-sentences)

(defun tatami-send-buffer-sentences (&optional profile)
  "Send every sentence in the current buffer sequentially to the API.
When PROFILE is provided, override `tatami-profile'.  Blank sentences are
ignored; the focus header from the final turn is returned."
  (interactive (list (when current-prefix-arg
                       (read-string "Profile: " tatami-profile))))
  (let ((tatami-profile (or profile tatami-profile))
        (last-header nil)
        (count 0)
        (start-time (current-time)))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((bounds (tatami--next-sentence t)))
          (if (not bounds)
              (goto-char (point-max))
            (let* ((start (car bounds))
                   (end (cdr bounds))
                   (sentence (string-trim (buffer-substring-no-properties start end))))
              (goto-char end)
              (unless (string-empty-p sentence)
                (setq last-header (tatami-send-turn sentence tatami-profile))
                (setq count (1+ count)))))))
    (when (> count 0)
      (message "Tatami: sent %d sentence%s from buffer in %.2fs"
               count
               (if (= count 1) "" "s")
               (float-time (time-subtract (current-time) start-time))))
    last-header)))

(defun tatami-send-next-sentence (&optional profile)
  "Send the sentence at point to the API and advance point.
When PROFILE is provided, override `tatami-profile'."
  (interactive (list (when current-prefix-arg
                       (read-string "Profile: " tatami-profile))))
  (let ((tatami-profile (or profile tatami-profile))
        bounds sentence)
    (while (and (setq bounds (tatami--next-sentence t))
                (string-empty-p (setq sentence (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds)))))))
    (unless bounds
      (user-error "No sentence at point"))
    (goto-char (cdr bounds))
    (tatami-send-turn sentence tatami-profile)))

;;;###autoload
(defun tatami-open-summary-buffer ()
  "Convenience command to show the :me summary in a read-only buffer."
  (interactive)
  (tatami-me-summary))

(provide 'tatami)

;;; tatami.el ends here
