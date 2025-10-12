;;; tatami1.el --- Emacs client for Futon headless API -*- lexical-binding: t; -*-

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
;;   (require 'tatami1)
;;   (setq tatami1-start-command '("clojure" "-M:run-m"))
;;   (setq tatami1-start-directory "/path/to/futon1")
;;   (tatami1-me-summary)
;;
;; Configure `tatami1-base-url`, `tatami1-start-command`, and `tatami1-profile`
;; as needed.  Inspect `tatami1-last-error` for detailed failure reasons.

;;; Code:

(require 'url)
(require 'json)
(require 'subr-x)
(require 'cl-lib)

(defgroup tatami1 nil
  "Interact with the Futon headless API."
  :group 'external
  :prefix "tatami1-")

(defcustom tatami1-base-url "http://localhost:8080"
  "Root URL for the headless API (no trailing slash)."
  :type 'string
  :group 'tatami1)

(defcustom tatami1-start-command '("clojure" "-M:run-m")
  "Command (list of strings) used to start the headless server."
  :type '(repeat string)
  :group 'tatami1)

(defcustom tatami1-start-directory nil
  "Directory used when launching the headless server.
When nil, use the current `default-directory` at invocation time."
  :type '(choice (const :tag "Current directory" nil) directory)
  :group 'tatami1)

(defcustom tatami1-profile nil
  "Optional profile name sent via the X-Profile header."
  :type '(choice (const :tag "Default profile" nil) string)
  :group 'tatami1)

(defcustom tatami1-startup-wait 10
  "Seconds to wait for the server to become reachable after launch."
  :type 'integer
  :group 'tatami1)

(defcustom tatami1-data-directory nil
  "Directory that stores the headless API data files.

When nil an absolute path to `data` is inferred relative to
`tatami1-start-directory` (or the current `default-directory` when
`tatami1-start-directory` is unset)."
  :type '(choice (const :tag "Infer from project" nil) directory)
  :group 'tatami1)

(defvar tatami1--server-process nil
  "The process object for a server started by Emacs.")

(defvar tatami1--last-error nil
  "Holds the most recent error message.")

(defvar tatami1--last-command nil
  "Tracks the command vector used to start the server.")

(defun tatami1--note-error (fmt &rest args)
  (setq tatami1--last-error (apply #'format fmt args)))

(defun tatami1-last-error ()
  "Return the last recorded Tatami1 error message."
  tatami1--last-error)

(defun tatami1--default-data-directory ()
  (let* ((base (or tatami1-data-directory
                   (and tatami1-start-directory
                        (expand-file-name "data" tatami1-start-directory))
                   (expand-file-name "data" default-directory))))
    (expand-file-name base)))

(defun tatami1--make-url (path)
  (concat (string-remove-suffix "/" tatami1-base-url) path))

(defun tatami1--profile-header ()
  (when tatami1-profile
    (cons "X-Profile" tatami1-profile)))

(defun tatami1--cleanup-buffer (buffer)
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(defun tatami1--request (method path &optional data expect-json)
  "Perform an HTTP request and return the response.
METHOD is coerced to an uppercase string.
PATH is appended to `tatami1-base-url`.
DATA, when provided, is JSON encoded.
EXPECT-JSON controls whether the body is parsed as JSON.

Errors are signalled with a short message; the detailed payload is preserved via
`tatami1-last-error`."
  (let* ((url-request-method (if (stringp method)
                                 method
                               (upcase (format "%s" method))))
         (url-request-data (when data
                             (encode-coding-string (json-encode data) 'utf-8)))
         (url-request-extra-headers
          (append (list (cons "Content-Type" "application/json; charset=utf-8")
                        (cons "Accept-Charset" "utf-8"))
                  (when-let ((hdr (tatami1--profile-header))) (list hdr))))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (buffer (url-retrieve-synchronously (tatami1--make-url path) t t tatami1-startup-wait)))
    (unless buffer
      (error "No response from headless API"))
    (unwind-protect
        (with-current-buffer buffer
          (let ((status (or url-http-response-status 0)))
            (goto-char (or (and (boundp 'url-http-end-of-headers)
                                 url-http-end-of-headers)
                           (search-forward "\n\n" nil t)
                           (point-min)))
            (let ((body (buffer-substring-no-properties (point) (point-max))))
              (unless (= status 200)
                (tatami1--note-error "API request failed (%s) %s" status body)
                (error "API request failed (%s)" status))
              (if expect-json
                  (if (string-empty-p body)
                      nil
                    (json-parse-string body :object-type 'alist :array-type 'list))
                body))))
      (tatami1--cleanup-buffer buffer))))

(defun tatami1--next-sentence (&optional bounds-only)
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

(defun tatami1--collect-sentences (text)
  "Split TEXT into trimmed sentences."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((sentence-end-double-space nil)
          sentences)
      (while (< (point) (point-max))
        (let ((sentence (tatami1--next-sentence)))
          (when sentence
            (push sentence sentences))))
      (nreverse sentences))))

(defun tatami1--server-running-p ()
  "Return non-nil when the server responds to /api/α/focus-header."
  (setq tatami1--last-error nil)
  (condition-case err
      (progn
        (tatami1--request "GET" "/api/%CE%B1/focus-header" nil t)
        t)
    (error
     (tatami1--note-error "%s" (error-message-string err))
     nil)))

(defun tatami1-stop-server ()
  "Stop a headless server started by Emacs."
  (interactive)
  (when (process-live-p tatami1--server-process)
    (delete-process tatami1--server-process)
    (setq tatami1--server-process nil)
    (message "Stopped headless server process")))

(defun tatami1-reset-storage (&optional directory)
  "Delete the headless API data directory.
When called interactively without a prefix argument the directory is inferred
from `tatami1-data-directory` / `tatami1-start-directory`.  With a prefix
argument prompt for the directory to delete.  The server process is stopped
before deletion when it was started from Emacs."
  (interactive)
  (let* ((default-dir (tatami1--default-data-directory))
         (target (expand-file-name (or directory
                                       (if current-prefix-arg
                                           (read-directory-name
                                            "Delete data directory: "
                                            default-dir nil t)
                                         default-dir)))))
    (if (file-directory-p target)
        (when (yes-or-no-p (format "Really delete data directory %s? " target))
          (tatami1-stop-server)
          (delete-directory target t)
          (make-directory target t)
          (message "Reset headless data directory %s" target))
      (if (called-interactively-p 'interactive)
          (message "No data directory at %s" target)
        (tatami1--note-error "No data directory at %s" target)))))

(defun tatami1--process-sentinel (proc event)
  "Internal sentinel to report when PROC terminates unexpectedly.
EVENT is the raw event string from `set-process-sentinel'."
  (let ((trimmed (string-trim (or event ""))))
    (unless (or (not (process-live-p proc))
                (string-empty-p trimmed)
                (string-match-p "finished" trimmed))
      (tatami1--note-error "Server process %s" trimmed)
      (message "Tatami1 server process stopped: %s" trimmed))))

(defun tatami1-start-server ()
  "Start the headless server if it is not already running."
  (interactive)
  (setq tatami1--last-command tatami1-start-command)
  (tatami1--launch-server t))

(defun tatami1--launch-server (&optional interactive)
  "Ensure a server is running, launching it when necessary.
INTERACTIVE controls messaging."
  (if (tatami1--server-running-p)
      (progn
        (when interactive (message "Headless server already running"))
        t)
    (let* ((default-directory (or tatami1-start-directory default-directory))
           (command tatami1--last-command)
           (process-name "headless-api-server"))
      (unless (and (listp command) (car command))
        (error "`tatami1-start-command' must be a non-empty list"))
      (when interactive
        (message "Starting headless server using %s" (string-join command " ")))
      (setq tatami1--server-process
            (apply #'start-process process-name (get-buffer-create "*headless-api-server*") command))
      (set-process-query-on-exit-flag tatami1--server-process nil)
      (set-process-sentinel tatami1--server-process #'tatami1--process-sentinel)
      (let ((elapsed 0)
            (interval 0.5)
            (max-wait tatami1-startup-wait)
            (ready nil))
        (while (and (< elapsed max-wait)
                    (not (setq ready (tatami1--server-running-p))))
          (sleep-for interval)
          (setq elapsed (+ elapsed interval)))
        (when (and (not ready) interactive)
          (message "Failed to contact headless server%s"
                   (if tatami1--last-error
                       (format ": %s" tatami1--last-error)
                     "")))
        ready))))

(defun tatami1--ensure-server (&optional interactive)
  (or (tatami1--server-running-p)
      (tatami1--launch-server interactive)))

(defun tatami1-me-summary (&optional profile)
  "Retrieve the current :me summary as plain text.
When PROFILE (string) is provided, use it instead of `tatami1-profile'."
  (interactive (list (when current-prefix-arg
                       (read-string "Profile: " tatami1-profile))))
  (let ((tatami1-profile (or profile tatami1-profile)))
    (unless (tatami1--ensure-server (called-interactively-p 'interactive))
      (error "Headless server is not reachable%s"
             (if tatami1--last-error
                 (format ": %s" tatami1--last-error)
               "")))
    (let* ((summary (tatami1--request "GET" "/api/%CE%B1/me/summary" nil nil))
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

(defun tatami1-focus-header (&optional profile)
  "Fetch the current focus header as an alist.
When PROFILE is provided, override `tatami1-profile'."
  (interactive (list (when current-prefix-arg
                       (read-string "Profile: " tatami1-profile))))
  (let ((tatami1-profile (or profile tatami1-profile)))
    (unless (tatami1--ensure-server (called-interactively-p 'interactive))
      (error "Headless server is not reachable%s"
             (if tatami1--last-error
                 (format ": %s" tatami1--last-error)
               "")))
    (let ((fh (tatami1--request "GET" "/api/%CE%B1/focus-header" nil t)))
      (when (called-interactively-p 'interactive)
        (message "Focus header retrieved for profile %s" (or tatami1-profile "default")))
      fh)))

(defun tatami1-send-turn (text &optional profile)
  "Post TEXT to the /api/α/turns endpoint and return the updated focus header.
When PROFILE is provided, override `tatami1-profile'."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Turn text: "))
    (when current-prefix-arg
      (read-string "Profile: " tatami1-profile))))
  (let ((tatami1-profile (or profile tatami1-profile)))
    (unless (tatami1--ensure-server (called-interactively-p 'interactive))
      (error "Headless server is not reachable%s"
             (if tatami1--last-error
                 (format ": %s" tatami1--last-error)
               "")))
    (let ((resp (tatami1--request "POST" "/api/%CE%B1/turns" `((text . ,text)) t)))
      (when (called-interactively-p 'interactive)
        (message "Turn submitted; %d entities returned" (length (alist-get 'entities resp))))
      (alist-get 'focus_header resp))))

(defun tatami1-send-sentences (text &optional profile)
  "Send each sentence from TEXT as an individual turn.
When PROFILE is provided, override `tatami1-profile'.  Returns the focus header
from the final turn posted."
  (interactive
    (list
     (if (use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end))
       (read-string "Text: "))
    (when current-prefix-arg
      (read-string "Profile: " tatami1-profile))))
  (let* ((tatami1-profile (or profile tatami1-profile))
         (sentences (tatami1--collect-sentences text))
         (last-header nil)
         (count 0)
         (start-time (current-time)))
    (unless sentences
      (user-error "No sentences to send"))
    (dolist (sentence sentences last-header)
      (setq last-header (tatami1-send-turn sentence tatami1-profile))
      (setq count (1+ count)))
    (message "Tatami1: sent %d sentence%s in %.2fs"
             count
             (if (= count 1) "" "s")
             (float-time (time-subtract (current-time) start-time)))))

(defalias 'tatami1-send-lines #'tatami1-send-sentences)

(defun tatami1-send-buffer-sentences (&optional profile)
  "Send every sentence in the current buffer sequentially to the API.
When PROFILE is provided, override `tatami1-profile'.  Blank sentences are
ignored; the focus header from the final turn is returned."
  (interactive (list (when current-prefix-arg
                       (read-string "Profile: " tatami1-profile))))
  (let ((tatami1-profile (or profile tatami1-profile))
        (last-header nil)
        (count 0)
        (start-time (current-time)))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((bounds (tatami1--next-sentence t)))
          (if (not bounds)
              (goto-char (point-max))
            (let* ((start (car bounds))
                   (end (cdr bounds))
                   (sentence (string-trim (buffer-substring-no-properties start end))))
              (goto-char end)
              (unless (string-empty-p sentence)
                (setq last-header (tatami1-send-turn sentence tatami1-profile))
                (setq count (1+ count)))))))
    (when (> count 0)
      (message "Tatami1: sent %d sentence%s from buffer in %.2fs"
               count
               (if (= count 1) "" "s")
               (float-time (time-subtract (current-time) start-time))))
    last-header)))

(defun tatami1-send-next-sentence (&optional profile)
  "Send the sentence at point to the API and advance point.
When PROFILE is provided, override `tatami1-profile'."
  (interactive (list (when current-prefix-arg
                       (read-string "Profile: " tatami1-profile))))
  (let ((tatami1-profile (or profile tatami1-profile))
        bounds sentence)
    (while (and (setq bounds (tatami1--next-sentence t))
                (string-empty-p (setq sentence (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds)))))))
    (unless bounds
      (user-error "No sentence at point"))
    (goto-char (cdr bounds))
    (tatami1-send-turn sentence tatami1-profile)))

;;;###autoload
(defun tatami1-open-summary-buffer ()
  "Convenience command to show the :me summary in a read-only buffer."
  (interactive)
  (tatami1-me-summary))

(provide 'tatami1)

;;; tatami1.el ends here
